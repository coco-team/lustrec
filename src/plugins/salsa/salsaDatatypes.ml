module LT = Lustre_types
module MT = Machine_code_types
module MC = Machine_code_common
module ST = Salsa.Types
module Float = Salsa.Float

let debug = ref false

let pp_hash ~sep f fmt r = 
  Format.fprintf fmt "[@[<v>";
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%t%s@ " (f k v) sep) r;
  Format.fprintf fmt "]@]";


module Ranges = 
  functor (Value: sig type t val union: t -> t -> t val pp: Format.formatter -> t -> unit end)  ->
struct
  type t = Value.t
  type r_t = (LT.ident, Value.t) Hashtbl.t

  let empty: r_t = Hashtbl.create 13

  (* Look for def of node i with inputs living in vtl_ranges, reinforce ranges
     to bound vdl: each output of node i *)
  let add_call ranges vdl id vtl_ranges = ranges (* TODO assert false.  On est
  						    pas obligé de faire
  						    qqchose. On peut supposer
  						    que les ranges sont donnés
  						    pour chaque noeud *)


  let pp = pp_hash ~sep:";" (fun k v fmt -> Format.fprintf fmt "%s -> %a" k Value.pp v) 
  let pp_val = Value.pp

  let add_def ranges name r = 
    (* Format.eprintf "%s: declare %a@."  *)
    (* 	  x.LT.var_id *)
    (* 	  Value.pp r ; *)
	
    let fresh = Hashtbl.copy ranges in
    Hashtbl.add fresh name r; fresh

  let enlarge ranges name r =
    let fresh = Hashtbl.copy ranges in
    if Hashtbl.mem fresh name then
      Hashtbl.replace fresh name (Value.union r (Hashtbl.find fresh name))
    else
      Hashtbl.add fresh name r; 
    fresh
    

  (* Compute a join per variable *)  
  let merge ranges1 ranges2 = 
    (* Format.eprintf "Mergeing rangesint %a with %a@." pp ranges1 pp ranges2; *)
    let ranges = Hashtbl.copy ranges1 in
    Hashtbl.iter (fun k v -> 
      if Hashtbl.mem ranges k then (
	(* Format.eprintf "%s: %a union %a = %a@."  *)
	(*   k *)
	(*   Value.pp v  *)
	(*   Value.pp (Hashtbl.find ranges k) *)
	(*   Value.pp (Value.union v (Hashtbl.find ranges k)); *)
      Hashtbl.replace ranges k (Value.union v (Hashtbl.find ranges k))
    )
      else
	 Hashtbl.add ranges k v
    ) ranges2;
    (* Format.eprintf "Merge result %a@." pp ranges; *)
    ranges

end

module FloatIntSalsa = 
struct
  type t = ST.abstractValue

  let pp fmt (f,r) =
    let fs, rs = (Salsa.Float.Domain.print (f,r)) in
    Format.fprintf fmt "%s + %s" fs rs 
(*    match f, r with
    | ST.I(a,b), ST.J(c,d) ->
      Format.fprintf fmt "[%f, %f] + [%s, %s]" a b (Num.string_of_num c) (Num.string_of_num d)
    | ST.I(a,b), ST.JInfty ->  Format.fprintf fmt "[%f, %f] + oo" a b 
    | ST.Empty, _ -> Format.fprintf fmt "???"

    | _ -> assert false
*)
  let union v1 v2 = Salsa.Float.Domain.join v1 v2
(*    match v1, v2 with
    |(ST.I(x1, x2), ST.J(y1, y2)), (ST.I(x1', x2'), ST.J(y1', y2')) ->
      ST.(I(min x1 x1', max x2 x2'), J(min y1 y1', max y2 y2'))
    | _ -> Format.eprintf "%a cup %a failed@.@?" pp v1 pp v2; assert false 
*)
  let inject cst = match cst with (* ATTENTION ATTENTION !!!!! Remettre les Num !!!! *) 
    | LT.Const_int(i)  -> Salsa.Builder.mk_cst (ST.R(float_of_int i (*Num.num_of_int i*), []), ST.R(float_of_int i (*Num.num_of_int i*), []))
    | LT.Const_real (c,e,s) -> (* TODO: this is incorrect. We should rather
				  compute the error associated to the float *)
       
       let r = float_of_string s  in
       let r = Salsa.Prelude.r_of_f_aux r in
       Salsa.Builder.mk_cst (Float.Domain.nnew r r)
	 
      (* let r = float_of_string s  in *)
      (* if r = 0. then *)
      (* 	Salsa.Builder.mk_cst (ST.R(-. min_float, min_float),Float.ulp (ST.R(-. min_float, min_float))) *)
      (* else *)
      (* 	Salsa.Builder.mk_cst (ST.I(r*.(1.-.epsilon_float),r*.(1.+.epsilon_float)),Float.ulp (ST.I(r,r))) *)
    | _ -> assert false
end

module RangesInt = Ranges (FloatIntSalsa)

module Vars = 
struct
  module VarSet = Set.Make (struct type t = LT.var_decl let compare x y = compare x.LT.var_id y.LT.var_id end)
  let real_vars vs = VarSet.filter (fun v -> Types.is_real_type v.LT.var_type) vs
  let of_list = List.fold_left (fun s e -> VarSet.add e s) VarSet.empty 

  include VarSet 

  let remove_list (set:t) (v_list: elt list) : t = List.fold_right VarSet.remove v_list set
  let pp fmt vs = Utils.fprintf_list ~sep:", " Printers.pp_var fmt (VarSet.elements vs)
end










(*************************************************************************************)
(*                 Converting values back and forth                                  *)
(*************************************************************************************)

let rec value_t2salsa_expr constEnv vt = 
  let value_t2salsa_expr = value_t2salsa_expr constEnv in
  let res = 
    match vt.MT.value_desc with
    (* | LT.Cst(LT.Const_tag(t) as c)   ->  *)
    (*   Format.eprintf "v2s: cst tag@."; *)
    (*   if List.mem_assoc t constEnv then ( *)
    (* 	Format.eprintf "trouvé la constante %s: %a@ " t Printers.pp_const c; *)
    (* 	FloatIntSalsa.inject (List.assoc t constEnv) *)
    (*   ) *)
    (*   else (     *)
    (* 	Format.eprintf "Const tag %s unhandled@.@?" t ; *)
    (* 	raise (Salsa.Prelude.Error ("Entschuldigung6, constant tag not yet implemented")) *)
    (*   ) *)
    | MT.Cst(cst)                ->        (* Format.eprintf "v2s: cst tag 2: %a@." Printers.pp_const cst;  *)FloatIntSalsa.inject cst
    | MT.LocalVar(v)            
    | MT.StateVar(v)            ->       (* Format.eprintf "v2s: var %s@." v.LT.var_id; *) 
      let sel_fun = (fun (vname, _) -> v.LT.var_id = vname) in
      if List.exists sel_fun  constEnv then
	let _, cst = List.find sel_fun constEnv in
	FloatIntSalsa.inject cst
      else
	let id = v.LT.var_id in
				   Salsa.Builder.mk_id id
    | MT.Fun(binop, [x;y])      -> let salsaX = value_t2salsa_expr x in
				   let salsaY = value_t2salsa_expr y in
				   let op = (
				     let pred f x y = Salsa.Builder.mk_int_of_bool (f x y) in
				     match binop with
				     | "+" -> Salsa.Builder.mk_plus
				     | "-" -> Salsa.Builder.mk_minus
				     | "*" -> Salsa.Builder.mk_times
				     | "/" -> Salsa.Builder.mk_div
				     | "=" -> pred Salsa.Builder.mk_eq
				     | "<" -> pred Salsa.Builder.mk_lt
				     | ">" -> pred Salsa.Builder.mk_gt
				     | "<=" -> pred Salsa.Builder.mk_lte
				     | ">=" -> pred Salsa.Builder.mk_gte
				     | _ -> assert false
				   )
				   in
				   op salsaX salsaY 
    | MT.Fun(unop, [x])         -> let salsaX = value_t2salsa_expr x in
				   Salsa.Builder.mk_uminus salsaX

    | MT.Fun(f,_)   -> raise (Salsa.Prelude.Error 
				("Unhandled function "^f^" in conversion to salsa expression"))
    
    | MT.Array(_) 
    | MT.Access(_)
    | MT.Power(_)   -> raise (Salsa.Prelude.Error ("Unhandled construct in conversion to salsa expression"))
  in
  (* if debug then *)
  (*   Format.eprintf "value_t2salsa_expr: %a -> %a@ " *)
  (*     MC.pp_val vt *)
  (*     (fun fmt x -> Format.fprintf fmt "%s" (Salsa.Print.printExpression x)) res; *)
  res

type var_decl = { vdecl: LT.var_decl; is_local: bool }
module VarEnv = Map.Make (struct type t = LT.ident let compare = compare end )

(* let is_local_var vars_env v = *)
(*   try *)
(*   (VarEnv.find v vars_env).is_local *)
(*   with Not_found -> Format.eprintf "Impossible to find var %s@.@?" v; assert false *)

let get_var vars_env v =
try
  VarEnv.find v vars_env
  with Not_found -> Format.eprintf "Impossible to find var %s@.@?" v; assert false

let compute_vars_env m =
  let env = VarEnv.empty in
  let env = 
    List.fold_left 
      (fun accu v -> VarEnv.add v.LT.var_id {vdecl = v; is_local = false; } accu) 
      env 
      m.MT.mmemory
  in
  let env = 
    List.fold_left (
      fun accu v -> VarEnv.add v.LT.var_id {vdecl = v; is_local = true; } accu
    )
      env
      MC.(m.MT.mstep.MT.step_inputs@m.MT.mstep.MT.step_outputs@m.MT.mstep.MT.step_locals)
  in
env

let rec salsa_expr2value_t vars_env cst_env e  = 
  let salsa_expr2value_t = salsa_expr2value_t vars_env cst_env in
  let binop op e1 e2 t = 
    let x = salsa_expr2value_t e1 in
    let y = salsa_expr2value_t e2 in                    
    MC.mk_val (MT.Fun (op, [x;y])) t
  in
  match e with
    ST.Cst((ST.R(c,_),_),_)     -> (* We project ranges into constants. We
					forget about errors and provide the
					mean/middle value of the interval
				     *)
      let  new_float = Salsa.NumMartel.float_of_num c in
      (* let new_float =  *)
      (* 	if f1 = f2 then *)
      (* 	  f1 *)
      (* 	else *)
      (* 	  (f1 +. f2) /. 2.0  *)
      (* in *)
      (* Log.report ~level:3 *)
      (* 	(fun fmt -> Format.fprintf fmt  "projecting [%.45f, %.45f] -> %.45f@ " f1 f2 new_float); *)
      let cst =  
	let s = 
	  if new_float = 0. then "0." else
	    (* We have to convert it into our format: int * int * real *)
	    let _ = Format.flush_str_formatter () in
	    Format.fprintf Format.str_formatter "%.50f" new_float;
	    Format.flush_str_formatter () 
	in
	Parser_lustre.signed_const Lexer_lustre.token (Lexing.from_string s) 
      in
      MC.mk_val (MT.Cst(cst)) Type_predef.type_real
  | ST.Id(id, _)          -> 
    (* Format.eprintf "Looking for id=%s@.@?" id; *)
     if List.mem_assoc id cst_env then (
       let cst = List.assoc id cst_env in
      (* Format.eprintf "Found cst = %a@.@?" Printers.pp_const cst; *)
       MC.mk_val (MT.Cst cst) Type_predef.type_real
     )
     else
      (* if is_const salsa_label then *)
      (*   MC.Cst(LT.Const_tag(get_const salsa_label)) *)
      (* else *) 
       let var_id = try get_var vars_env id with Not_found -> assert false in
       if var_id.is_local then
	 MC.mk_val (MT.LocalVar(var_id.vdecl)) var_id.vdecl.LT.var_type
       else
	 MC.mk_val (MT.StateVar(var_id.vdecl)) var_id.vdecl.LT.var_type
  | ST.Plus(x, y, _)               -> binop "+" x y Type_predef.type_real
  | ST.Minus(x, y, _)              -> binop "-" x y Type_predef.type_real
  | ST.Times(x, y, _)              -> binop "*" x y Type_predef.type_real
  | ST.Div(x, y, _)                -> binop "/" x y Type_predef.type_real
  | ST.Uminus(x,_)                 -> let x = salsa_expr2value_t x in
				      MC.mk_val (MT.Fun("uminus",[x])) Type_predef.type_real
  | ST.IntOfBool(ST.Eq(x, y, _),_) -> binop "=" x y Type_predef.type_bool
  | ST.IntOfBool(ST.Lt(x,y,_),_)   -> binop "<" x y Type_predef.type_bool
  | ST.IntOfBool(ST.Gt(x,y,_),_)   -> binop ">" x y Type_predef.type_bool
  | ST.IntOfBool(ST.Lte(x,y,_),_)  -> binop "<=" x y Type_predef.type_bool
  | ST.IntOfBool(ST.Gte(x,y,_),_)  -> binop ">=" x y Type_predef.type_bool
  | _      -> raise (Salsa.Prelude.Error "Entschuldigung, salsaExpr2value_t case not yet implemented")


let rec get_salsa_free_vars vars_env constEnv absenv e =
  let f = get_salsa_free_vars vars_env constEnv absenv in
  match e with
  | ST.Id (id, _) -> 
    if not (List.mem_assoc id absenv) && not (List.mem_assoc id constEnv) then
      Vars.singleton ((try VarEnv.find id vars_env with Not_found -> assert false).vdecl) 
    else
      Vars.empty
  | ST.Plus(x, y, _)  
  | ST.Minus(x, y, _)
  | ST.Times(x, y, _)
  | ST.Div(x, y, _)
  | ST.IntOfBool(ST.Eq(x, y, _),_) 
  | ST.IntOfBool(ST.Lt(x,y,_),_)   
  | ST.IntOfBool(ST.Gt(x,y,_),_)   
  | ST.IntOfBool(ST.Lte(x,y,_),_)  
  | ST.IntOfBool(ST.Gte(x,y,_),_)  
    -> Vars.union (f x) (f y)
  | ST.Uminus(x,_)    -> f x
  | ST.Cst _ -> Vars.empty
  | _ -> assert false


module FormalEnv =
struct
  type fe_t = (LT.ident, (int * MT.value_t)) Hashtbl.t
  let cpt = ref 0

  exception NoDefinition of LT.var_decl
  (* Returns the expression associated to v in env *)
  let get_def (env: fe_t) v = 
    try 
      snd (Hashtbl.find env v.LT.var_id) 
    with Not_found -> raise (NoDefinition v)

  let fold f = Hashtbl.fold (fun k (_,v) accu -> f k v accu)
      
  let to_salsa constEnv formalEnv = 
    fold (fun id expr accu ->
      (id, value_t2salsa_expr constEnv expr)::accu
    ) formalEnv [] 

  let def constEnv vars_env (env: fe_t) d expr = 
    incr cpt;
    let fresh = Hashtbl.copy env in
    let expr_salsa = value_t2salsa_expr constEnv expr in
    let salsa_env = to_salsa constEnv env in
    let expr_salsa, _ = Salsa.Rewrite.substVars expr_salsa salsa_env 0 in
    let expr_salsa = Salsa.Analyzer.evalPartExpr expr_salsa salsa_env ([] (* no blacklisted vars *)) ([] (*no arrays *)) in
    let expr_lustrec = salsa_expr2value_t vars_env [] expr_salsa in
    Hashtbl.add fresh d.LT.var_id (!cpt, expr_lustrec); fresh

  let empty (): fe_t = Hashtbl.create 13

  let pp fmt env = pp_hash ~sep:";" (fun k (_,v) fmt -> Format.fprintf fmt "%s -> %a" k MC.pp_val v) fmt env


  let get_sort_fun env =
    let order = Hashtbl.fold (fun k (cpt, _) accu -> (k,cpt)::accu) env [] in
    fun v1 v2 -> 
      if List.mem_assoc v1.LT.var_id order && List.mem_assoc v2.LT.var_id order then
	if (List.assoc v1.LT.var_id order) <= (List.assoc v2.LT.var_id order) then 
	  -1
	else 
	  1
      else
	assert false
end

     
(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
