(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

open Utils
open Lustre_types 
open Machine_code_types
open Corelang
open Causality
open Machine_code_common
open Dimension


let pp_elim fmt elim =
  begin
    Format.fprintf fmt "@[{ /* elim table: */@ ";
    IMap.iter (fun v expr -> Format.fprintf fmt "%s |-> %a@ " v pp_val expr) elim;
    Format.fprintf fmt "}@ @]";
  end

let rec eliminate elim instr =
  let e_expr = eliminate_expr elim in
  match get_instr_desc instr with
  | MComment _         -> instr
  | MLocalAssign (i,v) -> update_instr_desc instr (MLocalAssign (i, e_expr v))
  | MStateAssign (i,v) -> update_instr_desc instr (MStateAssign (i, e_expr v))
  | MReset i           -> instr
  | MNoReset i         -> instr
  | MStep (il, i, vl)  -> update_instr_desc instr (MStep(il, i, List.map e_expr vl))
  | MBranch (g,hl)     -> 
     update_instr_desc instr (
       MBranch
	 (e_expr g, 
	  (List.map 
	     (fun (l, il) -> l, List.map (eliminate elim) il) 
	     hl
	  )
	 )
     )
    
and eliminate_expr elim expr =
  match expr.value_desc with
  | LocalVar v -> (try IMap.find v.var_id elim with Not_found -> expr)
  | Fun (id, vl) -> {expr with value_desc = Fun (id, List.map (eliminate_expr elim) vl)}
  | Array(vl) -> {expr with value_desc = Array(List.map (eliminate_expr elim) vl)}
  | Access(v1, v2) -> { expr with value_desc = Access(eliminate_expr elim v1, eliminate_expr elim v2)}
  | Power(v1, v2) -> { expr with value_desc = Power(eliminate_expr elim v1, eliminate_expr elim v2)}
  | Cst _ | StateVar _ -> expr

let eliminate_dim elim dim =
  Dimension.expr_replace_expr 
    (fun v -> try
		dimension_of_value (IMap.find v elim) 
      with Not_found -> mkdim_ident dim.dim_loc v) 
    dim


(* 8th Jan 2016: issues when merging salsa with horn_encoding: The following
   functions seem unsused. They have to be adapted to the new type for expr
*)

let unfold_expr_offset m offset expr =
  List.fold_left
    (fun res -> (function | Index i -> mk_val (Access (res, value_of_dimension m i))
					      (Types.array_element_type res.value_type)
                          | Field f -> Format.eprintf "internal error: not yet implemented !"; assert false))
    expr offset

let rec simplify_cst_expr m offset typ cst =
    match offset, cst with
    | []          , _
      -> mk_val (Cst cst) typ
    | Index i :: q, Const_array cl when Dimension.is_dimension_const i
      -> let elt_typ = Types.array_element_type typ in
         simplify_cst_expr m q elt_typ (List.nth cl (Dimension.size_const_dimension i))
    | Index i :: q, Const_array cl
      -> let elt_typ = Types.array_element_type typ in
         unfold_expr_offset m [Index i] (mk_val (Array (List.map (simplify_cst_expr m q elt_typ) cl)) typ)
    | Field f :: q, Const_struct fl
      -> let fld_typ = Types.struct_field_type typ f in
         simplify_cst_expr m q fld_typ (List.assoc f fl)
    | _ -> (Format.eprintf "internal error: Optimize_machine.simplify_cst_expr %a@." Printers.pp_const cst; assert false)

let simplify_expr_offset m expr =
  let rec simplify offset expr =
    match offset, expr.value_desc with
    | Field f ::q , _                -> failwith "not yet implemented"
    | _           , Fun (id, vl) when Basic_library.is_value_internal_fun expr
                                     -> mk_val (Fun (id, List.map (simplify offset) vl)) expr.value_type
    | _           , Fun _
    | _           , StateVar _
    | _           , LocalVar _       -> unfold_expr_offset m offset expr
    | _           , Cst cst          -> simplify_cst_expr m offset expr.value_type cst
    | _           , Access (expr, i) -> simplify (Index (dimension_of_value i) :: offset) expr
    | []          , _                -> expr
    | Index _ :: q, Power (expr, _)  -> simplify q expr
    | Index i :: q, Array vl when Dimension.is_dimension_const i
                                     -> simplify q (List.nth vl (Dimension.size_const_dimension i))
    | Index i :: q, Array vl         -> unfold_expr_offset m [Index i] (mk_val (Array (List.map (simplify q) vl)) expr.value_type)
    (*Format.eprintf "simplify_expr %a %a = %a@." pp_val expr (Utils.fprintf_list ~sep:"" Printers.pp_offset) offset pp_val res; res)
     with e -> (Format.eprintf "simplify_expr %a %a = <FAIL>@." pp_val expr (Utils.fprintf_list ~sep:"" Printers.pp_offset) offset; raise e*)
  in simplify [] expr

let rec simplify_instr_offset m instr =
  match get_instr_desc instr with
  | MLocalAssign (v, expr) -> update_instr_desc instr (MLocalAssign (v, simplify_expr_offset m expr))
  | MStateAssign (v, expr) -> update_instr_desc instr (MStateAssign (v, simplify_expr_offset m expr))
  | MReset id              -> instr
  | MNoReset id            -> instr
  | MStep (outputs, id, inputs) -> update_instr_desc instr (MStep (outputs, id, List.map (simplify_expr_offset m) inputs))
  | MBranch (cond, brl)
    -> update_instr_desc instr (
      MBranch(simplify_expr_offset m cond, List.map (fun (l, il) -> l, simplify_instrs_offset m il) brl)
    )
  | MComment _             -> instr

and simplify_instrs_offset m instrs =
  List.map (simplify_instr_offset m) instrs

let is_scalar_const c =
  match c with
  | Const_real _
  | Const_int _
  | Const_tag _   -> true
  | _             -> false

(* An instruction v = expr may (and will) be unfolded iff:
   - either expr is atomic
     (no complex expressions, only const, vars and array/struct accesses)
   - or v has a fanin <= 1 (used at most once)
*)
let is_unfoldable_expr fanin expr =
  let rec unfold_const offset cst =
    match offset, cst with
    | _           , Const_int _
    | _           , Const_real _
    | _           , Const_tag _     -> true
    | Field f :: q, Const_struct fl -> unfold_const q (List.assoc f fl)
    | []          , Const_struct _  -> false
    | Index i :: q, Const_array cl when Dimension.is_dimension_const i
                                    -> unfold_const q (List.nth cl (Dimension.size_const_dimension i))
    | _           , Const_array _   -> false
    | _                             -> assert false in
  let rec unfold offset expr =
    match offset, expr.value_desc with
    | _           , Cst cst                      -> unfold_const offset cst
    | _           , LocalVar _
    | _           , StateVar _                   -> true
    | []          , Power _
    | []          , Array _                      -> false
    | Index i :: q, Power (v, _)                 -> unfold q v
    | Index i :: q, Array vl when Dimension.is_dimension_const i
                                                 -> unfold q (List.nth vl (Dimension.size_const_dimension i))
    | _           , Array _                      -> false
    | _           , Access (v, i)                -> unfold (Index (dimension_of_value i) :: offset) v
    | _           , Fun (id, vl) when fanin < 2 && Basic_library.is_value_internal_fun expr
                                                 -> List.for_all (unfold offset) vl
    | _           , Fun _                        -> false
    | _                                          -> assert false
  in unfold [] expr

let basic_unfoldable_assign fanin v expr =
  try
    let d = Hashtbl.find fanin v.var_id
    in is_unfoldable_expr d expr
  with Not_found -> false

let unfoldable_assign fanin v expr =
   (if !Options.mpfr then Mpfr.unfoldable_value expr else true)
&& basic_unfoldable_assign fanin v expr

let merge_elim elim1 elim2 =
  let merge k e1 e2 =
    match e1, e2 with
    | Some e1, Some e2 -> if e1 = e2 then Some e1 else None
    | _      , Some e2 -> Some e2
    | Some e1, _       -> Some e1
    | _                -> None
  in IMap.merge merge elim1 elim2

(* see if elim has to take in account the provided instr:
   if so, update elim and return the remove flag,
   otherwise, the expression should be kept and elim is left untouched *)
let rec instrs_unfold fanin elim instrs =
  let elim, rev_instrs = 
    List.fold_left (fun (elim, instrs) instr ->
      (* each subexpression in instr that could be rewritten by the elim set is
	 rewritten *)
      let instr = eliminate elim instr in
      (* if instr is a simple local assign, then (a) elim is simplified with it (b) it
	 is stored as the elim set *)
      instr_unfold fanin instrs elim instr
    ) (elim, []) instrs
  in elim, List.rev rev_instrs

and instr_unfold fanin instrs elim instr =
(*  Format.eprintf "SHOULD WE STORE THE EXPRESSION IN INSTR %a TO ELIMINATE IT@." pp_instr instr;*)
  match get_instr_desc instr with
  (* Simple cases*)
  | MStep([v], id, vl) when Basic_library.is_value_internal_fun (mk_val (Fun (id, vl)) v.var_type)
    -> instr_unfold fanin instrs elim (update_instr_desc instr (MLocalAssign (v, mk_val (Fun (id, vl)) v.var_type)))
  | MLocalAssign(v, expr) when unfoldable_assign fanin v expr
    -> (IMap.add v.var_id expr elim, instrs)
  | MBranch(g, hl) when false
    -> let elim_branches = List.map (fun (h, l) -> (h, instrs_unfold fanin elim l)) hl in
       let (elim, branches) =
	 List.fold_right
	   (fun (h, (e, l)) (elim, branches) -> (merge_elim elim e, (h, l)::branches))
	   elim_branches (elim, [])
       in elim, ((update_instr_desc instr (MBranch (g, branches))) :: instrs)
  | _
    -> (elim, instr :: instrs)
    (* default case, we keep the instruction and do not modify elim *)
  

(** We iterate in the order, recording simple local assigns in an accumulator
    1. each expression is rewritten according to the accumulator
    2. local assigns then rewrite occurrences of the lhs in the computed accumulator
*)

let static_call_unfold elim (inst, (n, args)) =
  let replace v =
    try
      dimension_of_value (IMap.find v elim)
    with Not_found -> Dimension.mkdim_ident Location.dummy_loc v
  in (inst, (n, List.map (Dimension.expr_replace_expr replace) args))

(** Perform optimization on machine code:
    - iterate through step instructions and remove simple local assigns
    
*)
let machine_unfold fanin elim machine =
  (*Log.report ~level:1 (fun fmt -> Format.fprintf fmt "machine_unfold %a@." pp_elim elim);*)
  let elim_consts, mconst = instrs_unfold fanin elim machine.mconst in
  let elim_vars, instrs = instrs_unfold fanin elim_consts machine.mstep.step_instrs in
  let instrs = simplify_instrs_offset machine instrs in
  let checks = List.map (fun (loc, check) -> loc, eliminate_expr elim_vars check) machine.mstep.step_checks in
  let locals = List.filter (fun v -> not (IMap.mem v.var_id elim_vars)) machine.mstep.step_locals in
  let minstances = List.map (static_call_unfold elim_consts) machine.minstances in
  let mcalls = List.map (static_call_unfold elim_consts) machine.mcalls
  in
  {
    machine with
      mstep = { 
	machine.mstep with 
	  step_locals = locals;
	  step_instrs = instrs;
	  step_checks = checks
      };
      mconst = mconst;
      minstances = minstances;
      mcalls = mcalls;
  },
  elim_vars

let instr_of_const top_const =
  let const = const_of_top top_const in
  let vdecl = mkvar_decl Location.dummy_loc (const.const_id, mktyp Location.dummy_loc Tydec_any, mkclock Location.dummy_loc Ckdec_any, true, None, None) in
  let vdecl = { vdecl with var_type = const.const_type }
  in mkinstr (MLocalAssign (vdecl, mk_val (Cst const.const_value) vdecl.var_type))

let machines_unfold consts node_schs machines =
  List.fold_right (fun m (machines, removed) ->
    let fanin = (IMap.find m.mname.node_id node_schs).Scheduling.fanin_table in
    let elim_consts, _ = instrs_unfold fanin IMap.empty (List.map instr_of_const consts) in
    let (m, removed_m) =  machine_unfold fanin elim_consts m in
    (m::machines, IMap.add m.mname.node_id removed_m removed)
    )
    machines
    ([], IMap.empty)

let get_assign_lhs instr =
  match get_instr_desc instr with
  | MLocalAssign(v, e) -> mk_val (LocalVar v) e.value_type
  | MStateAssign(v, e) -> mk_val (StateVar v) e.value_type
  | _                  -> assert false

let get_assign_rhs instr =
  match get_instr_desc instr with
  | MLocalAssign(_, e)
  | MStateAssign(_, e) -> e
  | _                  -> assert false

let is_assign instr =
  match get_instr_desc instr with
  | MLocalAssign _
  | MStateAssign _ -> true
  | _              -> false

let mk_assign v e =
 match v.value_desc with
 | LocalVar v -> MLocalAssign(v, e)
 | StateVar v -> MStateAssign(v, e)
 | _          -> assert false

let rec assigns_instr instr assign =
  match get_instr_desc instr with  
  | MLocalAssign (i,_)
  | MStateAssign (i,_) -> VSet.add i assign
  | MStep (ol, _, _)   -> List.fold_right VSet.add ol assign
  | MBranch (_,hl)     -> List.fold_right (fun (_, il) -> assigns_instrs il) hl assign
  | _                  -> assign

and assigns_instrs instrs assign =
  List.fold_left (fun assign instr -> assigns_instr instr assign) assign instrs

(*    
and substitute_expr subst expr =
  match expr with
  | StateVar v
  | LocalVar v -> (try IMap.find expr subst with Not_found -> expr)
  | Fun (id, vl) -> Fun (id, List.map (substitute_expr subst) vl)
  | Array(vl) -> Array(List.map (substitute_expr subst) vl)
  | Access(v1, v2) -> Access(substitute_expr subst v1, substitute_expr subst v2)
  | Power(v1, v2) -> Power(substitute_expr subst v1, substitute_expr subst v2)
  | Cst _  -> expr
*)
(** Finds a substitute for [instr] in [instrs], 
   i.e. another instr' with the same rhs expression.
   Then substitute this expression with the first assigned var
*)
let subst_instr subst instrs instr =
  (*Format.eprintf "subst instr: %a@." Machine_code.pp_instr instr;*)
  let instr = eliminate subst instr in
  let v = get_assign_lhs instr in
  let e = get_assign_rhs instr in
  (* Difficulties to merge with unstable. Here is the other code:

try
    let instr' = List.find (fun instr' -> is_assign instr' && get_assign_rhs instr' = e) instrs in
    match v.value_desc with
    | LocalVar v ->
      IMap.add v.var_id (get_assign_lhs instr') subst, instrs
    | StateVar v ->
      let lhs' = get_assign_lhs instr' in
      let typ' = lhs'.value_type in
      (match lhs'.value_desc with
      | LocalVar v' ->
	let instr = eliminate subst (mk_assign (mk_val (StateVar v) typ') (mk_val (LocalVar v') typ')) in
	subst, instr :: instrs
      | StateVar v' ->
	let subst_v' = IMap.add v'.var_id (mk_val (StateVar v) typ') IMap.empty in
let instrs' = snd (List.fold_right (fun instr (ok, instrs) -> (ok || instr = instr', if ok then instr :: instrs else if instr = instr' then instrs else eliminate subst_v' instr :: instrs)) instrs (false, [])) in
	IMap.add v'.var_id (mk_val (StateVar v) typ') subst, instr :: instrs'
      | _           -> assert false)
    | _          -> assert false
  with Not_found -> subst, instr :: instrs
 
*)

try
    let instr' = List.find (fun instr' -> is_assign instr' && get_assign_rhs instr' = e) instrs in
    match v.value_desc with
    | LocalVar v ->
      IMap.add v.var_id (get_assign_lhs instr') subst, instrs
    | StateVar stv ->
       let lhs = get_assign_lhs instr' in
      (match lhs.value_desc with
      | LocalVar v' ->
        let instr = eliminate subst (update_instr_desc instr (mk_assign v lhs)) in
	subst, instr :: instrs
      | StateVar stv' ->
	let subst_v' = IMap.add stv'.var_id v IMap.empty in
	let instrs' = snd (List.fold_right (fun instr (ok, instrs) -> (ok || instr = instr', if ok then instr :: instrs else if instr = instr' then instrs else eliminate subst_v' instr :: instrs)) instrs (false, [])) in
	IMap.add stv'.var_id v subst, instr :: instrs'
      | _           -> assert false)
    | _          -> assert false
  with Not_found -> subst, instr :: instrs
 
(** Common sub-expression elimination for machine instructions *)
(* - [subst] : hashtable from ident to (simple) definition
               it is an equivalence table
   - [elim]   : set of eliminated variables
   - [instrs] : previous instructions, which [instr] is compared against
   - [instr] : current instruction, normalized by [subst]
*)
let rec instr_cse (subst, instrs) instr =
  match get_instr_desc instr with
  (* Simple cases*)
  | MStep([v], id, vl) when Basic_library.is_internal_fun id (List.map (fun v -> v.value_type) vl)
      -> instr_cse (subst, instrs) (update_instr_desc instr (MLocalAssign (v, mk_val (Fun (id, vl)) v.var_type)))
  | MLocalAssign(v, expr) when is_unfoldable_expr 2 expr
      -> (IMap.add v.var_id expr subst, instr :: instrs)
  | _ when is_assign instr
      -> subst_instr subst instrs instr
  | _ -> (subst, instr :: instrs)

(** Apply common sub-expression elimination to a sequence of instrs
*)
let rec instrs_cse subst instrs =
  let subst, rev_instrs = 
    List.fold_left instr_cse (subst, []) instrs
  in subst, List.rev rev_instrs

(** Apply common sub-expression elimination to a machine
    - iterate through step instructions and remove simple local assigns
*)
let machine_cse subst machine =
  (*Log.report ~level:1 (fun fmt -> Format.fprintf fmt "machine_cse %a@." pp_elim subst);*)
  let subst, instrs = instrs_cse subst machine.mstep.step_instrs in
  let assigned = assigns_instrs instrs VSet.empty
  in
  {
    machine with
      mmemory = List.filter (fun vdecl -> VSet.mem vdecl assigned) machine.mmemory;
      mstep = { 
	machine.mstep with 
	  step_locals = List.filter (fun vdecl -> VSet.mem vdecl assigned) machine.mstep.step_locals;
	  step_instrs = instrs
      }
  }

let machines_cse machines =
  List.map
    (machine_cse IMap.empty)
    machines

(* variable substitution for optimizing purposes *)

(* checks whether an [instr] is skip and can be removed from program *)
let rec instr_is_skip instr =
  match get_instr_desc instr with
  | MLocalAssign (i, { value_desc = (LocalVar v) ; _}) when i = v -> true
  | MStateAssign (i, { value_desc = StateVar v; _}) when i = v -> true
  | MBranch (g, hl) -> List.for_all (fun (_, il) -> instrs_are_skip il) hl
  | _               -> false
and instrs_are_skip instrs =
  List.for_all instr_is_skip instrs

let instr_cons instr cont =
 if instr_is_skip instr then cont else instr::cont

let rec instr_remove_skip instr cont =
  match get_instr_desc instr with
  | MLocalAssign (i, { value_desc = LocalVar v; _ }) when i = v -> cont
  | MStateAssign (i, { value_desc = StateVar v; _ }) when i = v -> cont
  | MBranch (g, hl) -> update_instr_desc instr (MBranch (g, List.map (fun (h, il) -> (h, instrs_remove_skip il [])) hl)) :: cont
  | _               -> instr::cont

and instrs_remove_skip instrs cont =
  List.fold_right instr_remove_skip instrs cont

let rec value_replace_var fvar value =
  match value.value_desc with
  | Cst c -> value
  | LocalVar v -> { value with value_desc = LocalVar (fvar v) }
  | StateVar v -> value
  | Fun (id, args) -> { value with value_desc = Fun (id, List.map (value_replace_var fvar) args) }
  | Array vl -> { value with value_desc = Array (List.map (value_replace_var fvar) vl)}
  | Access (t, i) -> { value with value_desc = Access(value_replace_var fvar t, i)}
  | Power (v, n) -> { value with value_desc = Power(value_replace_var fvar v, n)}

let rec instr_replace_var fvar instr cont =
  match get_instr_desc instr with
  | MComment _          -> instr_cons instr cont
  | MLocalAssign (i, v) -> instr_cons (update_instr_desc instr (MLocalAssign (fvar i, value_replace_var fvar v))) cont
  | MStateAssign (i, v) -> instr_cons (update_instr_desc instr (MStateAssign (i, value_replace_var fvar v))) cont
  | MReset i            -> instr_cons instr cont
  | MNoReset i          -> instr_cons instr cont
  | MStep (il, i, vl)   -> instr_cons (update_instr_desc instr (MStep (List.map fvar il, i, List.map (value_replace_var fvar) vl))) cont
  | MBranch (g, hl)     -> instr_cons (update_instr_desc instr (MBranch (value_replace_var fvar g, List.map (fun (h, il) -> (h, instrs_replace_var fvar il [])) hl))) cont

and instrs_replace_var fvar instrs cont =
  List.fold_right (instr_replace_var fvar) instrs cont

let step_replace_var fvar step =
  (* Some outputs may have been replaced by locals.
     We then need to rename those outputs
     without changing their clocks, etc *)
  let outputs' =
    List.map (fun o -> { o with var_id = (fvar o).var_id }) step.step_outputs in
  let locals'  =
    List.fold_left (fun res l ->
      let l' = fvar l in
      if List.exists (fun o -> o.var_id = l'.var_id) outputs'
      then res
      else Utils.add_cons l' res)
      [] step.step_locals in
  { step with
    step_checks = List.map (fun (l, v) -> (l, value_replace_var fvar v)) step.step_checks;
    step_outputs = outputs';
    step_locals = locals';
    step_instrs = instrs_replace_var fvar step.step_instrs [];
}

let rec machine_replace_variables fvar m =
  { m with
    mstep = step_replace_var fvar m.mstep
  }

let machine_reuse_variables m reuse =
  let fvar v =
    try
      Hashtbl.find reuse v.var_id
    with Not_found -> v in
  machine_replace_variables fvar m

let machines_reuse_variables prog reuse_tables =
  List.map 
    (fun m -> 
      machine_reuse_variables m (Utils.IMap.find m.mname.node_id reuse_tables)
    ) prog

let rec instr_assign res instr =
  match get_instr_desc instr with
  | MLocalAssign (i, _) -> Disjunction.CISet.add i res
  | MStateAssign (i, _) -> Disjunction.CISet.add i res
  | MBranch (g, hl)     -> List.fold_left (fun res (h, b) -> instrs_assign res b) res hl
  | MStep (il, _, _)    -> List.fold_right Disjunction.CISet.add il res
  | _                   -> res

and instrs_assign res instrs =
  List.fold_left instr_assign res instrs

let rec instr_constant_assign var instr =
  match get_instr_desc instr with
  | MLocalAssign (i, { value_desc = Cst (Const_tag _); _ })
  | MStateAssign (i, { value_desc = Cst (Const_tag _); _ }) -> i = var
  | MBranch (g, hl)                     -> List.for_all (fun (h, b) -> instrs_constant_assign var b) hl
  | _                                   -> false

and instrs_constant_assign var instrs =
  List.fold_left (fun res i -> if Disjunction.CISet.mem var (instr_assign Disjunction.CISet.empty i) then instr_constant_assign var i else res) false instrs

let rec instr_reduce branches instr1 cont =
  match get_instr_desc instr1 with
  | MLocalAssign (_, { value_desc = Cst (Const_tag c); _}) -> instr1 :: (List.assoc c branches @ cont)
  | MStateAssign (_, { value_desc = Cst (Const_tag c); _}) -> instr1 :: (List.assoc c branches @ cont)
  | MBranch (g, hl)                     -> (update_instr_desc instr1 (MBranch (g, List.map (fun (h, b) -> (h, instrs_reduce branches b [])) hl))) :: cont
  | _                                   -> instr1 :: cont

and instrs_reduce branches instrs cont =
 match instrs with
 | []        -> cont
 | [i]       -> instr_reduce branches i cont
 | i1::i2::q -> i1 :: instrs_reduce branches (i2::q) cont

let rec instrs_fusion instrs =
  match instrs, List.map get_instr_desc instrs with
  | [], []
  | [_], [_]                                                               ->
    instrs
  | i1::i2::q, i1_desc::(MBranch ({ value_desc = LocalVar v; _}, hl))::q_desc when instr_constant_assign v i1 ->
    instr_reduce (List.map (fun (h, b) -> h, instrs_fusion b) hl) i1 (instrs_fusion q)
  | i1::i2::q, i1_desc::(MBranch ({ value_desc = StateVar v; _}, hl))::q_desc when instr_constant_assign v i1 ->
    instr_reduce (List.map (fun (h, b) -> h, instrs_fusion b) hl) i1 (instrs_fusion q) 
  | i1::i2::q, _                                                         ->
    i1 :: instrs_fusion (i2::q)
  | _ -> assert false (* Other cases should not happen since both lists are of same size *)
     
let step_fusion step =
  { step with
    step_instrs = instrs_fusion step.step_instrs;
  }

let rec machine_fusion m =
  { m with
    mstep = step_fusion m.mstep
  }

let machines_fusion prog =
  List.map machine_fusion prog


(*** Main function ***)
    
let optimize prog node_schs machine_code =
  let machine_code =
    if !Options.optimization >= 4 (* && !Options.output <> "horn" *) then
      begin
	Log.report ~level:1 
	  (fun fmt -> Format.fprintf fmt ".. machines optimization: sub-expression elimination@,");
	let machine_code = machines_cse machine_code in
	Log.report ~level:3 (fun fmt -> Format.fprintf fmt ".. generated machines (sub-expr elim):@ %a@ "pp_machines machine_code);
	machine_code
      end
    else
      machine_code
  in
  (* Optimize machine code *)
  let machine_code, removed_table = 
    if !Options.optimization >= 2 && !Options.output <> "emf" (*&& !Options.output <> "horn"*) then
      begin
	Log.report ~level:1 (fun fmt -> Format.fprintf fmt 
	  ".. machines optimization: const. inlining (partial eval. with const)@,");
	let machine_code, removed_table = machines_unfold (Corelang.get_consts prog) node_schs machine_code in
	Log.report ~level:3 (fun fmt -> Format.fprintf fmt "\t@[Eliminated constants: @[%a@]@]@ "
	  (pp_imap pp_elim) removed_table);
	Log.report ~level:3 (fun fmt -> Format.fprintf fmt ".. generated machines (const inlining):@ %a@ "pp_machines machine_code);	
	machine_code, removed_table
      end
    else
      machine_code, IMap.empty
  in  
  (* Optimize machine code *)
  let machine_code =
    if !Options.optimization >= 3 && not (Backends.is_functional ()) then
      begin
	Log.report ~level:1 (fun fmt -> Format.fprintf fmt ".. machines optimization: minimize stack usage by reusing variables@,");
	let node_schs    = Scheduling.remove_prog_inlined_locals removed_table node_schs in
	let reuse_tables = Scheduling.compute_prog_reuse_table node_schs in
	machines_fusion (machines_reuse_variables machine_code reuse_tables)
      end
    else
      machine_code
  in
  
  (* Salsa optimize machine code *)
  (*
  let machine_code = 
    if !Options.salsa_enabled then
      begin
	check_main ();
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. salsa machines optimization: optimizing floating-point accuracy with Salsa@,");
	(* Selecting float constants for Salsa *)
	let constEnv = List.fold_left (
	  fun accu c_topdecl ->
	    match c_topdecl.top_decl_desc with
	    | Const c when Types.is_real_type c.const_type  ->
	      (c.const_id, c.const_value) :: accu
	    | _ -> accu
	) [] (Corelang.get_consts prog) 
	in
	List.map 
	  (Machine_salsa_opt.machine_t2machine_t_optimized_by_salsa constEnv) 
	  machine_code 
      end
    else
      machine_code
  in
  Log.report ~level:3 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@ "
    (Utils.fprintf_list ~sep:"@ " Machine_code.pp_machine)
    machine_code);
  *)


    machine_code  
    
    
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
