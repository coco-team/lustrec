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

open Lustre_types
open Machine_code_types
open Corelang
open Clocks
open Causality

let print_statelocaltag = true
  
exception NormalizationError

module OrdVarDecl:Map.OrderedType with type t=var_decl =
  struct type t = var_decl;; let compare = compare end

module VSet = Set.Make(OrdVarDecl)

let rec pp_val fmt v =
  match v.value_desc with
    | Cst c         -> Printers.pp_const fmt c 
    | LocalVar v    ->
       if print_statelocaltag then
	 Format.fprintf fmt "%s(L)" v.var_id
       else
	 Format.pp_print_string fmt v.var_id
	   
    | StateVar v    ->
       if print_statelocaltag then
	 Format.fprintf fmt "%s(S)" v.var_id
       else
	 Format.pp_print_string fmt v.var_id
    | Array vl      -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:", " pp_val)  vl
    | Access (t, i) -> Format.fprintf fmt "%a[%a]" pp_val t pp_val i
    | Power (v, n)  -> Format.fprintf fmt "(%a^%a)" pp_val v pp_val n
    | Fun (n, vl)   -> Format.fprintf fmt "%s (%a)" n (Utils.fprintf_list ~sep:", " pp_val)  vl

let rec pp_instr fmt i =
  let _ =
    match i.instr_desc with
    | MLocalAssign (i,v) -> Format.fprintf fmt "%s<-l- %a" i.var_id pp_val v
    | MStateAssign (i,v) -> Format.fprintf fmt "%s<-s- %a" i.var_id pp_val v
    | MReset i           -> Format.fprintf fmt "reset %s" i
    | MNoReset i         -> Format.fprintf fmt "noreset %s" i
    | MStep (il, i, vl)  ->
       Format.fprintf fmt "%a = %s (%a)"
	 (Utils.fprintf_list ~sep:", " (fun fmt v -> Format.pp_print_string fmt v.var_id)) il
	 i
	 (Utils.fprintf_list ~sep:", " pp_val) vl
    | MBranch (g,hl)     ->
       Format.fprintf fmt "@[<v 2>case(%a) {@,%a@,}@]"
	 pp_val g
	 (Utils.fprintf_list ~sep:"@," pp_branch) hl
    | MComment s -> Format.pp_print_string fmt s
       
  in
  (* Annotation *)
  (* let _ = *)
  (*   match i.lustre_expr with None -> () | Some e -> Format.fprintf fmt " -- original expr: %a" Printers.pp_expr e *)
  (* in *)
  let _ = 
    match i.lustre_eq with None -> () | Some eq -> Format.fprintf fmt " -- original eq: %a" Printers.pp_node_eq eq
  in
  ()
    
and pp_branch fmt (t, h) =
  Format.fprintf fmt "@[<v 2>%s:@,%a@]" t (Utils.fprintf_list ~sep:"@," pp_instr) h

and pp_instrs fmt il =
  Format.fprintf fmt "@[<v 2>%a@]" (Utils.fprintf_list ~sep:"@," pp_instr) il

type step_t = {
  step_checks: (Location.t * value_t) list;
  step_inputs: var_decl list;
  step_outputs: var_decl list;
  step_locals: var_decl list;
  step_instrs: instr_t list;
  step_asserts: value_t list;
}

type static_call = top_decl * (Dimension.dim_expr list)

type machine_t = {
  mname: node_desc;
  mmemory: var_decl list;
  mcalls: (ident * static_call) list; (* map from stateful/stateless instance to node, no internals *)
  minstances: (ident * static_call) list; (* sub-map of mcalls, from stateful instance to node *)
  minit: instr_t list;
  mstatic: var_decl list; (* static inputs only *)
  mconst: instr_t list; (* assignments of node constant locals *)
  mstep: step_t;
  mspec: node_annot option;
  mannot: expr_annot list;
}

(* merge log: get_node_def was in c0f8 *)
(* Returns the node/machine associated to id in m calls *)
let get_node_def id m =
  try
    let (decl, _) = List.assoc id m.mcalls in
    Corelang.node_of_top decl
  with Not_found -> ( 
    (* Format.eprintf "Unable to find node %s in list [%a]@.@?" *)
    (*   id *)
    (*   (Utils.fprintf_list ~sep:", " (fun fmt (n,_) -> Format.fprintf fmt "%s" n)) m.mcalls *)
    (* ; *)
    raise Not_found
  )
    
(* merge log: machine_vars was in 44686 *)
let machine_vars m = m.mstep.step_inputs @ m.mstep.step_locals @ m.mstep.step_outputs @ m.mmemory

let pp_step fmt s =
  Format.fprintf fmt "@[<v>inputs : %a@ outputs: %a@ locals : %a@ checks : %a@ instrs : @[%a@]@ asserts : @[%a@]@]@ "
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_inputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_outputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_locals
    (Utils.fprintf_list ~sep:", " (fun fmt (_, c) -> pp_val fmt c)) s.step_checks
    (Utils.fprintf_list ~sep:"@ " pp_instr) s.step_instrs
    (Utils.fprintf_list ~sep:", " pp_val) s.step_asserts


let pp_static_call fmt (node, args) =
 Format.fprintf fmt "%s<%a>"
   (node_name node)
   (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) args

let pp_machine fmt m =
  Format.fprintf fmt
    "@[<v 2>machine %s@ mem      : %a@ instances: %a@ init     : %a@ const    : %a@ step     :@   @[<v 2>%a@]@ @  spec : @[%t@]@  annot : @[%a@]@]@ "
    m.mname.node_id
    (Utils.fprintf_list ~sep:", " Printers.pp_var) m.mmemory
    (Utils.fprintf_list ~sep:", " (fun fmt (o1, o2) -> Format.fprintf fmt "(%s, %a)" o1 pp_static_call o2)) m.minstances
    (Utils.fprintf_list ~sep:"@ " pp_instr) m.minit
    (Utils.fprintf_list ~sep:"@ " pp_instr) m.mconst
    pp_step m.mstep
    (fun fmt -> match m.mspec with | None -> () | Some spec -> Printers.pp_spec fmt spec)
    (Utils.fprintf_list ~sep:"@ " Printers.pp_expr_annot) m.mannot

let pp_machines fmt ml =
  Format.fprintf fmt "@[<v 0>%a@]" (Utils.fprintf_list ~sep:"@," pp_machine) ml

  
let rec is_const_value v =
  match v.value_desc with
  | Cst _          -> true
  | Fun (id, args) -> Basic_library.is_value_internal_fun v && List.for_all is_const_value args
  | _              -> false

(* Returns the declared stateless status and the computed one. *)
let get_stateless_status m =
 (m.mname.node_dec_stateless, try Utils.desome m.mname.node_stateless with _ -> failwith ("stateless status of machine " ^ m.mname.node_id ^ " not computed"))

let is_input m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_inputs

let is_output m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_outputs

let is_memory m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mmemory

let conditional ?lustre_eq c t e =
  mkinstr ?lustre_eq:lustre_eq  (MBranch(c, [ (tag_true, t); (tag_false, e) ]))

let dummy_var_decl name typ =
  {
    var_id = name;
    var_orig = false;
    var_dec_type = dummy_type_dec;
    var_dec_clock = dummy_clock_dec;
    var_dec_const = false;
    var_dec_value = None;
    var_parent_nodeid = None;
    var_type =  typ;
    var_clock = Clocks.new_ck Clocks.Cvar true;
    var_loc = Location.dummy_loc
  }

let arrow_id = "_arrow"

let arrow_typ = Types.new_ty Types.Tunivar

let arrow_desc =
  {
    node_id = arrow_id;
    node_type = Type_predef.type_bin_poly_op;
    node_clock = Clock_predef.ck_bin_univ;
    node_inputs= [dummy_var_decl "_in1" arrow_typ; dummy_var_decl "_in2" arrow_typ];
    node_outputs= [dummy_var_decl "_out" arrow_typ];
    node_locals= [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_stmts= [];
    node_dec_stateless = false;
    node_stateless = Some false;
    node_spec = None;
    node_annot = [];  }

let arrow_top_decl =
  {
    top_decl_desc = Node arrow_desc;
    top_decl_owner = (Options_management.core_dependency "arrow");
    top_decl_itf = false;
    top_decl_loc = Location.dummy_loc
  }

let mk_val v t =
  { value_desc = v; 
    value_type = t; 
    value_annot = None }
    
let arrow_machine =
  let state = "_first" in
  let var_state = dummy_var_decl state Type_predef.type_bool(* (Types.new_ty Types.Tbool) *) in
  let var_input1 = List.nth arrow_desc.node_inputs 0 in
  let var_input2 = List.nth arrow_desc.node_inputs 1 in
  let var_output = List.nth arrow_desc.node_outputs 0 in
  let cst b = mk_val (Cst (const_of_bool b)) Type_predef.type_bool in
  let t_arg = Types.new_univar () in (* TODO Xavier: c'est bien la bonne def ? *)
  {
    mname = arrow_desc;
    mmemory = [var_state];
    mcalls = [];
    minstances = [];
    minit = [mkinstr (MStateAssign(var_state, cst true))];
    mstatic = [];
    mconst = [];
    mstep = {
      step_inputs = arrow_desc.node_inputs;
      step_outputs = arrow_desc.node_outputs;
      step_locals = [];
      step_checks = [];
      step_instrs = [conditional (mk_val (StateVar var_state) Type_predef.type_bool)
			(List.map mkinstr
			[MStateAssign(var_state, cst false);
			 MLocalAssign(var_output, mk_val (LocalVar var_input1) t_arg)])
                        (List.map mkinstr
			[MLocalAssign(var_output, mk_val (LocalVar var_input2) t_arg)]) ];
      step_asserts = [];
    };
    mspec = None;
    mannot = [];
  }

let empty_desc =
  {
    node_id = arrow_id;
    node_type = Types.bottom;
    node_clock = Clocks.bottom;
    node_inputs= [];
    node_outputs= [];
    node_locals= [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_stmts= [];
    node_dec_stateless = true;
    node_stateless = Some true;
    node_spec = None;
    node_annot = [];  }

let empty_machine =
  {
    mname = empty_desc;
    mmemory = [];
    mcalls = [];
    minstances = [];
    minit = [];
    mstatic = [];
    mconst = [];
    mstep = {
      step_inputs = [];
      step_outputs = [];
      step_locals = [];
      step_checks = [];
      step_instrs = [];
      step_asserts = [];
    };
    mspec = None;
    mannot = [];
  }

let new_instance =
  let cpt = ref (-1) in
  fun caller callee tag ->
    begin
      let o =
	if Stateless.check_node callee then
	  node_name callee
	else
	  Printf.sprintf "ni_%d" (incr cpt; !cpt) in
      let o =
	if !Options.ansi && is_generic_node callee
	then Printf.sprintf "%s_inst_%d" o (Utils.position (fun e -> e.expr_tag = tag) caller.node_gencalls)
	else o in
      o
    end


(* translate_<foo> : node -> context -> <foo> -> machine code/expression *)
(* the context contains  m : state aka memory variables  *)
(*                      si : initialization instructions *)
(*                       j : node aka machine instances  *)
(*                       d : local variables             *)
(*                       s : step instructions           *)
let translate_ident node (m, si, j, d, s) id =
  (* Format.eprintf "trnaslating ident: %s@." id; *)
  try (* id is a node var *)
    let var_id = get_node_var id node in
    if VSet.exists (fun v -> v.var_id = id) m
    then (
      (* Format.eprintf "a STATE VAR@."; *)
      mk_val (StateVar var_id) var_id.var_type
    )
    else (
      (* Format.eprintf "a LOCAL VAR@."; *)
      mk_val (LocalVar var_id) var_id.var_type
    )
  with Not_found ->
    try (* id is a constant *)
      let vdecl = (Corelang.var_decl_of_const (const_of_top (Hashtbl.find Corelang.consts_table id))) in
      mk_val (LocalVar vdecl) vdecl.var_type
    with Not_found ->
      (* id is a tag *)
      (* DONE construire une liste des enum declarés et alors chercher dedans la liste
	 qui contient id *)
      try
        let typ = (typedef_of_top (Hashtbl.find Corelang.tag_table id)).tydef_id in
        mk_val (Cst (Const_tag id)) (Type_predef.type_const typ)
      with Not_found -> (Format.eprintf "internal error: Machine_code.translate_ident %s" id;
                         assert false)

let rec control_on_clock node ((m, si, j, d, s) as args) ck inst =
 match (Clocks.repr ck).cdesc with
 | Con    (ck1, cr, l) ->
   let id  = Clocks.const_of_carrier cr in
   control_on_clock node args ck1 (mkinstr
				     (* TODO il faudrait prendre le lustre
					associé à instr et rajouter print_ck_suffix
					ck) de clocks.ml *)
				     (MBranch (translate_ident node args id,
					       [l, [inst]] )))
 | _                   -> inst

let rec join_branches hl1 hl2 =
 match hl1, hl2 with
 | []          , _            -> hl2
 | _           , []           -> hl1
 | (t1, h1)::q1, (t2, h2)::q2 ->
   if t1 < t2 then (t1, h1) :: join_branches q1 hl2 else
   if t1 > t2 then (t2, h2) :: join_branches hl1 q2
   else (t1, List.fold_right join_guards h1 h2) :: join_branches q1 q2

and join_guards inst1 insts2 =
 match get_instr_desc inst1, List.map get_instr_desc insts2 with
 | _                   , []                               ->
   [inst1]
 | MBranch (x1, hl1), MBranch (x2, hl2) :: q when x1 = x2 ->
    mkinstr
      (* TODO on pourrait uniquement concatener les lustres de inst1 et hd(inst2) *)
      (MBranch (x1, join_branches (sort_handlers hl1) (sort_handlers hl2)))
   :: (List.tl insts2)
 | _ -> inst1 :: insts2

let join_guards_list insts =
 List.fold_right join_guards insts []

(* specialize predefined (polymorphic) operators
   wrt their instances, so that the C semantics
   is preserved *)
let specialize_to_c expr =
 match expr.expr_desc with
 | Expr_appl (id, e, r) ->
   if List.exists (fun e -> Types.is_bool_type e.expr_type) (expr_list_of_expr e)
   then let id =
	  match id with
	  | "="  -> "equi"
	  | "!=" -> "xor"
	  | _    -> id in
	{ expr with expr_desc = Expr_appl (id, e, r) }
   else expr
 | _ -> expr

let specialize_op expr =
  match !Options.output with
  | "C" -> specialize_to_c expr
  | _   -> expr

let rec translate_expr node ((m, si, j, d, s) as args) expr =
  let expr = specialize_op expr in
  let value_desc = 
    match expr.expr_desc with
    | Expr_const v                     -> Cst v
    | Expr_ident x                     -> (translate_ident node args x).value_desc
    | Expr_array el                    -> Array (List.map (translate_expr node args) el)
    | Expr_access (t, i)               -> Access (translate_expr node args t, translate_expr node args (expr_of_dimension i))
    | Expr_power  (e, n)               -> Power  (translate_expr node args e, translate_expr node args (expr_of_dimension n))
    | Expr_tuple _
    | Expr_arrow _ 
    | Expr_fby _
    | Expr_pre _                       -> (Printers.pp_expr Format.err_formatter expr; Format.pp_print_flush Format.err_formatter (); raise NormalizationError)
    | Expr_when    (e1, _, _)          -> (translate_expr node args e1).value_desc
    | Expr_merge   (x, _)              -> raise NormalizationError
    | Expr_appl (id, e, _) when Basic_library.is_expr_internal_fun expr ->
      let nd = node_from_name id in
      Fun (node_name nd, List.map (translate_expr node args) (expr_list_of_expr e))
    | Expr_ite (g,t,e) -> (
      (* special treatment depending on the active backend. For horn backend, ite
	 are preserved in expression. While they are removed for C or Java
	 backends. *)
      match !Options.output with
      | "horn" -> 
	 Fun ("ite", [translate_expr node args g; translate_expr node args t; translate_expr node args e])
      | "C" | "java" | _ -> 
	 (Format.eprintf "Normalization error for backend %s: %a@."
	    !Options.output
	    Printers.pp_expr expr;
	  raise NormalizationError)
    )
    | _                   -> raise NormalizationError
  in
  mk_val value_desc expr.expr_type

let translate_guard node args expr =
  match expr.expr_desc with
  | Expr_ident x  -> translate_ident node args x
  | _ -> (Format.eprintf "internal error: translate_guard %s %a@." node.node_id Printers.pp_expr expr;assert false)

let rec translate_act node ((m, si, j, d, s) as args) (y, expr) =
  let eq = Corelang.mkeq Location.dummy_loc ([y.var_id], expr) in
  match expr.expr_desc with
  | Expr_ite   (c, t, e) -> let g = translate_guard node args c in
			    conditional ?lustre_eq:(Some eq) g
                              [translate_act node args (y, t)]
                              [translate_act node args (y, e)]
  | Expr_merge (x, hl)   -> mkinstr ?lustre_eq:(Some eq) (MBranch (translate_ident node args x,
                                     List.map (fun (t,  h) -> t, [translate_act node args (y, h)]) hl))
  | _                    -> mkinstr ?lustre_eq:(Some eq)  (MLocalAssign (y, translate_expr node args expr))

let reset_instance node args i r c =
  match r with
  | None        -> []
  | Some r      -> let g = translate_guard node args r in
                   [control_on_clock node args c (conditional g [mkinstr (MReset i)] [mkinstr (MNoReset i)])]

let translate_eq node ((m, si, j, d, s) as args) eq =
  (* Format.eprintf "translate_eq %a with clock %a@." Printers.pp_node_eq eq Clocks.print_ck eq.eq_rhs.expr_clock;  *)
  match eq.eq_lhs, eq.eq_rhs.expr_desc with
  | [x], Expr_arrow (e1, e2)                     ->
     let var_x = get_node_var x node in
     let o = new_instance node arrow_top_decl eq.eq_rhs.expr_tag in
     let c1 = translate_expr node args e1 in
     let c2 = translate_expr node args e2 in
     (m,
      mkinstr (MReset o) :: si,
      Utils.IMap.add o (arrow_top_decl, []) j,
      d,
      (control_on_clock node args eq.eq_rhs.expr_clock (mkinstr ?lustre_eq:(Some eq) (MStep ([var_x], o, [c1;c2])))) :: s)
  | [x], Expr_pre e1 when VSet.mem (get_node_var x node) d     ->
     let var_x = get_node_var x node in
     (VSet.add var_x m,
      si,
      j,
      d,
      control_on_clock node args eq.eq_rhs.expr_clock (mkinstr ?lustre_eq:(Some eq) (MStateAssign (var_x, translate_expr node args e1))) :: s)
  | [x], Expr_fby (e1, e2) when VSet.mem (get_node_var x node) d ->
     let var_x = get_node_var x node in
     (VSet.add var_x m,
      mkinstr ?lustre_eq:(Some eq) (MStateAssign (var_x, translate_expr node args e1)) :: si,
      j,
      d,
      control_on_clock node args eq.eq_rhs.expr_clock (mkinstr ?lustre_eq:(Some eq) (MStateAssign (var_x, translate_expr node args e2))) :: s)

  | p  , Expr_appl (f, arg, r) when not (Basic_library.is_expr_internal_fun eq.eq_rhs) ->
     let var_p = List.map (fun v -> get_node_var v node) p in
     let el = expr_list_of_expr arg in
     let vl = List.map (translate_expr node args) el in
     let node_f = node_from_name f in
     let call_f =
       node_f,
       NodeDep.filter_static_inputs (node_inputs node_f) el in
     let o = new_instance node node_f eq.eq_rhs.expr_tag in
     let env_cks = List.fold_right (fun arg cks -> arg.expr_clock :: cks) el [eq.eq_rhs.expr_clock] in
     let call_ck = Clock_calculus.compute_root_clock (Clock_predef.ck_tuple env_cks) in
     (*Clocks.new_var true in
       Clock_calculus.unify_imported_clock (Some call_ck) eq.eq_rhs.expr_clock eq.eq_rhs.expr_loc;
       Format.eprintf "call %a: %a: %a@," Printers.pp_expr eq.eq_rhs Clocks.print_ck (Clock_predef.ck_tuple env_cks) Clocks.print_ck call_ck;*)
     (m,
      (if Stateless.check_node node_f then si else mkinstr (MReset o) :: si),
      Utils.IMap.add o call_f j,
      d,
      (if Stateless.check_node node_f
       then []
       else reset_instance node args o r call_ck) @
	(control_on_clock node args call_ck (mkinstr ?lustre_eq:(Some eq) (MStep (var_p, o, vl)))) :: s)
  (*
    (* special treatment depending on the active backend. For horn backend, x = ite (g,t,e)
    are preserved. While they are replaced as if g then x = t else x = e in  C or Java
    backends. *)
    | [x], Expr_ite   (c, t, e)
    when (match !Options.output with | "horn" -> true | "C" | "java" | _ -> false)
    ->
    let var_x = get_node_var x node in
    (m,
    si,
    j,
    d,
    (control_on_clock node args eq.eq_rhs.expr_clock
    (MLocalAssign (var_x, translate_expr node args eq.eq_rhs))::s)
    )

  *)
  | [x], _                                       -> (
    let var_x = get_node_var x node in
    (m, si, j, d,
     control_on_clock
       node
       args
       eq.eq_rhs.expr_clock
       (translate_act node args (var_x, eq.eq_rhs)) :: s
    )
  )
  | _                                            ->
     begin
       Format.eprintf "internal error: Machine_code.translate_eq %a@?" Printers.pp_node_eq eq;
       assert false
     end

let find_eq xl eqs =
  let rec aux accu eqs =
      match eqs with
	| [] ->
	  begin
	    Format.eprintf "Looking for variables %a in the following equations@.%a@."
	      (Utils.fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) xl
	      Printers.pp_node_eqs eqs;
	    assert false
	  end
	| hd::tl ->
	  if List.exists (fun x -> List.mem x hd.eq_lhs) xl then hd, accu@tl else aux (hd::accu) tl
    in
    aux [] eqs

(* Sort the set of equations of node [nd] according
   to the computed schedule [sch]
*)
let sort_equations_from_schedule nd sch =
  (* Format.eprintf "%s schedule: %a@." *)
  (* 		 nd.node_id *)
  (* 		 (Utils.fprintf_list ~sep:" ; " Scheduling.pp_eq_schedule) sch; *)
  let eqs, auts = get_node_eqs nd in
  assert (auts = []); (* Automata should be expanded by now *)
  let split_eqs = Splitting.tuple_split_eq_list eqs in
  let eqs_rev, remainder =
    List.fold_left
      (fun (accu, node_eqs_remainder) vl ->
       if List.exists (fun eq -> List.exists (fun v -> List.mem v eq.eq_lhs) vl) accu
       then
	 (accu, node_eqs_remainder)
       else
	 let eq_v, remainder = find_eq vl node_eqs_remainder in
	 eq_v::accu, remainder
      )
      ([], split_eqs)
      sch
  in
  begin
    if List.length remainder > 0 then (
      let eqs, auts = get_node_eqs nd in
      assert (auts = []); (* Automata should be expanded by now *)
      Format.eprintf "Equations not used are@.%a@.Full equation set is:@.%a@.@?"
		     Printers.pp_node_eqs remainder
      		     Printers.pp_node_eqs eqs;
      assert false);
    List.rev eqs_rev
  end

let constant_equations nd =
 List.fold_right (fun vdecl eqs ->
   if vdecl.var_dec_const
   then
     { eq_lhs = [vdecl.var_id];
       eq_rhs = Utils.desome vdecl.var_dec_value;
       eq_loc = vdecl.var_loc
     } :: eqs
   else eqs)
   nd.node_locals []

let translate_eqs node args eqs =
  List.fold_right (fun eq args -> translate_eq node args eq) eqs args;;

let translate_decl nd sch =
  (*Log.report ~level:1 (fun fmt -> Printers.pp_node fmt nd);*)

  let sorted_eqs = sort_equations_from_schedule nd sch in
  let constant_eqs = constant_equations nd in

  (* In case of non functional backend (eg. C), additional local variables have
     to be declared for each assert *)
  let new_locals, assert_instrs, nd_node_asserts =
    let exprl = List.map (fun assert_ -> assert_.assert_expr ) nd.node_asserts in
    if Backends.is_functional () then
      [], [], exprl  
    else (* Each assert(e) is associated to a fresh variable v and declared as
	    v=e; assert (v); *)
      let _, vars, eql, assertl =
	List.fold_left (fun (i, vars, eqlist, assertlist) expr ->
	  let loc = expr.expr_loc in
	  let var_id = nd.node_id ^ "_assert_" ^ string_of_int i in
	  let assert_var =
	    mkvar_decl
	      loc
	      ~orig:false (* fresh var *)
	      (var_id,
	       mktyp loc Tydec_bool,
	       mkclock loc Ckdec_any,
	       false, (* not a constant *)
	       None, (* no default value *)
	       Some nd.node_id
	      )
	  in
	  assert_var.var_type <- Type_predef.type_bool (* Types.new_ty (Types.Tbool) *); 
	  let eq = mkeq loc ([var_id], expr) in
	  (i+1, assert_var::vars, eq::eqlist, {expr with expr_desc = Expr_ident var_id}::assertlist)
	) (1, [], [], []) exprl
      in
      vars, eql, assertl
  in
  let locals_list = nd.node_locals @ new_locals in

  let nd = { nd with node_locals = locals_list } in
  let init_args = VSet.empty, [], Utils.IMap.empty, List.fold_right (fun l -> VSet.add l) locals_list VSet.empty, [] in
  (* memories, init instructions, node calls, local variables (including memories), step instrs *)
  let m0, init0, j0, locals0, s0 = translate_eqs nd init_args constant_eqs in
  assert (VSet.is_empty m0);
  assert (init0 = []);
  assert (Utils.IMap.is_empty j0);
  let m, init, j, locals, s as context_with_asserts = translate_eqs nd (m0, init0, j0, locals0, []) (assert_instrs@sorted_eqs) in
  let mmap = Utils.IMap.fold (fun i n res -> (i, n)::res) j [] in
  {
    mname = nd;
    mmemory = VSet.elements m;
    mcalls = mmap;
    minstances = List.filter (fun (_, (n,_)) -> not (Stateless.check_node n)) mmap;
    minit = init;
    mconst = s0;
    mstatic = List.filter (fun v -> v.var_dec_const) nd.node_inputs;
    mstep = {
      step_inputs = nd.node_inputs;
      step_outputs = nd.node_outputs;
      step_locals = VSet.elements (VSet.diff locals m);
      step_checks = List.map (fun d -> d.Dimension.dim_loc, translate_expr nd init_args (expr_of_dimension d)) nd.node_checks;
      step_instrs = (
	(* special treatment depending on the active backend. For horn backend,
	   common branches are not merged while they are in C or Java
	   backends. *)
	(*match !Options.output with
	| "horn" -> s
	  | "C" | "java" | _ ->*)
	if !Backends.join_guards then
	  join_guards_list s
	else
	  s
      );
      step_asserts = List.map (translate_expr nd context_with_asserts) nd_node_asserts;
    };
    mspec = nd.node_spec;
    mannot = nd.node_annot;
  }

(** takes the global declarations and the scheduling associated to each node *)
let translate_prog decls node_schs =
  let nodes = get_nodes decls in
  List.map
    (fun decl ->
     let node = node_of_top decl in
      let sch = (Utils.IMap.find node.node_id node_schs).Scheduling.schedule in
      translate_decl node sch
    ) nodes

let get_machine_opt name machines =
  List.fold_left
    (fun res m ->
      match res with
      | Some _ -> res
      | None -> if m.mname.node_id = name then Some m else None)
    None machines

let get_const_assign m id =
  try
    match get_instr_desc (List.find
	     (fun instr -> match get_instr_desc instr with
	     | MLocalAssign (v, _) -> v == id
	     | _ -> false)
	     m.mconst
    ) with
    | MLocalAssign (_, e) -> e
    | _                   -> assert false
  with Not_found -> assert false


let value_of_ident loc m id =
  (* is is a state var *)
  try
    let v = List.find (fun v -> v.var_id = id) m.mmemory
    in mk_val (StateVar v) v.var_type 
  with Not_found ->
    try (* id is a node var *)
      let v = get_node_var id m.mname
      in mk_val (LocalVar v) v.var_type
  with Not_found ->
    try (* id is a constant *)
      let c = Corelang.var_decl_of_const (const_of_top (Hashtbl.find Corelang.consts_table id))
      in mk_val (LocalVar c) c.var_type
    with Not_found ->
      (* id is a tag *)
      let t = Const_tag id
      in mk_val (Cst t) (Typing.type_const loc t)

(* type of internal fun used in dimension expression *)
let type_of_value_appl f args =
  if List.mem f Basic_library.arith_funs
  then (List.hd args).value_type
  else Type_predef.type_bool

let rec value_of_dimension m dim =
  match dim.Dimension.dim_desc with
  | Dimension.Dbool b         ->
     mk_val (Cst (Const_tag (if b then Corelang.tag_true else Corelang.tag_false))) Type_predef.type_bool
  | Dimension.Dint i          ->
     mk_val (Cst (Const_int i)) Type_predef.type_int
  | Dimension.Dident v        -> value_of_ident dim.Dimension.dim_loc m v
  | Dimension.Dappl (f, args) ->
     let vargs = List.map (value_of_dimension m) args
     in mk_val (Fun (f, vargs)) (type_of_value_appl f vargs) 
  | Dimension.Dite (i, t, e)  ->
     (match List.map (value_of_dimension m) [i; t; e] with
     | [vi; vt; ve] -> mk_val (Fun ("ite", [vi; vt; ve])) vt.value_type
     | _            -> assert false)
  | Dimension.Dlink dim'      -> value_of_dimension m dim'
  | _                         -> assert false

let rec dimension_of_value value =
  match value.value_desc with
  | Cst (Const_tag t) when t = Corelang.tag_true  -> Dimension.mkdim_bool  Location.dummy_loc true
  | Cst (Const_tag t) when t = Corelang.tag_false -> Dimension.mkdim_bool  Location.dummy_loc false
  | Cst (Const_int i)                             -> Dimension.mkdim_int   Location.dummy_loc i
  | LocalVar v                                    -> Dimension.mkdim_ident Location.dummy_loc v.var_id
  | Fun (f, args)                                 -> Dimension.mkdim_appl  Location.dummy_loc f (List.map dimension_of_value args)
  | _                                             -> assert false

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
