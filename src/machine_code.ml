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
open Machine_code_common
open Corelang
open Clocks
open Causality
  
exception NormalizationError


       
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
    mk_val (Var var_id) var_id.var_type
  with Not_found ->
    try (* id is a constant *)
      let vdecl = (Corelang.var_decl_of_const (const_of_top (Hashtbl.find Corelang.consts_table id))) in
      mk_val (Var vdecl) vdecl.var_type
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
			    mk_conditional ?lustre_eq:(Some eq) g
                              [translate_act node args (y, t)]
                              [translate_act node args (y, e)]
  | Expr_merge (x, hl)   -> mkinstr ?lustre_eq:(Some eq) (MBranch (translate_ident node args x,
                                     List.map (fun (t,  h) -> t, [translate_act node args (y, h)]) hl))
  | _                    -> mkinstr ?lustre_eq:(Some eq)  (MLocalAssign (y, translate_expr node args expr))

let reset_instance node args i r c =
  match r with
  | None        -> []
  | Some r      -> let g = translate_guard node args r in
                   [control_on_clock node args c (mk_conditional g [mkinstr (MReset i)] [mkinstr (MNoReset i)])]

let translate_eq node ((m, si, j, d, s) as args) eq =
  (* Format.eprintf "translate_eq %a with clock %a@." Printers.pp_node_eq eq Clocks.print_ck eq.eq_rhs.expr_clock;  *)
  match eq.eq_lhs, eq.eq_rhs.expr_desc with
  | [x], Expr_arrow (e1, e2)                     ->
     let var_x = get_node_var x node in
     let o = new_instance node Arrow.arrow_top_decl eq.eq_rhs.expr_tag in
     let c1 = translate_expr node args e1 in
     let c2 = translate_expr node args e2 in
     (m,
      mkinstr (MReset o) :: si,
      Utils.IMap.add o (Arrow.arrow_top_decl, []) j,
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


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
