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
open Corelang


type aut_state =
    {
      incoming_r' : var_decl;
      incoming_s' : var_decl;
      incoming_r : var_decl;
      incoming_s : var_decl;
      actual_r : var_decl;
      actual_s : var_decl
    }

let as_clock var_decl =
  let tydec = var_decl.var_dec_type in
  { var_decl with var_dec_type = { ty_dec_desc = Tydec_clock tydec.ty_dec_desc; ty_dec_loc = tydec.ty_dec_loc } }

let mkbool loc b =
 mkexpr loc (Expr_const (const_of_bool b))

let mkident loc id =
 mkexpr loc (Expr_ident id)

let mkconst loc id =
 mkexpr loc (Expr_const (Const_tag id))

let mkfby loc e1 e2 =
 mkexpr loc (Expr_arrow (e1, mkexpr loc (Expr_pre e2)))

let mkpair loc e1 e2 =
 mkexpr loc (Expr_tuple [e1; e2])

let mkidentpair loc restart state =
 mkexpr loc (Expr_tuple [mkident loc restart; mkident loc state])

let add_branch (loc, expr, restart, st) cont =
 mkexpr loc (Expr_ite (expr, mkexpr loc (Expr_tuple [mkbool loc restart; mkident loc st]), cont))

let mkhandler loc st unless until locals (stmts, asserts, annots) =
 {hand_state = st;
  hand_unless = unless;
  hand_until = until;
  hand_locals = locals;
  hand_stmts = stmts;
  hand_asserts = asserts;
  hand_annots = annots;
  hand_loc = loc}

let mkautomata loc id handlers =
  {aut_id = id;
   aut_handlers = handlers;
   aut_loc = loc}

let expr_of_exit loc restart state conds tag =
  mkexpr loc (Expr_when (List.fold_right add_branch conds (mkidentpair loc restart state), state, tag))

let rec unless_read reads handler =
  let res =
  List.fold_left (fun read (_, c, _, _) -> Utils.ISet.union read (get_expr_vars c)) reads handler.hand_unless
  in
(
(*
Format.eprintf "unless_reads %s = %a@." handler.hand_state (fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) (ISet.elements reads);
Format.eprintf "unless_reads' %s = %a@." handler.hand_state (fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) (ISet.elements res);
*)
res
)

let rec until_read reads handler =
  List.fold_left (fun read (_, c, _, _) -> Utils.ISet.union read (get_expr_vars c)) reads handler.hand_until

let rec handler_read reads handler =
  let locals = List.fold_left (fun locals v -> ISet.add v.var_id locals) ISet.empty handler.hand_locals in
  let allvars =
    List.fold_left (fun read stmt ->
      match stmt with
      | Eq eq -> Utils.ISet.union read (get_expr_vars eq.eq_rhs)
      | Aut aut -> automata_read read aut) reads handler.hand_stmts
  in let res = ISet.diff allvars locals
     in
(
(*
Format.eprintf "handler_allvars %s = %a@." handler.hand_state (fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) (ISet.elements allvars);
Format.eprintf "handler_read %s = %a@." handler.hand_state (fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) (ISet.elements res);
*)
res
)

and automata_read reads aut =
  List.fold_left (fun read handler -> until_read (handler_read (unless_read read handler) handler) handler) reads aut.aut_handlers

let rec handler_write writes handler =
  let locals = List.fold_left (fun locals v -> ISet.add v.var_id locals) ISet.empty handler.hand_locals in
  let allvars =
    List.fold_left (fun write stmt ->
      match stmt with
      | Eq eq -> List.fold_left (fun write v -> ISet.add v write) write eq.eq_lhs
      | Aut aut -> List.fold_left handler_write write aut.aut_handlers) writes handler.hand_stmts
  in ISet.diff allvars locals

let node_vars_of_idents node iset =
  List.fold_right (fun v res -> if ISet.mem v.var_id iset then v :: res else res) (get_node_vars node) []

let mkautomata_state nodeid used typedef loc id =
  let tydec_bool = { ty_dec_desc = Tydec_bool; ty_dec_loc = loc } in
  let tydec_state id = { ty_dec_desc = Tydec_const id; ty_dec_loc = loc } in
  let ckdec_any = { ck_dec_desc = Ckdec_any; ck_dec_loc = loc } in
  let incoming_r' = mk_new_name used (id ^ "__next_restart_in") in
  let incoming_s' = mk_new_name used (id ^ "__next_state_in") in
  let incoming_r = mk_new_name used (id ^ "__restart_in") in
  let incoming_s = mk_new_name used (id ^ "__state_in") in
  let actual_r = mk_new_name used (id ^ "__restart_act") in
  let actual_s = mk_new_name used (id ^ "__state_act") in
  {
    incoming_r' = mkvar_decl loc (incoming_r', tydec_bool, ckdec_any, false, None, Some nodeid);
    incoming_s' = mkvar_decl loc (incoming_s', tydec_state typedef.tydef_id, ckdec_any, false, None, Some nodeid);
    incoming_r = mkvar_decl loc (incoming_r, tydec_bool, ckdec_any, false, None, Some nodeid);
    incoming_s = mkvar_decl loc (incoming_s, tydec_state typedef.tydef_id, ckdec_any, false, None, Some nodeid);
    actual_r = mkvar_decl loc (actual_r  , tydec_bool, ckdec_any, false, None, Some nodeid);
    actual_s = mkvar_decl loc (actual_s  , tydec_state typedef.tydef_id, ckdec_any, false, None, Some nodeid)
  }

let vars_of_aut_state aut_state =
  [aut_state.incoming_r'; aut_state.incoming_r; aut_state.actual_r; aut_state.incoming_s'; as_clock aut_state.incoming_s; as_clock aut_state.actual_s]

let node_of_unless nused used node aut_id aut_state handler =
(*Format.eprintf "node_of_unless %s@." node.node_id;*)
  let inputs = unless_read ISet.empty handler in
  let var_inputs = aut_state.incoming_r (*:: aut_state.incoming_s*) :: (node_vars_of_idents node inputs) in
  let var_outputs = aut_state.actual_r :: aut_state.actual_s :: [] in
  let init_expr = mkpair handler.hand_loc (mkident handler.hand_loc aut_state.incoming_r.var_id) (mkconst handler.hand_loc handler.hand_state) in
(*  let init_expr = mkidentpair handler.hand_loc aut_state.incoming_r.var_id aut_state.incoming_s.var_id in *)
  let expr_outputs = List.fold_right add_branch handler.hand_unless init_expr in
  let eq_outputs = Eq (mkeq handler.hand_loc ([aut_state.actual_r.var_id; aut_state.actual_s.var_id], expr_outputs)) in
  let node_id = mk_new_name nused (Format.sprintf "%s__%s_unless" aut_id handler.hand_state) in
  let args = List.map (fun v -> mkexpr handler.hand_loc (Expr_when (mkident handler.hand_loc v.var_id, aut_state.incoming_s.var_id, handler.hand_state))) var_inputs in
  let reset = Some (mkident handler.hand_loc aut_state.incoming_r.var_id) in
  {
    node_id = node_id;
    node_type = Types.new_var ();
    node_clock = Clocks.new_var true;
    node_inputs = List.map copy_var_decl var_inputs;
    node_outputs = List.map copy_var_decl var_outputs;
    node_locals = [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = []; 
    node_stmts = [ eq_outputs ];
    node_dec_stateless = false;
    node_stateless = None;
    node_spec = None;
    node_annot = []
  },
  mkexpr handler.hand_loc (Expr_appl (node_id, mkexpr handler.hand_loc (Expr_tuple args), reset))


let rename_output used name = mk_new_name used (Format.sprintf "%s_out" name)

let rec rename_stmts_outputs frename stmts =
  match stmts with
  | []           -> []
  | (Eq eq) :: q   -> let eq' = Eq { eq with eq_lhs = List.map frename eq.eq_lhs } in
		      eq' :: rename_stmts_outputs frename q
  | (Aut aut) :: q -> let handlers' = List.map (fun h -> { h with hand_stmts = rename_stmts_outputs frename h.hand_stmts}) aut.aut_handlers in
                      let aut' = Aut { aut with aut_handlers = handlers' } in
		      aut' :: rename_stmts_outputs frename q

let mk_frename used outputs =
  let table = ISet.fold (fun name table -> IMap.add name (rename_output used name) table) outputs IMap.empty in
  (fun name -> try IMap.find name table with Not_found -> name)

let node_of_assign_until nused used node aut_id aut_state handler =
(*Format.eprintf "node_of_assign_until %s@." node.node_id;*)
  let writes = handler_write ISet.empty handler in
  let inputs = ISet.diff (handler_read (until_read ISet.empty handler) handler) writes in
  let frename = mk_frename used writes in
  let var_inputs = aut_state.actual_r (*:: aut_state.actual_s*) :: node_vars_of_idents node inputs in
  let new_var_locals = node_vars_of_idents node writes in
  let var_outputs = List.sort IdentModule.compare (node_vars_of_idents node writes) in
  let new_var_outputs = List.map (fun vdecl -> { vdecl with var_id = frename vdecl.var_id }) var_outputs in
  let new_output_eqs = List.map2 (fun o o' -> Eq (mkeq handler.hand_loc ([o'.var_id], mkident handler.hand_loc o.var_id))) var_outputs new_var_outputs in
  let init_until = mkpair handler.hand_loc (mkconst handler.hand_loc tag_false) (mkconst handler.hand_loc handler.hand_state) in
  let until_expr = List.fold_right add_branch handler.hand_until init_until in
  let until_eq = Eq (mkeq handler.hand_loc ([aut_state.incoming_r.var_id; aut_state.incoming_s.var_id], until_expr)) in
  let node_id = mk_new_name nused (Format.sprintf "%s__%s_handler_until" aut_id handler.hand_state) in
  let args = List.map (fun v -> mkexpr handler.hand_loc (Expr_when (mkident handler.hand_loc v.var_id, aut_state.actual_s.var_id, handler.hand_state))) var_inputs in
  let reset = Some (mkident handler.hand_loc aut_state.actual_r.var_id) in
  List.fold_left (fun res v -> ISet.add v.var_id res) ISet.empty var_outputs,
  {
    node_id = node_id;
    node_type = Types.new_var ();
    node_clock = Clocks.new_var true;
    node_inputs = List.map copy_var_decl var_inputs;
    node_outputs = List.map copy_var_decl (aut_state.incoming_r :: aut_state.incoming_s :: new_var_outputs);
    node_locals = List.map copy_var_decl (new_var_locals @ handler.hand_locals);
    node_gencalls = [];
    node_checks = [];
    node_asserts = handler.hand_asserts; 
    node_stmts = until_eq :: new_output_eqs @ handler.hand_stmts;
    node_dec_stateless = false;
    node_stateless = None;
    node_spec = None;
    node_annot = handler.hand_annots
  },
  mkexpr handler.hand_loc (Expr_appl (node_id, mkexpr handler.hand_loc (Expr_tuple args), reset))

let typedef_of_automata aut =
  let tname = Format.sprintf "%s__type" aut.aut_id in
  { tydef_id = tname;
    tydef_desc = Tydec_enum (List.map (fun h -> h.hand_state) aut.aut_handlers)
  }

let expand_automata nused used owner typedef node aut =
  let initial = (List.hd aut.aut_handlers).hand_state in
  let aut_state = mkautomata_state node.node_id used typedef aut.aut_loc aut.aut_id in
  let unodes = List.map (fun h -> node_of_unless nused used node aut.aut_id aut_state h) aut.aut_handlers in
  let aunodes = List.map (fun h -> node_of_assign_until nused used node aut.aut_id aut_state h) aut.aut_handlers in
  let all_outputs = List.fold_left (fun all (outputs, _, _) -> ISet.union outputs all) ISet.empty aunodes in
  let unless_handlers = List.map2 (fun h (n, c) -> (h.hand_state, c)) aut.aut_handlers unodes in
  let unless_expr = mkexpr aut.aut_loc (Expr_merge (aut_state.incoming_s.var_id, unless_handlers)) in
  let unless_eq = mkeq aut.aut_loc ([aut_state.actual_r.var_id; aut_state.actual_s.var_id], unless_expr) in
  let assign_until_handlers = List.map2 (fun h (_, n, c) -> (h.hand_state, c)) aut.aut_handlers aunodes in
  let assign_until_expr = mkexpr aut.aut_loc (Expr_merge (aut_state.actual_s.var_id, assign_until_handlers)) in
  let assign_until_vars = [aut_state.incoming_r'.var_id; aut_state.incoming_s'.var_id] @ (ISet.elements all_outputs) in
  let assign_until_eq = mkeq aut.aut_loc (assign_until_vars, assign_until_expr) in
  let fby_incoming_expr = mkfby aut.aut_loc (mkpair aut.aut_loc (mkconst aut.aut_loc tag_false) (mkconst aut.aut_loc initial)) (mkidentpair aut.aut_loc aut_state.incoming_r'.var_id aut_state.incoming_s'.var_id) in
  let incoming_eq = mkeq aut.aut_loc ([aut_state.incoming_r.var_id; aut_state.incoming_s.var_id], fby_incoming_expr) in
  let locals' = vars_of_aut_state aut_state in
  let eqs' = [Eq unless_eq; Eq assign_until_eq; Eq incoming_eq] in
  (  List.map2 (fun h (n, _) -> mktop_decl h.hand_loc owner false (Node n)) aut.aut_handlers unodes
   @ List.map2 (fun h (_, n, _) -> mktop_decl h.hand_loc owner false (Node n)) aut.aut_handlers aunodes,
  locals',
  eqs')

let expand_node_stmt nused used owner node (top_types, top_nodes, locals, eqs) stmt =
  match stmt with
  | Eq eq -> (top_types, top_nodes, locals, (Eq eq)::eqs)
  | Aut aut ->
    let typedef = typedef_of_automata aut in
    let used' name = used name || List.exists (fun v -> v.var_id = name) locals in
    let nused' name =
      nused name ||
      List.exists (fun t -> match t.top_decl_desc with
      | ImportedNode nd -> nd.nodei_id = name | Node nd -> nd.node_id = name
      | _ -> false) top_nodes in
    let (top_decls', locals', eqs') = expand_automata nused' used' owner typedef node aut in
    let top_typedef = mktop_decl aut.aut_loc owner false (TypeDef typedef) in
    (top_typedef :: top_types, top_decls'@top_nodes, locals'@locals, eqs'@eqs)

let expand_node_stmts nused used loc owner node =
  let (top_types', top_nodes', locals', eqs') =
    List.fold_left (expand_node_stmt nused used owner node) ([], [], [], []) node.node_stmts in
  let node' = 
    { node with node_locals = locals'@node.node_locals; node_stmts = eqs' } in
  let top_node = mktop_decl loc owner false (Node node') in
  top_types', top_node, top_nodes'

let rec expand_decls_rec nused top_decls =
  match top_decls with
  | [] -> []
  | top_decl::q ->
    match top_decl.top_decl_desc with
    | Node nd ->
      let used name =
	   List.exists (fun v -> v.var_id = name) nd.node_inputs
	|| List.exists (fun v -> v.var_id = name) nd.node_outputs
	|| List.exists (fun v -> v.var_id = name) nd.node_locals in
      let (top_types', top_decl', top_nodes') = expand_node_stmts nused used top_decl.top_decl_loc top_decl.top_decl_owner nd in
      top_types' @ (top_decl' :: expand_decls_rec nused (top_nodes'@q))
    | _       -> top_decl :: expand_decls_rec nused q

let expand_decls top_decls =
  let top_names = List.fold_left (fun names t -> match t.top_decl_desc with
    | Node nd         -> ISet.add nd.node_id names
    | ImportedNode nd -> ISet.add nd.nodei_id names
    | _               -> names) ISet.empty top_decls in
  let nused name = ISet.mem name top_names in
  expand_decls_rec nused top_decls

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)

