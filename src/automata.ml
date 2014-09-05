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
open LustreSpec
open Corelang

let pp_restart fmt restart =
  Format.fprintf fmt "%s" (if restart then "restart" else "resume")

let pp_unless fmt (_, expr, restart, st) =
  Format.fprintf fmt "unless %a %a %s"
    Printers.pp_expr expr
    pp_restart restart
    st

let pp_until fmt (_, expr, restart, st) =
  Format.fprintf fmt "until %a %a %s"
    Printers.pp_expr expr
    pp_restart restart
    st

let pp_handler fmt handler =
  Format.fprintf fmt "state %s -> %a %a let %a tel %a"
    handler.hand_state
    (Utils.fprintf_list ~sep:"@ " pp_unless) handler.hand_unless
    (fun fmt locals ->
      match locals with [] -> () | _ ->
	Format.fprintf fmt "@[<v 4>var %a@]@ " 
	  (Utils.fprintf_list ~sep:"@ " 
	     (fun fmt v -> Format.fprintf fmt "%a;" Printers.pp_node_var v))
	  locals)
    handler.hand_locals
    Printers.pp_node_eqs handler.hand_eqs
    (Utils.fprintf_list ~sep:"@ " pp_until) handler.hand_until

let pp_automata fmt aut =
  Format.fprintf fmt "automaton %s %a"
    aut.aut_id
    (Utils.fprintf_list ~sep:"@ " pp_handler) aut.aut_handlers

let mkbool loc b =
 mkexpr loc (Expr_const (const_of_bool b))

let mkident loc id =
 mkexpr loc (Expr_ident id)

let mkfby loc e1 e2 =
 mkexpr loc (Expr_arrow (e1, mkexpr loc (Expr_pre e2)))

let init loc restart state =
 mkexpr loc (Expr_tuple [mkident loc restart; mkident loc state])

let add_branch (loc, expr, restart, st) cont =
 mkexpr loc (Expr_ite (expr, mkexpr loc (Expr_tuple [mkbool loc restart; mkident loc st]), cont))

let mkhandler loc st unless until locals (eqs, asserts, annots) =
 {hand_state = st;
  hand_unless = unless;
  hand_until = until;
  hand_locals = locals;
  hand_eqs = eqs;
  hand_asserts = asserts;
  hand_annots = annots;
  hand_loc = loc}

let mkautomata loc id handlers =
  {aut_id = id;
   aut_handlers = handlers;
   aut_loc = loc}

let handler_read handler =
 List.fold_left (fun read eq -> get_expr_vars read eq.eq_rhs) ISet.empty handler.hand_eqs

let handler_write handler =
 List.fold_left (fun write eq -> List.fold_left (fun write v -> ISet.add v write) write eq.eq_lhs) ISet.empty handler.hand_eqs

let node_of_handler node aut_id handler =
  let locals = List.fold_left (fun locals v -> ISet.add v.var_id locals) ISet.empty handler.hand_locals in 
  let inputs = handler_read handler in
  let outputs = handler_write handler in
  {
    node_id = Format.sprintf "%s_%s_handler" aut_id handler.hand_state;
    node_type = Types.new_var ();
    node_clock = Clocks.new_var true;
    node_inputs = List.map (fun v -> get_node_var v node) (ISet.elements (ISet.diff inputs locals));
    node_outputs = List.map (fun v -> get_node_var v node) (ISet.elements (ISet.diff outputs locals));
    node_locals = handler.hand_locals;
    node_gencalls = [];
    node_checks = [];
    node_asserts = handler.hand_asserts; 
    node_eqs = handler.hand_eqs;
    node_dec_stateless = false;
    node_stateless = None;
    node_spec = None;
    node_annot = handler.hand_annots
  }

let expr_of_exit loc restart state conds tag =
  mkexpr loc (Expr_when (List.fold_right add_branch conds (init loc restart state), state, tag))

let expr_of_handler loc restart state node tag =
  let arg = mkexpr loc (Expr_tuple (List.map (fun v -> mkident loc v.var_id) node.node_inputs)) in
  mkexpr loc (Expr_when (mkexpr loc (Expr_appl (node.node_id, arg, Some (restart, tag_true))), state, tag))

let assign_aut_handlers loc node actual_r actual_s hnodes =
  let outputs = (snd (List.hd hnodes)).node_outputs in
  let assign_handlers = List.map (fun (hs, n) -> (hs, expr_of_handler loc actual_r actual_s n hs)) hnodes in
  let assign_expr = mkexpr loc (Expr_merge (actual_s, assign_handlers)) in
  let assign_eq = mkeq loc (List.map (fun v -> v.var_id) outputs, assign_expr) in
  assign_eq

let typedef_of_automata node aut =
  let tname = Format.sprintf "%s_type" aut.aut_id in
  { tydef_id = tname;
    tydef_desc = Tydec_enum (List.map (fun h -> h.hand_state) aut.aut_handlers)
  }

let expand_automata top_node aut =
  let node = node_of_top top_node in
  let owner = top_node.top_decl_owner in
  let typedef = typedef_of_automata node aut in
  let initial = (List.hd aut.aut_handlers).hand_state in
  let incoming_r = mk_new_name (get_node_vars node) (aut.aut_id ^ "__restart_in") in
  let incoming_s = mk_new_name (get_node_vars node) (aut.aut_id ^ "__state_in") in
  let actual_r = mk_new_name (get_node_vars node) (aut.aut_id ^ "__restart_act") in
  let actual_s = mk_new_name (get_node_vars node) (aut.aut_id ^ "__state_act") in
  let unless_handlers = List.map (fun h -> (h.hand_state, expr_of_exit h.hand_loc incoming_r incoming_s h.hand_unless h.hand_state)) aut.aut_handlers in
  let unless_expr = mkexpr aut.aut_loc (Expr_merge (incoming_s, unless_handlers)) in
  let unless_eq = mkeq aut.aut_loc ([actual_r; actual_s], unless_expr) in
  let until_handlers = List.map (fun h -> (h.hand_state, expr_of_exit h.hand_loc actual_r actual_s h.hand_until h.hand_state)) aut.aut_handlers in
  let until_expr = mkexpr aut.aut_loc (Expr_merge (actual_s, until_handlers)) in
  let fby_until_expr = mkfby aut.aut_loc (init aut.aut_loc tag_false initial) until_expr in
  let until_eq = mkeq aut.aut_loc ([incoming_r; incoming_s], fby_until_expr) in
  let hnodes = List.map (fun h -> (h.hand_state, node_of_handler node aut.aut_id h)) aut.aut_handlers in
  let assign_eq = assign_aut_handlers aut.aut_loc node actual_r actual_s hnodes in
  let tydec_bool = { ty_dec_desc = Tydec_bool; ty_dec_loc = aut.aut_loc } in
  let tydec_const id = { ty_dec_desc = Tydec_const id; ty_dec_loc = aut.aut_loc } in
  let ckdec_any = { ck_dec_desc = Ckdec_any; ck_dec_loc = aut.aut_loc } in
  let locals' = [mkvar_decl aut.aut_loc (incoming_r, tydec_bool, ckdec_any, false);
                 mkvar_decl aut.aut_loc (actual_r  , tydec_bool, ckdec_any, false);
                 mkvar_decl aut.aut_loc (incoming_s, tydec_const typedef.tydef_id, ckdec_any, false);
                 mkvar_decl aut.aut_loc (actual_s  , tydec_const typedef.tydef_id, ckdec_any, false)] in
  let eqs' = [unless_eq; assign_eq; until_eq] in
  let node' = { node with node_locals = locals'@node.node_locals; node_eqs = eqs'@node.node_eqs } in
  (mktop_decl aut.aut_loc owner false (TypeDef typedef)) ::
  { top_node with top_decl_desc = Node node' } ::
  (List.map2 (fun h (hs, n) -> mktop_decl h.hand_loc owner false (Node n)) aut.aut_handlers hnodes)

let node_extract_automata top_decl =
  match top_decl.top_decl_desc with
  | Node nd -> [top_decl]
  | _ -> [top_decl]
(*
let extract_automata top_decls =
 List.fold_left (fun top_decls top_decl -> ) top_decls
*)
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
