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

let mkbool loc b =
 mkexpr loc (Expr_const (const_of_bool b))

let mkident loc id =
 mkexpr loc (Expr_ident id)

let init loc restart st =
 mkexpr loc (Expr_tuple [mkbool loc restart; mkident loc st])

let add_branch (loc, expr, restart, st) cont =
 mkexpr loc (Expr_ite (expr, init loc restart st, cont))

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

let node_of_handler loc id inputs outputs handler =
 Node {
   node_id = id;
   node_type = Types.new_var ();
   node_clock = Clocks.new_var true;
   node_inputs = inputs;
   node_outputs = outputs;
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

let handler_read handler =
 List.fold_left (fun read eq -> get_expr_vars read eq.eq_rhs) ISet.empty handler.hand_eqs

let handler_write handler =
 List.fold_left (fun write eq -> List.fold_left (fun write v -> ISet.add v write) write eq.eq_lhs) ISet.empty handler.hand_eqs

let expr_of_exit_conditions loc st conds =
 List.fold_right add_branch conds (init loc false st)

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

(*
let rec extract_node expr top_decls =
  match expr.expr_desc with
  | Expr_const _
  | Expr_ident _
  | Expr_tuple _
  | Expr_ite   of expr * expr * expr
  | Expr_arrow of expr * expr
  | Expr_fby of expr * expr
  | Expr_array of expr list
  | Expr_access of expr * Dimension.dim_expr
  | Expr_power of expr * Dimension.dim_expr
  | Expr_pre of expr
  | Expr_when of expr * ident * label
  | Expr_merge of ident * (label * expr) list
  | Expr_appl
*)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
