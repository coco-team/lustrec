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

(** Access checking module. Done after typing. Generates dimension constraints stored in nodes *)

let debug fmt args = () (* Format.eprintf "%a"  *)
(* Though it shares similarities with the clock calculus module, no code
    is shared.  Simple environments, very limited identifier scoping, no
    identifier redefinition allowed. *)

open Utils
(* Yes, opening both modules is dirty as some type names will be
   overwritten, yet this makes notations far lighter.*)
open Lustre_types
open Corelang
open Types
open Format

module ConstraintModule =
struct (* bool dimension module *)
  type t = Dimension.dim_expr
  let equal d1 d2 = Dimension.is_eq_dimension d1 d2
  let compare d1 d2 = if equal d1 d2 then 0 else compare d1.Dimension.dim_id d2.Dimension.dim_id
  let hash n = Hashtbl.hash n
end

module CSet = Set.Make(ConstraintModule)

(** [check_expr env expr] checks expression [expr] and gathers constraints 
    in set [checks]. *)
let rec check_expr checks expr =
  (*Format.eprintf "check_expr %a with type %a@." Printers.pp_expr expr Types.print_ty expr.expr_type;*)
  let res = 
  match expr.expr_desc with
  | Expr_const _
  | Expr_ident _ -> checks
  | Expr_array elist -> List.fold_left check_expr checks elist
  | Expr_access (e1, d) -> check_expr (CSet.add (Dimension.check_access expr.expr_loc (Types.array_type_dimension e1.expr_type) d) checks) e1
    (* TODO: check dimensions *)
 
  | Expr_power (e1, _) -> check_expr checks e1
 
  | Expr_tuple elist -> List.fold_left check_expr checks elist

  | Expr_ite (c, t, e) -> List.fold_left check_expr checks [c; t; e]
 
  | Expr_appl (_, args, _) -> check_expr checks args
 
  | Expr_fby (e1,e2)
  | Expr_arrow (e1,e2) -> check_expr (check_expr checks e1) e2
  | Expr_pre e1
  | Expr_when (e1,_,_) -> check_expr checks e1
 
  | Expr_merge (_,hl) -> List.fold_left (fun checks (l, h) -> check_expr checks h) checks hl
  in (*Format.eprintf "typing %B %a at %a = %a@." const Printers.pp_expr expr Location.pp_loc expr.expr_loc Types.print_ty res;*) res

let rec check_var_decl_type loc checks ty =
  if Types.is_array_type ty
  then
    check_var_decl_type loc
      (CSet.add (Dimension.check_bound loc (Types.array_type_dimension ty)) checks)
      (Types.array_element_type ty) 
  else checks

let check_var_decl checks vdecl =
  check_var_decl_type vdecl.var_loc checks vdecl.var_type

(** [check_node nd] checks node [nd]. 
    The resulting constraints are stored in nodes. *)
let check_node nd =
  let checks = CSet.empty in
  let checks =
    List.fold_left check_var_decl checks (get_node_vars nd) in
  let checks =
    let eqs, auts = get_node_eqs nd in
    assert (auts = []); (* Not checking automata yet . *)
    List.fold_left (fun checks eq -> check_expr checks eq.eq_rhs) checks eqs in
  nd.node_checks <- CSet.elements checks

let check_top_decl decl =
  match decl.top_decl_desc with
  | Node nd -> check_node nd
  | _ -> ()

let check_prog decls =
  List.iter check_top_decl decls


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
