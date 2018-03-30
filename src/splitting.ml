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
open Corelang
open Lustre_types
open Format


let rec tuple_split_expr expr = 
  match expr.expr_desc with
  | Expr_const _ 
  | Expr_ident _ -> [expr]
  | Expr_tuple elist -> elist
  | Expr_appl (id, args, r) ->
    if Basic_library.is_homomorphic_fun id 
    then
      let args_list = List.map tuple_split_expr (expr_list_of_expr args) in
      List.map
	(fun arg -> {expr with expr_tag = Utils.new_tag (); expr_desc = Expr_appl (id, expr_of_expr_list args.expr_loc arg, r) })
	(transpose_list args_list)
    else
      [expr]
  | Expr_array el ->
    let args_list = List.map tuple_split_expr el in
    List.map
      (fun arg -> {expr with expr_tag = Utils.new_tag (); expr_desc = Expr_array arg })
      (transpose_list args_list)
  | Expr_access (e1, d) ->
    List.map
      (fun e1 -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_access (e1, d) })
      (tuple_split_expr e1)
 | Expr_power (e1, d) ->
    List.map
      (fun e1 -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_power (e1, d) })
      (tuple_split_expr e1)
  | Expr_arrow (e1,e2) -> 
    List.map2
      (fun e1 e2 -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_arrow (e1, e2) })
      (tuple_split_expr e1)
      (tuple_split_expr e2)
  | Expr_pre e ->
    List.map
      (fun e -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_pre e })
      (tuple_split_expr e)
  | Expr_fby (v, e) ->
    List.map
      (fun e -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_fby (v, e) })
      (tuple_split_expr e)
  | Expr_when (e, c, l) ->
    List.map
      (fun e -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_when (e, c, l) })
      (tuple_split_expr e)
  | Expr_ite (c, t, e) ->
    List.map2
      (fun t e -> { expr with expr_tag = Utils.new_tag (); expr_desc = Expr_ite (c, t, e) })
      (tuple_split_expr t)
      (tuple_split_expr e)
  | Expr_merge (c,hl) ->
    let tl, hl = List.split (List.map (fun (t, h) -> (t, tuple_split_expr h)) hl) in
    List.map
      (fun hl -> {expr with expr_tag = Utils.new_tag (); expr_desc = Expr_merge (c, List.combine tl hl) })
      (transpose_list hl)

let rec tuple_split_eq eq =
  let split_rhs = tuple_split_expr eq.eq_rhs in
  if List.length split_rhs = 1
  then
    [eq]
  else
    List.map2
      (fun lhs rhs -> mkeq eq.eq_loc ([lhs], rhs))
      eq.eq_lhs
      split_rhs

let tuple_split_eq_list eqs =
 List.fold_right (fun eq -> (@) (tuple_split_eq eq)) eqs []


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
