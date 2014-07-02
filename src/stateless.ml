(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)
open LustreSpec
open Corelang

type error =
| Stateful_kwd of ident
| Stateful_imp of ident

exception Error of Location.t * error

let rec check_expr expr =
  match expr.expr_desc with
  | Expr_const _ 
  | Expr_ident _ -> true
  | Expr_tuple el
  | Expr_array el -> List.for_all check_expr el
  | Expr_access (e1, _)
  | Expr_power (e1, _) -> check_expr e1
  | Expr_ite (c, t, e) -> check_expr c && check_expr t && check_expr e
  | Expr_arrow _
  | Expr_fby _
  | Expr_pre _ -> false
  | Expr_when (e', i, l)-> check_expr e'
  | Expr_merge (i, hl) -> List.for_all (fun (t, h) -> check_expr h) hl 
  | Expr_appl (i, e', i') ->
    check_expr e' &&
      (Basic_library.is_internal_fun i || check_node (node_from_name i))
and compute_node nd =
 List.for_all (fun eq -> check_expr eq.eq_rhs) nd.node_eqs
and check_node td =
  match td.top_decl_desc with 
  | Node nd         -> (
    match nd.node_stateless with
    | None     -> 
      begin
	let stateless = compute_node nd in
	nd.node_stateless <- Some stateless;
	if nd.node_dec_stateless && (not stateless)
	then raise (Error (td.top_decl_loc, Stateful_kwd nd.node_id))
	else (nd.node_dec_stateless <- stateless; stateless)
      end
    | Some stl -> stl)
  | ImportedNode nd -> nd.nodei_stateless
  | _ -> true

let check_prog decls =
  List.iter (fun td -> ignore (check_node td)) decls

let check_compat_decl decl =
 match decl.top_decl_desc with
 | ImportedNode nd ->
   let td = Corelang.node_from_name nd.nodei_id in
   (match td.top_decl_desc with
   | Node nd' -> let stateless = check_node td in
		 if nd.nodei_stateless && (not stateless)
		 then raise (Error (td.top_decl_loc, Stateful_imp nd.nodei_id))
		 else nd'.node_dec_stateless <- nd.nodei_stateless
   | _        -> assert false)
 | Node _          -> assert false
 | _               -> ()

let check_compat header =
  List.iter check_compat_decl header

let pp_error fmt err =
  match err with
  | Stateful_kwd nd ->
    Format.fprintf fmt
      "node %s should be stateless but is actually stateful.@."
      nd
  | Stateful_imp nd ->
    Format.fprintf fmt
      "node %s is declared stateless but is actually stateful.@."
      nd

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
