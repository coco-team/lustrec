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
open Corelang
open LustreSpec
open Format
open Utils

(* Prints [v] as [pp_fun] would do, but adds a backslash at each end of line,
   following the C convention for multiple lines macro *)
let pp_as_c_macro pp_fun fmt v =
  let (out, flush, newline, spaces) = pp_get_all_formatter_output_functions fmt () in
  let macro_newline () = (out "\\" 0 1; newline ()) in
  begin
    pp_set_all_formatter_output_functions fmt out flush macro_newline spaces;
    pp_fun fmt v;
    pp_set_all_formatter_output_functions fmt out flush newline spaces;
  end


let pp_var_name fmt id = fprintf fmt "%s" id.var_id

let pp_eq_lhs = fprintf_list ~sep:", " pp_print_string

let rec pp_const fmt c = 
  match c with
    | Const_int i -> pp_print_int fmt i
    | Const_real r -> pp_print_string fmt r
    | Const_float r -> pp_print_float fmt r
    | Const_tag  t -> pp_print_string fmt t
    | Const_array ca -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:"," pp_const) ca

and pp_var fmt id = fprintf fmt "%s: %a" id.var_id Types.print_ty id.var_type

and pp_expr fmt expr =
  match expr.expr_desc with
    | Expr_const c -> pp_const fmt c
    | Expr_ident id -> Format.fprintf fmt "%s" id
(*    | Expr_cst_array (c, e) -> fprintf fmt "%a^%a" pp_expr e pp_const c *)
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp_expr c pp_expr t pp_expr e
    | Expr_arrow (e1, e2) -> fprintf fmt "%a -> %a" pp_expr e1 pp_expr e2
    | Expr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_expr e1 pp_expr e2
    | Expr_pre e -> fprintf fmt "pre %a" pp_expr e
    | Expr_when (e, id, l) -> fprintf fmt "%a when %s(%s)" pp_expr e l id
    | Expr_merge (id, hl) -> 
      fprintf fmt "merge %s %a" id pp_handlers hl
    | Expr_appl (id, e, r) -> pp_app fmt id e r
    | Expr_uclock _
    | Expr_dclock _
    | Expr_phclock _ -> assert false

and pp_tuple fmt el =
 fprintf_list ~sep:"," pp_expr fmt el

and pp_handler fmt (t, h) =
 fprintf fmt "(%s -> %a)" t pp_expr h

and pp_handlers fmt hl =
 fprintf_list ~sep:" " pp_handler fmt hl

and pp_app fmt id e r =
  match r with
  | None ->
    (match id, e.expr_desc with
    | "+", Expr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
    | "uminus", _ -> fprintf fmt "(- %a)" pp_expr e
    | "-", Expr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
    | "*", Expr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
    | "/", Expr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_expr e1 pp_expr e2
    | "mod", Expr_tuple([e1;e2]) -> fprintf fmt "(%a mod %a)" pp_expr e1 pp_expr e2
    | "&&", Expr_tuple([e1;e2]) -> fprintf fmt "(%a and %a)" pp_expr e1 pp_expr e2
    | "||", Expr_tuple([e1;e2]) -> fprintf fmt "(%a or %a)" pp_expr e1 pp_expr e2
    | "xor", Expr_tuple([e1;e2]) -> fprintf fmt "(%a xor %a)" pp_expr e1 pp_expr e2
    | "impl", Expr_tuple([e1;e2]) -> fprintf fmt "(%a => %a)" pp_expr e1 pp_expr e2
    | "<", Expr_tuple([e1;e2]) -> fprintf fmt "(%a < %a)" pp_expr e1 pp_expr e2
    | "<=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a <= %a)" pp_expr e1 pp_expr e2
    | ">", Expr_tuple([e1;e2]) -> fprintf fmt "(%a > %a)" pp_expr e1 pp_expr e2
    | ">=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a >= %a)" pp_expr e1 pp_expr e2
    | "!=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a != %a)" pp_expr e1 pp_expr e2
    | "=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a = %a)" pp_expr e1 pp_expr e2
    | "not", _ -> fprintf fmt "(not %a)" pp_expr e
    | _ -> fprintf fmt "%s (%a)" id pp_expr e)
  | Some (x, l) -> fprintf fmt "%s (%a) every %s(%s)" id pp_expr e l x 
	
let pp_node_eq fmt eq = 
  fprintf fmt "%a = %a;" 
    pp_eq_lhs eq.eq_lhs
    pp_expr eq.eq_rhs

let pp_node_eqs = fprintf_list ~sep:"@ " pp_node_eq 

let pp_node_args = fprintf_list ~sep:"; " pp_var 

let pp_var_type_dec fmt ty =
  let rec pp_var_type_dec_desc fmt tdesc =
  match tdesc with 
    | Tydec_any -> fprintf fmt "<any>"
    | Tydec_int -> fprintf fmt "int"
    | Tydec_real -> fprintf fmt "real"
    | Tydec_float -> fprintf fmt "float"
    | Tydec_bool -> fprintf fmt "bool"
    | Tydec_clock t -> fprintf fmt "%a clock" pp_var_type_dec_desc t
    | Tydec_const t -> fprintf fmt "%s" t
    | Tydec_enum id_list -> fprintf fmt "enum {%a }" (fprintf_list ~sep:", " pp_print_string) id_list
    | Tydec_array (s, t) -> fprintf fmt "%a^%a" pp_var_type_dec_desc t Dimension.pp_dimension s
in pp_var_type_dec_desc fmt ty.ty_dec_desc

(* let rec pp_var_type fmt ty =  *)
(*   fprintf fmt "%a" (match ty.tdesc with  *)
(*     | Tvar | Tarrow | Tlink | Tunivar -> assert false *)
(*     | Tint -> pp_print_string fmt "int" *)
(*     | Treal -> pp_print_string fmt "real" *)
(*     | Tbool -> pp_print_string fmt "bool" *)
(*     | Trat -> pp_print_string fmt "rat" *)
(*     | Tclock -> pp_print_string fmt "clock"  *)
(*     | Ttuple tel -> fprintf_list ~sep:" * " pp_var_type fmt tel *)
(*   ) *)

let pp_node fmt nd = 
fprintf fmt "@[<v>node %s (%a) returns (%a)@ %a%alet@ @[<h 2>   @ @[%a@]@ @]@ tel@]@ "
  nd.node_id
  pp_node_args nd.node_inputs
  pp_node_args nd.node_outputs
  (fun fmt locals ->
  match locals with [] -> () | _ ->
    fprintf fmt "@[<v 4>var %a@]@ " 
      (fprintf_list ~sep:"@ " 
	 (fun fmt v -> fprintf fmt "%a;" pp_var v))
      locals
  ) nd.node_locals
  (fun fmt checks ->
  match checks with [] -> () | _ ->
    fprintf fmt "@[<v 4>check@ %a@]@ " 
      (fprintf_list ~sep:"@ " 
	 (fun fmt d -> fprintf fmt "%a" Dimension.pp_dimension d))
      checks
  ) nd.node_checks
  pp_node_eqs nd.node_eqs
(*fprintf fmt "@ /* Scheduling: %a */ @ " (fprintf_list ~sep:", " pp_print_string) (Scheduling.schedule_node nd)*)

let pp_imported_node fmt ind = 
  fprintf fmt "@[<v>node %s (%a) returns (%a) %t@]"
    ind.nodei_id
    pp_node_args ind.nodei_inputs
    pp_node_args ind.nodei_outputs
    (fun fmt -> if ind.nodei_stateless then Format.fprintf fmt "stateless") 

let pp_imported_fun fmt ind = 
  fprintf fmt "@[<v>function %s (%a) returns (%a)@]"
    ind.fun_id
    pp_node_args ind.fun_inputs
    pp_node_args ind.fun_outputs

let pp_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "%a@ " pp_node nd
  | ImportedNode ind ->
    fprintf fmt "imported %a;@ " pp_imported_node ind
  | ImportedFun ind ->
    fprintf fmt "%a;@ " pp_imported_fun ind
  | Consts clist -> (
    fprintf fmt "const %a@ " 
      (fprintf_list ~sep:"@ " (fun fmt cdecl ->
	fprintf fmt "%s = %a;"
	  cdecl.const_id pp_const cdecl.const_value)) clist)
  | Include s -> fprintf fmt "include %s" s


let pp_prog fmt prog = 
  fprintf_list ~sep:"@ " pp_decl fmt prog

let pp_short_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "node %s@ " nd.node_id
  | ImportedNode ind -> fprintf fmt "imported node %s" ind.nodei_id
  | ImportedFun ind -> fprintf fmt "function %s" ind.fun_id
  | Consts clist -> (
    fprintf fmt "const %a@ " 
      (fprintf_list ~sep:"@ " (fun fmt cdecl ->
	pp_print_string fmt cdecl.const_id)) clist)
  | Include s -> fprintf fmt "include %s" s



let pp_econst fmt c = 
  match c with
    | EConst_int i -> pp_print_int fmt i
    | EConst_real r -> pp_print_string fmt r
    | EConst_float r -> pp_print_float fmt r
    | EConst_bool b -> pp_print_bool fmt b
    | EConst_string s -> pp_print_string fmt ("\"" ^ s ^ "\"")

let rec pp_eexpr is_output fmt eexpr = 
  let pp_eexpr = pp_eexpr is_output in
  match eexpr.eexpr_desc with
    | EExpr_const c -> pp_econst fmt c
    | EExpr_ident id -> 
      if is_output id then pp_print_string fmt ("*" ^ id) else pp_print_string fmt id
    | EExpr_tuple el -> fprintf_list ~sep:"," pp_eexpr fmt el
    | EExpr_arrow (e1, e2) -> fprintf fmt "%a -> %a" pp_eexpr e1 pp_eexpr e2
    | EExpr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_eexpr e1 pp_eexpr e2
    | EExpr_concat (e1, e2) -> fprintf fmt "%a::%a" pp_eexpr e1 pp_eexpr e2
    | EExpr_tail e -> fprintf fmt "tail %a" pp_eexpr e
    | EExpr_pre e -> fprintf fmt "pre %a" pp_eexpr e
    | EExpr_when (e, id) -> fprintf fmt "%a when %s" pp_eexpr e id
    | EExpr_merge (id, e1, e2) -> 
      fprintf fmt "merge (%s, %a, %a)" id pp_eexpr e1 pp_eexpr e2
    | EExpr_appl (id, e, r) -> pp_eapp is_output fmt id e r
    | EExpr_forall (vars, e) -> fprintf fmt "forall %a; %a" pp_node_args vars pp_eexpr e 
    | EExpr_exists (vars, e) -> fprintf fmt "exists %a; %a" pp_node_args vars pp_eexpr e 


    | EExpr_whennot _
    | EExpr_uclock _
    | EExpr_dclock _
    | EExpr_phclock _ -> assert false
and pp_eapp is_output fmt id e r =
  let pp_eexpr = pp_eexpr is_output in
  match r with
  | None ->
    (match id, e.eexpr_desc with
    | "+", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_eexpr e1 pp_eexpr e2
    | "uminus", _ -> fprintf fmt "(- %a)" pp_eexpr e
    | "-", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_eexpr e1 pp_eexpr e2
    | "*", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_eexpr e1 pp_eexpr e2
    | "/", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_eexpr e1 pp_eexpr e2
    | "mod", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a mod %a)" pp_eexpr e1 pp_eexpr e2
    | "&&", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a && %a)" pp_eexpr e1 pp_eexpr e2
    | "||", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a || %a)" pp_eexpr e1 pp_eexpr e2
    | "xor", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a ^^ %a)" pp_eexpr e1 pp_eexpr e2
    | "impl", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a ==> %a)" pp_eexpr e1 pp_eexpr e2
    | "<", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a < %a)" pp_eexpr e1 pp_eexpr e2
    | "<=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a <= %a)" pp_eexpr e1 pp_eexpr e2
    | ">", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a > %a)" pp_eexpr e1 pp_eexpr e2
    | ">=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a >= %a)" pp_eexpr e1 pp_eexpr e2
    | "!=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a != %a)" pp_eexpr e1 pp_eexpr e2
    | "=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a == %a)" pp_eexpr e1 pp_eexpr e2
    | "not", _ -> fprintf fmt "(! %a)" pp_eexpr e
    | "ite", EExpr_tuple([e1;e2;e3]) -> fprintf fmt "(if %a then %a else %a)" pp_eexpr e1 pp_eexpr e2 pp_eexpr e3
    | _ -> fprintf fmt "%s (%a)" id pp_eexpr e)
  | Some x -> fprintf fmt "%s (%a) every %s" id pp_eexpr e x 

  
let pp_ensures is_output fmt e =
  let pp_eexpr = pp_eexpr is_output in
  match e with
    | EnsuresExpr e -> fprintf fmt "ensures %a;@ " pp_eexpr e
    | SpecObserverNode (name, args) -> fprintf fmt "observer %s (%a);@ " name (fprintf_list ~sep:", " pp_eexpr) args
 
let pp_acsl_spec outputs fmt spec =
  let is_output = fun oid -> List.exists (fun v -> v.var_id = oid) outputs in
  let pp_eexpr = pp_eexpr is_output in
  fprintf fmt "@[<v 2>/*@@ ";
  fprintf_list ~sep:"" (fun fmt r -> fprintf fmt "requires %a;@ " pp_eexpr r) fmt spec.requires;
  fprintf_list ~sep:"" (pp_ensures is_output) fmt spec.ensures;
  fprintf fmt "@ ";
  (* fprintf fmt "assigns *self%t%a;@ "  *)
  (*   (fun fmt -> if List.length outputs > 0 then fprintf fmt ", ") *)
  (*   (fprintf_list ~sep:"," (fun fmt v -> fprintf fmt "*%s" v.var_id)) outputs; *)
  fprintf_list ~sep:"@ " (fun fmt (name, assumes, requires) -> 
    fprintf fmt "behavior %s:@[@ %a@ %a@]" 
      name
      (fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "assumes %a;" pp_eexpr r)) assumes
      (fprintf_list ~sep:"@ " (pp_ensures is_output)) requires
  ) fmt spec.behaviors;
  fprintf fmt "@]@ */@.";
  ()
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
