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

let pp_var fmt id = fprintf fmt "%s%s: %a" (if id.var_dec_const then "const " else "") id.var_id Types.print_ty id.var_type

let pp_node_var fmt id = fprintf fmt "%s%s: %a%a" (if id.var_dec_const then "const " else "") id.var_id Types.print_node_ty id.var_type Clocks.print_ck_suffix id.var_clock

let pp_node_args = fprintf_list ~sep:"; " pp_node_var 

let pp_quantifiers fmt (q, vars) =
  match q with
    | Forall -> fprintf fmt "forall %a" (fprintf_list ~sep:"; " pp_var) vars 
    | Exists -> fprintf fmt "exists %a" (fprintf_list ~sep:"; " pp_var) vars 

(*
let pp_econst fmt c = 
  match c with
    | EConst_int i -> pp_print_int fmt i
    | EConst_real r -> pp_print_string fmt r
    | EConst_float r -> pp_print_float fmt r
    | EConst_tag  t -> pp_print_string fmt t
    | EConst_string s -> pp_print_string fmt ("\"" ^ s ^ "\"")


let rec pp_eexpr fmt eexpr = 
  match eexpr.eexpr_desc with
    | EExpr_const c -> pp_econst fmt c
    | EExpr_ident id -> pp_print_string fmt id
    | EExpr_tuple el -> fprintf_list ~sep:"," pp_eexpr fmt el
    | EExpr_arrow (e1, e2) -> fprintf fmt "%a -> %a" pp_eexpr e1 pp_eexpr e2
    | EExpr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_eexpr e1 pp_eexpr e2
    (* | EExpr_concat (e1, e2) -> fprintf fmt "%a::%a" pp_eexpr e1 pp_eexpr e2 *)
    (* | EExpr_tail e -> fprintf fmt "tail %a" pp_eexpr e *)
    | EExpr_pre e -> fprintf fmt "pre %a" pp_eexpr e
    | EExpr_when (e, id) -> fprintf fmt "%a when %s" pp_eexpr e id
    | EExpr_merge (id, e1, e2) -> 
      fprintf fmt "merge (%s, %a, %a)" id pp_eexpr e1 pp_eexpr e2
    | EExpr_appl (id, e, r) -> pp_eapp fmt id e r
    | EExpr_forall (vars, e) -> fprintf fmt "forall %a; %a" pp_node_args vars pp_eexpr e 
    | EExpr_exists (vars, e) -> fprintf fmt "exists %a; %a" pp_node_args vars pp_eexpr e 


    (* | EExpr_whennot _ *)
    (* | EExpr_uclock _ *)
    (* | EExpr_dclock _ *)
    (* | EExpr_phclock _ -> assert false *)
and pp_eapp fmt id e r =
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
*)


let rec pp_struct_const_field fmt (label, c) =
  fprintf fmt "%a = %a;" pp_print_string label pp_const c
and pp_const fmt c = 
  match c with
    | Const_int i -> pp_print_int fmt i
    | Const_real r -> pp_print_string fmt r
    | Const_float r -> pp_print_float fmt r
    | Const_tag  t -> pp_print_string fmt t
    | Const_array ca -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:"," pp_const) ca
    | Const_struct fl -> Format.fprintf fmt "{%a }" (Utils.fprintf_list ~sep:" " pp_struct_const_field) fl
    | Const_string s -> pp_print_string fmt ("\"" ^ s ^ "\"")


let rec pp_expr fmt expr =
  (match expr.expr_annot with 
  | None -> fprintf fmt "%t" 
  | Some ann -> fprintf fmt "(%a %t)" pp_expr_annot ann)
    (fun fmt -> 
      match expr.expr_desc with
    | Expr_const c -> pp_const fmt c
    | Expr_ident id -> Format.fprintf fmt "%s" id
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp_expr c pp_expr t pp_expr e
    | Expr_arrow (e1, e2) -> fprintf fmt "(%a -> %a)" pp_expr e1 pp_expr e2
    | Expr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_expr e1 pp_expr e2
    | Expr_pre e -> fprintf fmt "pre %a" pp_expr e
    | Expr_when (e, id, l) -> fprintf fmt "%a when %s(%s)" pp_expr e l id
    | Expr_merge (id, hl) -> 
      fprintf fmt "merge %s %a" id pp_handlers hl
    | Expr_appl (id, e, r) -> pp_app fmt id e r
    )
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
    | _, Expr_tuple _ -> fprintf fmt "%s %a" id pp_expr e
    | _ -> fprintf fmt "%s (%a)" id pp_expr e
)
  | Some (x, l) -> fprintf fmt "%s (%a) every %s(%s)" id pp_expr e l x 



and pp_eexpr fmt e =
  fprintf fmt "%a%t %a"
    (Utils.fprintf_list ~sep:"; " pp_quantifiers) e.eexpr_quantifiers
    (fun fmt -> match e.eexpr_quantifiers with [] -> () | _ -> fprintf fmt ";")
    pp_expr e.eexpr_qfexpr


and pp_expr_annot fmt expr_ann =
  let pp_annot fmt (kwds, ee) =
    Format.fprintf fmt "(*! %t: %a *)"
      (fun fmt -> match kwds with | [] -> assert false | [x] -> Format.pp_print_string fmt x | _ -> Format.fprintf fmt "/%a/" (fprintf_list ~sep:"/" Format.pp_print_string) kwds)
      pp_eexpr ee
  in
  fprintf_list ~sep:"@ " pp_annot fmt expr_ann.annots
    
	
let pp_node_eq fmt eq = 
  fprintf fmt "%a = %a;" 
    pp_eq_lhs eq.eq_lhs
    pp_expr eq.eq_rhs

let pp_node_eqs = fprintf_list ~sep:"@ " pp_node_eq 


let pp_var_type_dec fmt ty =
  let rec pp_var_struct_type_field fmt (label, tdesc) =
    fprintf fmt "%a : %a" pp_var_type_dec_desc tdesc pp_print_string label
  and pp_var_type_dec_desc fmt tdesc =
  match tdesc with 
    | Tydec_any -> fprintf fmt "<any>"
    | Tydec_int -> fprintf fmt "int"
    | Tydec_real -> fprintf fmt "real"
    | Tydec_float -> fprintf fmt "float"
    | Tydec_bool -> fprintf fmt "bool"
    | Tydec_clock t -> fprintf fmt "%a clock" pp_var_type_dec_desc t
    | Tydec_const t -> fprintf fmt "%s" t
    | Tydec_enum id_list -> fprintf fmt "enum {%a }" (fprintf_list ~sep:", " pp_print_string) id_list
    | Tydec_struct f_list -> fprintf fmt "struct {%a }" (fprintf_list ~sep:"; " pp_var_struct_type_field) f_list
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


let pp_spec fmt spec =
  fprintf fmt "@[<hov 2>(*@@ ";
  fprintf_list ~sep:"@;@@ " (fun fmt r -> fprintf fmt "requires %a;" pp_eexpr r) fmt spec.requires;
  fprintf_list ~sep:"@;@@ " (fun fmt r -> fprintf fmt "ensures %a; " pp_eexpr r) fmt spec.ensures;
  fprintf_list ~sep:"@;" (fun fmt (name, assumes, ensures, _) -> 
    fprintf fmt "behavior %s:@[@ %a@ %a@]" 
      name
      (fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "assumes %a;" pp_eexpr r)) assumes
      (fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "ensures %a;" pp_eexpr r)) ensures
  ) fmt spec.behaviors;
  fprintf fmt "@]*)";
  ()


let pp_asserts fmt asserts =
  match asserts with 
  | _::_ -> (
  fprintf fmt "(* Asserts definitions *)@ ";
  fprintf_list ~sep:"@ " (fun fmt assert_ -> 
    let expr = assert_.assert_expr in
    fprintf fmt "assert %a;" pp_expr expr 
  ) fmt asserts 
  )
  | _ -> ()
    
let pp_node fmt nd = 
fprintf fmt "@[<v 0>%a%t%s %s (%a) returns (%a)@.%a%alet@.@[<h 2>   @ @[<v>%a@ %a@ %a@]@ @]@.tel@]@."
  (fun fmt s -> match s with Some s -> pp_spec fmt s | _ -> ()) nd.node_spec
  (fun fmt -> match nd.node_spec with None -> () | Some _ -> Format.fprintf fmt "@.")
  (if nd.node_dec_stateless then "function" else "node")
  nd.node_id
  pp_node_args nd.node_inputs
  pp_node_args nd.node_outputs
  (fun fmt locals ->
  match locals with [] -> () | _ ->
    fprintf fmt "@[<v 4>var %a@]@ " 
      (fprintf_list ~sep:"@ " 
	 (fun fmt v -> fprintf fmt "%a;" pp_node_var v))
      locals
  ) nd.node_locals
  (fun fmt checks ->
  match checks with [] -> () | _ ->
    fprintf fmt "@[<v 4>check@ %a@]@ " 
      (fprintf_list ~sep:"@ " 
	 (fun fmt d -> fprintf fmt "%a" Dimension.pp_dimension d))
      checks
  ) nd.node_checks
  (fprintf_list ~sep:"@ " pp_expr_annot) nd.node_annot
  pp_node_eqs nd.node_eqs
  pp_asserts nd.node_asserts
(*fprintf fmt "@ /* Scheduling: %a */ @ " (fprintf_list ~sep:", " pp_print_string) (Scheduling.schedule_node nd)*)

let pp_imported_node fmt ind = 
  fprintf fmt "@[<v>%s %s (%a) returns (%a) %t@]"
    (if ind.nodei_stateless then "function" else "node")
    ind.nodei_id
    pp_node_args ind.nodei_inputs
    pp_node_args ind.nodei_outputs
    (fun fmt -> if ind.nodei_stateless then Format.fprintf fmt "stateless") 

let pp_const_list fmt clist = 
  fprintf_list ~sep:"@ " (fun fmt cdecl ->
    fprintf fmt "%s = %a;"
      cdecl.const_id pp_const cdecl.const_value) fmt clist

let pp_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "%a@ " pp_node nd
  | ImportedNode ind ->
    fprintf fmt "imported %a;@ " pp_imported_node ind
  | Consts clist -> (fprintf fmt "const %a@ " pp_const_list clist)
  | Open (local, s) -> if local then fprintf fmt "open \"%s\"" s else fprintf fmt "open <%s>" s


let pp_prog fmt prog = 
  fprintf_list ~sep:"@ " pp_decl fmt prog

let pp_short_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "node %s@ " nd.node_id
  | ImportedNode ind -> fprintf fmt "imported node %s" ind.nodei_id
  | Consts clist -> (fprintf fmt "const %a@ " pp_const_list clist)
    | Open (local, s) -> if local then fprintf fmt "open \"%s\"" s else fprintf fmt "open <%s>" s

let pp_lusi fmt decl = 
  match decl.top_decl_desc with
  | Node nd ->  
    fprintf fmt 
      "@[<v>%s %s (%a) returns (%a);@ @]@ "
      (if Stateless.check_node decl then "function" else "node")
      nd.node_id
      pp_node_args nd.node_inputs
      pp_node_args nd.node_outputs
| Consts clist -> (fprintf fmt "const %a@ " pp_const_list clist)
| ImportedNode _ | Open _ -> ()




let pp_lusi_header fmt filename prog =
  fprintf fmt "(* Generated Lustre Interface file from %s *)@." filename;
  fprintf fmt "(* generated by Lustre-C compiler version %s, %a *)@." Version.number pp_date (Unix.gmtime (Unix.time ()));
  fprintf fmt "(* feel free to mask some of the nodes by removing them from this file. *)@.@.";
  List.iter (fprintf fmt "%a@." pp_lusi) prog    
  
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
