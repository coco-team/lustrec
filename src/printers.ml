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
open Format
open Utils

(* Prints [v] as [pp_fun] would do, but adds a backslash at each end of line,
   following the C convention for multiple lines macro *)
let pp_as_c_macro pp_fun fmt v =
  let formatter_out_funs = pp_get_formatter_out_functions fmt () in
  let macro_newline () =
    begin
      formatter_out_funs.out_string "\\" 0 1;
      formatter_out_funs.out_newline ()
    end in
  begin
    pp_set_formatter_out_functions fmt { formatter_out_funs with out_newline = macro_newline };
    pp_fun fmt v;
    pp_set_formatter_out_functions fmt formatter_out_funs;
  end

let rec pp_var_struct_type_field fmt (label, tdesc) =
  fprintf fmt "%a : %a;" pp_print_string label pp_var_type_dec_desc tdesc
and pp_var_type_dec_desc fmt tdesc =
  match tdesc with 
  | Tydec_any -> fprintf fmt "<any>"
  | Tydec_int -> fprintf fmt "int"
  | Tydec_real -> fprintf fmt "real"
  (* | Tydec_float -> fprintf fmt "float" *)
  | Tydec_bool -> fprintf fmt "bool"
  | Tydec_clock t -> fprintf fmt "%a clock" pp_var_type_dec_desc t
  | Tydec_const t -> fprintf fmt "%s" t
  | Tydec_enum id_list -> fprintf fmt "enum {%a }" (fprintf_list ~sep:", " pp_print_string) id_list
  | Tydec_struct f_list -> fprintf fmt "struct {%a }" (fprintf_list ~sep:" " pp_var_struct_type_field) f_list
  | Tydec_array (s, t) -> fprintf fmt "%a^%a" pp_var_type_dec_desc t Dimension.pp_dimension s

let pp_var_type_dec fmt ty =
  pp_var_type_dec_desc fmt ty.ty_dec_desc

let pp_var_name fmt id = fprintf fmt "%s" id.var_id
let pp_var_type fmt id =
  if !Options.print_dec_types then
    pp_var_type_dec fmt id.var_dec_type
  else
    Types.print_node_ty fmt id.var_type
let pp_var_clock fmt id = Clocks.print_ck_suffix fmt id.var_clock
  
let pp_eq_lhs = fprintf_list ~sep:", " pp_print_string

let pp_var fmt id =
  fprintf fmt "%s%s: %a"
    (if id.var_dec_const then "const " else "")
    id.var_id
    pp_var_type id

let pp_quantifiers fmt (q, vars) =
  match q with
    | Forall -> fprintf fmt "forall %a" (fprintf_list ~sep:"; " pp_var) vars 
    | Exists -> fprintf fmt "exists %a" (fprintf_list ~sep:"; " pp_var) vars 

let rec pp_struct_const_field fmt (label, c) =
  fprintf fmt "%a = %a;" pp_print_string label pp_const c
and pp_const fmt c = 
  match c with
    | Const_int i -> pp_print_int fmt i
    | Const_real (c, e, s) -> pp_print_string fmt s (*if e = 0 then pp_print_int fmt c else if e < 0 then Format.fprintf fmt "%ie%i" c (-e) else Format.fprintf fmt "%ie-%i" c e *)
    (* | Const_float r -> pp_print_float fmt r *)
    | Const_tag  t -> pp_print_string fmt t
    | Const_array ca -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:"," pp_const) ca
    | Const_struct fl -> Format.fprintf fmt "{%a }" (Utils.fprintf_list ~sep:" " pp_struct_const_field) fl
    | Const_string s -> pp_print_string fmt ("\"" ^ s ^ "\"")


let pp_annot_key fmt kwds =
  match kwds with
  | [] -> assert false
  | [x] -> Format.pp_print_string fmt x
  | _ -> Format.fprintf fmt "/%a/" (fprintf_list ~sep:"/" Format.pp_print_string) kwds

let rec pp_expr fmt expr =
  (match expr.expr_annot with 
  | None -> fprintf fmt "%t" 
  | Some ann -> fprintf fmt "@[(%a %t)@]" pp_expr_annot ann)
    (fun fmt -> 
      match expr.expr_desc with
    | Expr_const c -> pp_const fmt c
    | Expr_ident id -> fprintf fmt "%s" id
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "@[<hov 1>(if %a then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@])" pp_expr c pp_expr t pp_expr e
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
  | None -> pp_call fmt id e
  | Some c -> fprintf fmt "%t every (%a)" (fun fmt -> pp_call fmt id e) pp_expr c 

and pp_call fmt id e =
  match id, e.expr_desc with
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
  | "!=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a <> %a)" pp_expr e1 pp_expr e2
  | "=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a = %a)" pp_expr e1 pp_expr e2
  | "not", _ -> fprintf fmt "(not %a)" pp_expr e
  | _, Expr_tuple _ -> fprintf fmt "%s %a" id pp_expr e
  | _ -> fprintf fmt "%s (%a)" id pp_expr e

and pp_eexpr fmt e =
  fprintf fmt "%a%t %a"
    (Utils.fprintf_list ~sep:"; " pp_quantifiers) e.eexpr_quantifiers
    (fun fmt -> match e.eexpr_quantifiers with [] -> () | _ -> fprintf fmt ";")
    pp_expr e.eexpr_qfexpr

and  pp_sf_value fmt e =
   fprintf fmt "%a"
     (* (Utils.fprintf_list ~sep:"; " pp_quantifiers) e.eexpr_quantifiers *)
     (* (fun fmt -> match e.eexpr_quantifiers *)
     (*             with [] -> () *)
     (*                | _ -> fprintf fmt ";") *)
     pp_expr e.eexpr_qfexpr

and pp_s_function fmt expr_ann =
  let pp_annot fmt (kwds, ee) =
    Format.fprintf fmt " %t : %a"
                   (fun fmt -> match kwds with
                               | [] -> assert false
                               | [x] -> Format.pp_print_string fmt x
                               | _ -> Format.fprintf fmt "%a" (fprintf_list ~sep:"/" Format.pp_print_string) kwds)
                   pp_sf_value ee
  in
  fprintf_list ~sep:"@ " pp_annot fmt expr_ann.annots

and pp_expr_annot fmt expr_ann =
  let pp_annot fmt (kwds, ee) =
    Format.fprintf fmt "(*! %a: %a; *)"
      pp_annot_key kwds
      pp_eexpr ee
  in
  fprintf_list ~sep:"@ " pp_annot fmt expr_ann.annots


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

(*
let pp_node_var fmt id = fprintf fmt "%s%s: %a(%a)%a" (if id.var_dec_const then "const " else "") id.var_id print_dec_ty id.var_dec_type.ty_dec_desc Types.print_ty id.var_type Clocks.print_ck_suffix id.var_clock
*)
let pp_node_var fmt id =
  begin
    fprintf fmt "%s%s: %a%a"
      (if id.var_dec_const then "const " else "")
      id.var_id
      pp_var_type id
      pp_var_clock id;
    match id.var_dec_value with
    | None -> () 
    | Some v -> fprintf fmt " = %a" pp_expr v
  end 

let pp_node_args = fprintf_list ~sep:";@ " pp_node_var 

let pp_node_eq fmt eq = 
  fprintf fmt "%a = %a;" 
    pp_eq_lhs eq.eq_lhs
    pp_expr eq.eq_rhs

let pp_restart fmt restart =
  Format.fprintf fmt "%s" (if restart then "restart" else "resume")

let pp_unless fmt (_, expr, restart, st) =
  Format.fprintf fmt "unless %a %a %s"
    pp_expr expr
    pp_restart restart
    st

let pp_until fmt (_, expr, restart, st) =
  Format.fprintf fmt "until %a %a %s"
    pp_expr expr
    pp_restart restart
    st

let rec pp_handler fmt handler =
  Format.fprintf fmt "state %s:@ @[<v 2>  %a%t%alet@,@[<v 2>  %a@ %a@ %a@]@,tel@ %a@]"
    handler.hand_state
    (Utils.fprintf_list ~sep:"@ " pp_unless) handler.hand_unless
    (fun fmt -> if not ([] = handler.hand_unless) then fprintf fmt "@ ")
    (fun fmt locals ->
      match locals with [] -> () | _ ->
	Format.fprintf fmt "@[<v 4>var %a@]@ " 
	  (Utils.fprintf_list ~sep:"@ " 
	     (fun fmt v -> Format.fprintf fmt "%a;" pp_node_var v))
	  locals)
    handler.hand_locals
    (fprintf_list ~sep:"@ " pp_expr_annot) handler.hand_annots
    pp_node_stmts handler.hand_stmts
    pp_asserts handler.hand_asserts
    (Utils.fprintf_list ~sep:"@," pp_until) handler.hand_until

and pp_node_stmt fmt stmt =
  match stmt with
  | Eq eq -> pp_node_eq fmt eq
  | Aut aut -> pp_node_aut fmt aut

and pp_node_stmts fmt stmts = fprintf_list ~sep:"@ " pp_node_stmt fmt stmts

and pp_node_aut fmt aut =
  Format.fprintf fmt "@[<v 0>automaton %s@,%a@]"
    aut.aut_id
    (Utils.fprintf_list ~sep:"@ " pp_handler) aut.aut_handlers

and pp_node_eqs fmt eqs = fprintf_list ~sep:"@ " pp_node_eq fmt eqs

let pp_typedef fmt ty =
  fprintf fmt "type %s = %a;" ty.tydef_id pp_var_type_dec_desc ty.tydef_desc

let pp_typedec fmt ty =
  fprintf fmt "type %s;" ty.tydec_id

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
  fprintf_list ~sep:"@,@@ " (fun fmt r -> fprintf fmt "requires %a;" pp_eexpr r) fmt spec.requires;
  fprintf_list ~sep:"@,@@ " (fun fmt r -> fprintf fmt "ensures %a; " pp_eexpr r) fmt spec.ensures;
  fprintf_list ~sep:"@," (fun fmt (name, assumes, ensures, _) -> 
    fprintf fmt "behavior %s:@[@ %a@ %a@]" 
      name
      (fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "assumes %a;" pp_eexpr r)) assumes
      (fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "ensures %a;" pp_eexpr r)) ensures
  ) fmt spec.behaviors;
  fprintf fmt "@]*)";
  ()

    
let pp_node fmt nd = 
fprintf fmt "@[<v 0>%a%t%s @[<hov 0>%s (@[%a)@]@ returns (@[%a)@]@]@ %a%alet@[<h 2>   @ @[<v>%a@ %a@ %a@]@]@ tel@]@ "
  (fun fmt s -> match s with Some s -> pp_spec fmt s | _ -> ()) nd.node_spec
  (fun fmt -> match nd.node_spec with None -> () | Some _ -> Format.fprintf fmt "@ ")
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
  pp_node_stmts nd.node_stmts
  pp_asserts nd.node_asserts
(*fprintf fmt "@ /* Scheduling: %a */ @ " (fprintf_list ~sep:", " pp_print_string) (Scheduling.schedule_node nd)*)

let pp_imported_node fmt ind = 
  fprintf fmt "@[<v>%s %s (%a) returns (%a)@]"
    (if ind.nodei_stateless then "function" else "node")
    ind.nodei_id
    pp_node_args ind.nodei_inputs
    pp_node_args ind.nodei_outputs

let pp_const_decl fmt cdecl =
  fprintf fmt "%s = %a;" cdecl.const_id pp_const cdecl.const_value

let pp_const_decl_list fmt clist = 
  fprintf_list ~sep:"@ " pp_const_decl fmt clist

let pp_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "%a" pp_node nd
  | ImportedNode ind ->
    fprintf fmt "imported %a;" pp_imported_node ind
  | Const c -> fprintf fmt "const %a" pp_const_decl c
  | Open (local, s) -> if local then fprintf fmt "#open \"%s\"" s else fprintf fmt "#open <%s>" s
  | TypeDef tdef -> fprintf fmt "%a" pp_typedef tdef

let pp_prog fmt prog =
  (* we first print types: the function SortProg.sort could do the job but ut
     introduces a cyclic dependance *)
  let type_decl, others =
    List.partition (fun decl -> match decl.top_decl_desc with TypeDef _ -> true | _ -> false) prog
  in
  fprintf fmt "@[<v 0>%a@]" (fprintf_list ~sep:"@ " pp_decl) (type_decl@others)

let pp_short_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "node %s@ " nd.node_id
  | ImportedNode ind -> fprintf fmt "imported node %s" ind.nodei_id
  | Const c -> fprintf fmt "const %a@ " pp_const_decl c
  | Open (local, s) -> if local then fprintf fmt "#open \"%s\"@ " s else fprintf fmt "#open <%s>@ " s
  | TypeDef tdef -> fprintf fmt "type %s;@ " tdef.tydef_id

let pp_lusi fmt decl = 
  match decl.top_decl_desc with
  | ImportedNode ind -> fprintf fmt "%a;@ " pp_imported_node ind
  | Const c -> fprintf fmt "const %a@ " pp_const_decl c
  | Open (local, s) -> if local then fprintf fmt "#open \"%s\"@ " s else fprintf fmt "#open <%s>@ " s
  | TypeDef tdef -> fprintf fmt "%a@ " pp_typedef tdef
  | Node _ -> assert false

let pp_lusi_header fmt basename prog =
  fprintf fmt "@[<v 0>";
  fprintf fmt "(* Generated Lustre Interface file from %s.lus *)@ " basename;
  fprintf fmt "(* by Lustre-C compiler version %s, %a *)@ " Version.number pp_date (Unix.gmtime (Unix.time ()));
  fprintf fmt "(* Feel free to mask some of the definitions by removing them from this file. *)@ @ ";
  List.iter (fprintf fmt "%a@ " pp_lusi) prog;
  fprintf fmt "@]@."

let pp_offset fmt offset =
  match offset with
  | Index i -> fprintf fmt "[%a]" Dimension.pp_dimension i
  | Field f -> fprintf fmt ".%s" f

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
