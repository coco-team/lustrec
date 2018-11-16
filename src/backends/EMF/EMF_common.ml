open Lustre_types
open Machine_code_types
module VSet = Corelang.VSet
open Format
open Machine_code_common

(* Matlab starting counting from 1.
   simple function to extract the element id in the list. Starts from 1. *)
let rec get_idx x l =
  match l with
  | hd::tl -> if hd = x then 1 else 1+(get_idx x tl)
  | [] -> assert false

let rec get_expr_vars v =
  match v.value_desc with
  | Cst c -> VSet.empty
  | Var v -> VSet.singleton v
  | Fun (_, args) -> List.fold_left (fun accu v -> VSet.union accu (get_expr_vars v)) VSet.empty args
  | _ -> assert false (* Invalid argument *)

let is_imported_node f m =
  let (decl, _) = List.assoc f m.mcalls in
  Corelang.is_imported_node decl

(* Handling of enumerated types: for the moment each of such type is transformed
   into an int: the idx number of the constant in the typedef. This is not so
   nice but is compatible with basic Simulink types: int, real, bools) *)
(*
let recorded_enums = ref []
let record_types prog =
  let typedefs = Corelang.get_typedefs prog in
  List.iter (fun top ->
    let consts = consts_of_enum_type top in
  ) prog
*)
    
(* Basic printing functions *)

let hash_map = Hashtbl.create 13
  
(* If string length of f is longer than 50 chars, we select the 10 first and
   last and put a hash in the middle *)
let print_protect fmt f =
  fprintf str_formatter "%t" f;
  let s = flush_str_formatter () in
  let l = String.length s in
  if l > 30 then
    (* let _ = Format.eprintf "Looking for variable %s in hash @[<v 0>%t@]@." *)
    (*   s *)
    (*   (fun fmt -> Hashtbl.iter (fun s new_s -> fprintf fmt "%s -> %s@ " s new_s) hash_map) *)
    (* in *)
    if Hashtbl.mem hash_map s then
    fprintf fmt "%s" (Hashtbl.find hash_map s)
    else
      let prefix = String.sub s 0 10 and
	  suffix = String.sub s (l-10) 10 in
      let hash = Hashtbl.hash s in
      fprintf str_formatter "%s_%i_%s" prefix hash suffix;
      let new_s = flush_str_formatter () in
      Hashtbl.add hash_map s new_s;
      fprintf fmt "%s" new_s
  else
    fprintf fmt "%s" s
    
let pp_var_string fmt v =fprintf fmt "\"%t\"" (fun fmt -> print_protect fmt (fun fmt -> fprintf fmt "%s" v)) 
let pp_var_name fmt v = print_protect fmt (fun fmt -> Printers.pp_var_name fmt v) 
(*let pp_node_args = fprintf_list ~sep:", " pp_var_name*)

(********* Printing types ***********)
(* Two cases:
   - printing a variable definition:
     -  we look at the declared type if available
     - if not, we print the inferred type

   - printing a constant definion
*)
  
  
let pp_tag_type fmt typ =
  let rec aux tydec_desc =
  match tydec_desc with  
  | Tydec_int -> fprintf fmt "int"
  | Tydec_real -> fprintf fmt "real"
  | Tydec_bool -> fprintf fmt "bool"
  | Tydec_clock ty -> aux ty
  | Tydec_enum const_list -> (
    let size = List.length const_list in
    if size < 255 then
      fprintf fmt "uint8"
    else if size < 65535 then
      fprintf fmt "uint16"
    else
      assert false (* Too much states. This not reasonable *)
  )
  | Tydec_const _ | Tydec_struct _ | Tydec_array _ | Tydec_any -> eprintf "unhandled cst tag in EMF: %a@." Printers.pp_var_type_dec_desc tydec_desc; assert false
  in
  aux typ.tydef_desc

     
let pp_cst_type fmt c (*infered_typ*) =
  match c with
  | Const_tag t ->
     let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.tag_table t)) in
     if typ.tydef_id = "bool" then
       fprintf fmt "bool"
     else
       pp_tag_type fmt typ
  | Const_int _ -> fprintf fmt "int" (*!Options.int_type*)
  | Const_real _ -> fprintf fmt "real" (*!Options.real_type*)
  | Const_string _ -> fprintf fmt "string" 
  | _ -> eprintf "cst: %a@." Printers.pp_const c; assert false

let rec pp_infered_type fmt t =
  let open Types in
  if is_bool_type t  then fprintf fmt "bool" else
  if is_int_type t then fprintf fmt "int" else (* !Options.int_type *)
  if is_real_type t then fprintf fmt "real" else (* !Options.real_type *)
  match t.tdesc with
  | Tclock t ->
     pp_infered_type fmt t
  | Tstatic (_, t) ->
     fprintf fmt "%a" pp_infered_type t
  | Tconst id ->
    (* This is a type id for a enumerated type, eg. introduced by automata *)
     let typ =
       (Corelang.typedef_of_top (Hashtbl.find Corelang.type_table (Tydec_const id)))
     in
     pp_tag_type fmt typ
   | Tlink ty -> 
       pp_infered_type fmt ty 
   | _ -> eprintf "unhandled type: %a@." Types.print_node_ty t; assert false
     
let rec pp_concrete_type dec_t infered_t fmt =
  match dec_t with
  | Tydec_int -> fprintf fmt "int" (* !Options.int_type *)
  | Tydec_real -> fprintf fmt "real" (* !Options.real_type *)
  (* TODO we could add more concrete types here if they were available in
     dec_t *)
  | Tydec_bool -> fprintf fmt "bool"
  | Tydec_clock t -> pp_concrete_type t infered_t fmt
  | Tydec_const id -> (
    (* This is a type id for a enumerated type, eg. introduced by automata *)
    let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.type_table dec_t)) in
    pp_tag_type fmt typ
  )
  | Tydec_any -> pp_infered_type fmt infered_t 
  | _ -> eprintf
     "unhandled construct in type printing for EMF backend: %a@."
     Printers.pp_var_type_dec_desc dec_t; raise (Failure "var")
       

(*let pp_cst_type fmt v =
  match v.value_desc with
  | Cst c-> pp_cst_type c v.value_type fmt (* constants do not have declared type (yet) *)
  | _ -> assert false
*)
       
let pp_var_type fmt v =
  try
    if Machine_types.is_specified v then
      Machine_types.pp_var_type fmt v
    else
      pp_concrete_type v.var_dec_type.ty_dec_desc v.var_type fmt
  with Failure _ -> eprintf "failed var: %a@." Printers.pp_var v; assert false
    
(******** Other print functions *)
    
let pp_emf_var_decl fmt v =
  fprintf fmt "@[{\"name\": \"%a\", \"datatype\":\"%a\", \"original_name\": \"%a\"}@]"
    pp_var_name v
    pp_var_type v
    Printers.pp_var_name v
    
let pp_emf_vars_decl fmt vl =
  fprintf fmt "@[";
  Utils.fprintf_list ~sep:",@ " pp_emf_var_decl fmt vl;
  fprintf fmt "@]"
  
let reset_name id =
  "reset_" ^ id
  
let pp_tag_id fmt t =
  let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.tag_table t)) in
  if typ.tydef_id = "bool" then
    pp_print_string fmt t
  else
    let const_list = match typ.tydef_desc with Tydec_enum tl -> tl | _ -> assert false in
    fprintf fmt "%i" (get_idx t const_list)

let pp_emf_cst fmt c =
  match c with
  | Const_tag t->
     let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.tag_table t)) in
     if typ.tydef_id = "bool" then (
       fprintf fmt "{@[\"type\": \"constant\",@ ";
       fprintf fmt"\"value\": \"%a\",@ "
	 Printers.pp_const c;
       fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type c;
       fprintf fmt "@]}"
     )
     else (
       fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\",@ " 
	 pp_tag_id t;
       fprintf fmt "\"origin_type\": \"%s\",@ \"origin_value\": \"%s\",@ "
	 typ.tydef_id t;
       fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type c;
       fprintf fmt "@]}"
     )
  | Const_string s ->
    fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%s\",@ " s;
    fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type c;
    fprintf fmt "@]}"
     
  | _ -> (
    fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\",@ "
      Printers.pp_const c;
    fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type c;
    fprintf fmt "@]}"
  )
  
  
let pp_emf_cst_or_var m fmt v =
  match v.value_desc with
  | Cst c -> pp_emf_cst fmt c
  | Var v -> (
    fprintf fmt "{@[\"type\": \"variable\",@ \"value\": \"%a\",@ "
      pp_var_name v;
    (*    fprintf fmt "\"original_name\": \"%a\",@ " Printers.pp_var_name v; *)
    fprintf fmt "\"datatype\": \"%a\"@ " pp_var_type v;
    fprintf fmt "@]}"
  )
  | _ -> eprintf "Not of cst or var: %a@." (pp_val m) v ; assert false (* Invalid argument *)


let pp_emf_cst_or_var_list m =
  Utils.fprintf_list ~sep:",@ " (pp_emf_cst_or_var m)

(* Printer lustre expr and eexpr *)
    
let rec pp_emf_expr fmt e =
  match e.expr_desc with
  | Expr_const c -> pp_emf_cst fmt c
  | Expr_ident id ->
     fprintf fmt "{@[\"type\": \"variable\",@ \"value\": \"%a\",@ "
       print_protect (fun fmt -> pp_print_string fmt id);
    fprintf fmt "\"datatype\": \"%t\"@ "
      (pp_concrete_type
	 Tydec_any (* don't know much about that time since it was not
		      declared. That may not work with clock constants *)
	 e.expr_type
      );
    fprintf fmt "@]}"

  | Expr_tuple el ->
     fprintf fmt "[@[<hov 0>%a@ @]]"
       (Utils.fprintf_list ~sep:",@ " pp_emf_expr) el
  | _ -> (
    Log.report ~level:2
      (fun fmt ->
	fprintf fmt "Warning: unhandled expression %a in annotation.@ "
	  Printers.pp_expr e;
	fprintf fmt "Will not be produced in the experted JSON EMF"
      );    
    fprintf fmt "\"unhandled construct, complain to Ploc\""
  )
(* Remaining constructs *)  
(* | Expr_ite   of expr * expr * expr *)
(* | Expr_arrow of expr * expr *)
(* | Expr_fby of expr * expr *)
(* | Expr_array of expr list *)
(* | Expr_access of expr * Dimension.dim_expr *)
(* | Expr_power of expr * Dimension.dim_expr *)
(* | Expr_pre of expr *)
(* | Expr_when of expr * ident * label *)
(* | Expr_merge of ident * (label * expr) list *)
(* | Expr_appl of call_t *)
     

let pp_emf_eexpr fmt ee =
  fprintf fmt "{@[<hov 0>\"quantifiers\": \"%a\",@ \"qfexpr\": @[%a@]@] }"
    (Utils.fprintf_list ~sep:"; " Printers.pp_quantifiers) ee.eexpr_quantifiers
    pp_emf_expr ee.eexpr_qfexpr
    
    
(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
