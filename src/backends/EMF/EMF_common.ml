open LustreSpec
open Format
open Machine_code 

(* Matlab starting counting from 1.
   simple function to extract the element id in the list. Starts from 1. *)
let rec get_idx x l =
  match l with
  | hd::tl -> if hd = x then 1 else 1+(get_idx x tl)
  | [] -> assert false

let rec get_expr_vars v =
  match v.value_desc with
  | Cst c -> VSet.empty
  | LocalVar v | StateVar v -> VSet.singleton v
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
    
let pp_var_string fmt v = fprintf fmt "\"%s\"" v
(*let pp_var_name fmt v = fprintf fmt "\"%a\"" Printers.pp_var_name v*)
(*let pp_node_args = fprintf_list ~sep:", " pp_var_name*)

(********* Printing types ***********)
(* Two cases:
   - printing a variable definition:
     -  we look at the declared type if available
     - if not, we print the inferred type

   - printing a constant definion
*)
  
  
let pp_tag_type fmt typ =
  let const_list = match typ.tydef_desc with Tydec_enum tl -> tl | _ -> assert false in
  let size = List.length const_list in
  if size < 255 then
    fprintf fmt "uint8"
  else if size < 65535 then
fprintf fmt "uint16"
  else
    assert false (* Too much states. This not reasonable *)
      
   
     
let pp_cst_type c infered_typ fmt =
  match c with
  | Const_tag t ->
     let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.tag_table t)) in
     if typ.tydef_id = "bool" then
       fprintf fmt "bool"
     else
       pp_tag_type fmt typ
  | Const_int _ -> fprintf fmt "%s" !Options.int_type
  | Const_real _ -> fprintf fmt "%s" !Options.real_type
  | _ -> Format.eprintf "cst: %a@." Printers.pp_const c; assert false

let rec pp_infered_type fmt t =
  let open Types in
  match t.tdesc with
  | Tint ->
     fprintf fmt "%s" !Options.int_type
  | Treal ->
     fprintf fmt "%s" !Options.real_type
  | Tbool ->
     fprintf fmt "bool"
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
  | _ -> Format.eprintf "unhandled type: %a@." Types.print_node_ty t; assert false
let rec pp_concrete_type dec_t infered_t fmt =
  match dec_t with
  | Tydec_int -> fprintf fmt "%s" !Options.int_type
  | Tydec_real -> fprintf fmt "%s" !Options.real_type
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
  | _ -> Format.eprintf
     "unhandled construct in type printing for EMF backend: %a@."
     Printers.pp_var_type_dec_desc dec_t; raise (Failure "var")
       

let pp_cst_type fmt v =
  match v.value_desc with
  | Cst c-> pp_cst_type c v.value_type fmt (* constants do not have declared type (yet) *)
  | _ -> assert false
     
let pp_var_type fmt v =
  try
  pp_concrete_type v.var_dec_type.ty_dec_desc v.var_type fmt
  with Failure _ -> Format.eprintf "failed var: %a@." Printers.pp_var v; assert false
(******** Other print functions *)
    
let pp_emf_var_decl fmt v =
  fprintf fmt "@[{\"name\": \"%a\", \"datatype\":\"%a\"}@]"
    Printers.pp_var_name v
    pp_var_type v
    
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
     
let pp_emf_cst_or_var fmt v =
  match v.value_desc with
  | Cst ((Const_tag t) as c)->
     let typ = (Corelang.typedef_of_top (Hashtbl.find Corelang.tag_table t)) in
     if typ.tydef_id = "bool" then (
       fprintf fmt "{@[\"type\": \"constant\",@ ";
       fprintf fmt"\"value\": \"%a\",@ "
	 Printers.pp_const c;
       fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type v;
       fprintf fmt "@]}"
     )
     else (
       fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\",@ " 
	 pp_tag_id t;
       fprintf fmt "\"origin_type\": \"%s\",@ \"origin_value\": \"%s\",@ "
	 typ.tydef_id t;
       fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type v;
       fprintf fmt "@]}"
     )
  | Cst c -> (
    fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\",@ "
      Printers.pp_const c;
    fprintf fmt "\"datatype\": \"%a\"@ " pp_cst_type v;
    fprintf fmt "@]}"
  )
  | LocalVar v
  | StateVar v -> (
    fprintf fmt "{@[\"type\": \"variable\",@ \"value\": \"%a\",@ "
      Printers.pp_var_name v;
    fprintf fmt "\"datatype\": \"%a\"@ " pp_var_type v;
    fprintf fmt "@]}"
  )
  | _ -> Format.eprintf "Not of cst or var: %a@." Machine_code.pp_val v ; assert false (* Invalid argument *)


let pp_emf_cst_or_var_list =
  Utils.fprintf_list ~sep:",@ " pp_emf_cst_or_var


(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
