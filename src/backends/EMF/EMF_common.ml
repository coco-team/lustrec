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

let pp_emf_var_decl fmt v =
  fprintf fmt "@[{\"name\": \"%a\", \"type\":\"%a\"}@]"
    Printers.pp_var_name v
    Printers.pp_var_type v
    
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
     if typ.tydef_id = "bool" then
       fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\"@ @]}"
	 Printers.pp_const c
     else (
       fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\",@ " 
	 pp_tag_id t;
       fprintf fmt "\"origin_type\": \"%s\",@ \"origin_value\": \"%s\"@ "
	 typ.tydef_id t;
       fprintf fmt "@]}"
     )
  | Cst c ->
     fprintf fmt "{@[\"type\": \"constant\",@ \"value\": \"%a\"@ @]}"
       Printers.pp_const c
  | LocalVar v
  | StateVar v ->
     fprintf fmt "{@[\"type\": \"variable\",@ \"value\": \"%a\"@ @]}"
       Printers.pp_var_name v
  | _ -> assert false (* Invalid argument *)


let pp_emf_cst_or_var_list =
  Utils.fprintf_list ~sep:",@ " pp_emf_cst_or_var


(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
