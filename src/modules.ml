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
open Lustre_types
open Corelang

let add_symbol loc msg hashtbl name value =
 if Hashtbl.mem hashtbl name
 then raise (Error (loc, Error.Already_bound_symbol msg))
 else Hashtbl.add hashtbl name value

let check_symbol loc msg hashtbl name =
 if not (Hashtbl.mem hashtbl name)
 then raise (Error (loc, Error.Unbound_symbol msg))
 else ()

let add_imported_node name value =
(*Format.eprintf "add_imported_node %s %a (owner=%s)@." name Printers.pp_imported_node (imported_node_of_top value) value.top_decl_owner;*)
  try
    let value' = Hashtbl.find node_table name in
    let owner' = value'.top_decl_owner in
    let itf' = value'.top_decl_itf in
    let owner = value.top_decl_owner in
    let itf = value.top_decl_itf in
    match value'.top_decl_desc, value.top_decl_desc with
    | Node _        , ImportedNode _  when owner = owner' && itf' && (not itf) -> Hashtbl.add node_table name value
    | ImportedNode _, ImportedNode _            -> raise (Error (value.top_decl_loc, Error.Already_bound_symbol ("node " ^ name)))
    | _                                         -> assert false
  with
    Not_found                                   -> Hashtbl.add node_table name value

let add_node name value =
(*Format.eprintf "add_node %s %a (owner=%s)@." name Printers.pp_imported_node (get_node_interface (node_of_top value)) value.top_decl_owner;*)
  try
    let value' = Hashtbl.find node_table name in
    let owner' = value'.top_decl_owner in
    let itf' = value'.top_decl_itf in
    let owner = value.top_decl_owner in
    let itf = value.top_decl_itf in
    match value'.top_decl_desc, value.top_decl_desc with
    | ImportedNode _, Node _          when owner = owner' && itf' && (not itf) -> ()
    | Node _        , Node _                    -> raise (Error (value.top_decl_loc, Error.Already_bound_symbol ("node " ^ name)))
    | _                                         -> assert false
  with
    Not_found                                   -> Hashtbl.add node_table name value


let add_tag loc name typ =
  if Hashtbl.mem tag_table name then
    raise (Error (loc, Error.Already_bound_symbol ("enum tag " ^ name)))
  else Hashtbl.add tag_table name typ

let add_field loc name typ =
  if Hashtbl.mem field_table name then
    raise (Error (loc, Error.Already_bound_symbol ("struct field " ^ name)))
  else Hashtbl.add field_table name typ

let import_typedef name tydef =
  let loc = tydef.top_decl_loc in
  let rec import ty =
    match ty with
    | Tydec_enum tl   ->
       List.iter (fun tag -> add_tag loc tag tydef) tl
    | Tydec_struct fl -> 
       List.iter (fun (field, ty) -> add_field loc field tydef; import ty) fl
    | Tydec_clock ty      -> import ty
    | Tydec_const c       ->
       if not (Hashtbl.mem type_table (Tydec_const c))
       then raise (Error (loc, Error.Unbound_symbol ("type " ^ c)))
       else ()
    | Tydec_array (c, ty) -> import ty
    | _                   -> ()
  in import ((typedef_of_top tydef).tydef_desc)

let add_type itf name value =
(*Format.eprintf "Modules.add_type %B %s %a (owner=%s)@." itf name Printers.pp_typedef (typedef_of_top value) value.top_decl_owner;*)
  try
    let value' = Hashtbl.find type_table (Tydec_const name) in
    let owner' = value'.top_decl_owner in
    let itf' = value'.top_decl_itf in
    let owner = value.top_decl_owner in
    let itf = value.top_decl_itf in
    match value'.top_decl_desc, value.top_decl_desc with
    | TypeDef ty', TypeDef ty when coretype_equal ty'.tydef_desc ty.tydef_desc && owner' = owner && itf' && (not itf) -> ()
    | TypeDef ty', TypeDef ty -> raise (Error (value.top_decl_loc, Error.Already_bound_symbol ("type " ^ name)))
    | _       -> assert false
  with Not_found -> (import_typedef name value; Hashtbl.add type_table (Tydec_const name) value)

let check_type loc name =
 if not (Hashtbl.mem type_table (Tydec_const name))
 then raise (Error (loc, Error.Unbound_symbol ("type " ^ name)))
 else ()

let add_const itf name value =
  try
    let value' = Hashtbl.find consts_table name in
    let owner' = value'.top_decl_owner in
    let itf' = value'.top_decl_itf in
    let owner = value.top_decl_owner in
    let itf = value.top_decl_itf in
    match value'.top_decl_desc, value.top_decl_desc with
    | Const c', Const c when c.const_value = c'.const_value && owner' = owner && itf' && (not itf) -> ()
    | Const c', Const c -> raise (Error (value.top_decl_loc, Error.Already_bound_symbol ("const " ^ name)))
    | _       -> assert false
  with Not_found -> Hashtbl.add consts_table name value

let import_dependency_aux loc (local, dep) =
  let basename = Options_management.name_dependency (local, dep) in
  let extension = ".lusic" in 
  try
    let lusic = Lusic.read_lusic basename extension in
    Lusic.check_obsolete lusic basename;
    lusic
  with
  | Sys_error msg ->
    begin
      (*Format.eprintf "Error: %s@." msg;*)
      raise (Error (loc, Error.Unknown_library basename))
    end
  | Corelang.Error (_, msg) -> raise (Corelang.Error (loc, msg))

let import_dependency loc (local, dep) =
  try
    import_dependency_aux loc (local, dep)
  with
  | Corelang.Error (_, err) as exc -> (
    Format.eprintf "Import error: %a%a@."
      Error.pp_error_msg err
      Location.pp_loc loc;
    raise exc
  )

let check_dependency lusic basename =
  try
    Lusic.check_obsolete lusic basename
  with
  | Corelang.Error (loc, err) as exc -> (
    Format.eprintf "Import error: %a%a@."
      Error.pp_error_msg err
      Location.pp_loc loc;
    raise exc
  )

let rec load_header_rec imported header =
  List.fold_left (fun imported decl ->
    match decl.top_decl_desc with
    | Node nd -> assert false
    | ImportedNode ind -> (add_imported_node ind.nodei_id decl; imported)
    | Const c -> (add_const true c.const_id decl; imported)
    | TypeDef tdef -> (add_type true tdef.tydef_id decl; imported)
    | Open (local, dep) ->
       let basename = Options_management.name_dependency (local, dep) in
       if ISet.mem basename imported then imported else
	 let lusic = import_dependency_aux decl.top_decl_loc (local, dep)
	 in load_header_rec (ISet.add basename imported) lusic.Lusic.contents
  ) imported header

let load_header imported header =
  try
    load_header_rec imported header
  with
    Corelang.Error (loc, err) as exc -> (
      Format.eprintf "Import error: %a%a@."
	Error.pp_error_msg err
	Location.pp_loc loc;
      raise exc
    );;

let rec load_program_rec imported program =
  List.fold_left (fun imported decl ->
    match decl.top_decl_desc with
    | Node nd -> (add_node nd.node_id decl; imported)
    | ImportedNode ind -> assert false
    | Const c -> (add_const false c.const_id decl; imported)
    | TypeDef tdef -> (add_type false tdef.tydef_id decl; imported)
    | Open (local, dep) ->
       let basename = Options_management.name_dependency (local, dep) in
       if ISet.mem basename imported then imported else
	 let lusic = import_dependency_aux decl.top_decl_loc (local, dep)
	 in load_header_rec (ISet.add basename imported) lusic.Lusic.contents
  ) imported program
    
let load_program imported program =
  try
    load_program_rec imported program
  with
    Corelang.Error (loc, err) as exc -> (
      Format.eprintf "Import error: %a%a@."
	Error.pp_error_msg err
	Location.pp_loc loc;
      raise exc
    );;
