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

exception Syntax_err of Location.t

open Format
open LustreSpec
open Corelang

let add_symbol loc msg hashtbl name value =
 if Hashtbl.mem hashtbl name
 then raise (Error (loc, Already_bound_symbol msg))
 else Hashtbl.add hashtbl name value

let check_symbol loc msg hashtbl name =
 if not (Hashtbl.mem hashtbl name)
 then raise (Error (loc, Unbound_symbol msg))
 else ()

let add_node own name value =
  try
    match (Hashtbl.find node_table name).top_decl_desc, value.top_decl_desc with
    | Node _        , ImportedNode _ when own   -> ()
    | ImportedNode _, _                         -> Hashtbl.add node_table name value
    | Node _        , _                         -> raise (Error (value.top_decl_loc, Already_bound_symbol ("node " ^ name)))
    | _                                         -> assert false
  with
    Not_found                                   -> Hashtbl.add node_table name value


let add_tag loc own name typ =
  if Hashtbl.mem tag_table name && (not own) then
    raise (Error (loc, Unbound_symbol ("enum tag " ^ name)))
  else Hashtbl.add tag_table name typ

let add_field loc own name typ =
  if Hashtbl.mem field_table name && (not own) then
    raise (Error (loc, Unbound_symbol ("struct field " ^ name)))
  else Hashtbl.add field_table name typ

let rec check_type_def loc own name ty =
  match ty with
  | Tydec_enum tl   ->
    begin
      List.iter (fun tag -> add_tag loc own tag (Tydec_const name)) tl;
      ty
    end
  | Tydec_struct fl -> 
    begin
      List.iter (fun (field, _) -> add_field loc own field (Tydec_const name)) fl;
      Tydec_struct (List.map (fun (f, ty) -> (f, check_type_def loc own name ty)) fl)
    end
  | Tydec_clock ty      -> Tydec_clock (check_type_def loc own name ty)
  | Tydec_const c       ->
    if not (Hashtbl.mem type_table (Tydec_const c))
    then raise (Error (loc, Unbound_symbol ("type " ^ c)))
    else get_repr_type ty
  | Tydec_array (c, ty) -> Tydec_array (c, check_type_def loc own name ty)
  | _                   -> ty

let add_type own name value =
(*Format.eprintf "add_type %B %s@." own name;*)
  match value.top_decl_desc with
  | Type ty ->
    let loc = value.top_decl_loc in
    if Hashtbl.mem type_table (Tydec_const name) && (not own)
    then raise (Error (loc, Already_bound_symbol ("type " ^ name)))
    else Hashtbl.add type_table (Tydec_const name) (check_type_def loc own name ty.ty_def_desc)
  | _       -> assert false

let check_type loc name =
 if not (Hashtbl.mem type_table (Tydec_const name))
 then raise (Error (loc, Unbound_symbol ("type " ^ name)))
 else ()

let report_error loc =
  Location.print loc;
  print_string "Syntax error\n"
(*
let wrap own parsing_fun token_fun lexbuf =
  try
    let ast = parsing_fun token_fun lexbuf own in
    Parsing.clear_parser ();
    ast
  with
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Syntax_err loc)
 *)
let header own parsing_fun token_fun lexbuf =
  try
    let ast = parsing_fun token_fun lexbuf own in
    Parsing.clear_parser ();
    ast
  with
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Syntax_err loc)

let prog parsing_fun token_fun lexbuf =
  try
    let ast = parsing_fun token_fun lexbuf in
    Parsing.clear_parser ();
    ast
  with
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Syntax_err loc)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
