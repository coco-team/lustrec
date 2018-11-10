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
open Format
open Lustre_types
open Corelang

type error =
  | Undefined_token of string
  | Unexpected_eof
  | Unfinished_string
  | Unfinished_comment
  | Syntax_error
  | String_Syntax_error of string
  | Unfinished_annot
  | Unfinished_node_spec 
  | Annot_error of string
  | Node_spec_error of string

exception Error of (Location.t * error)


let pp_error fmt err =
  match err with
  | Unexpected_eof          -> fprintf fmt "unexpected end of file"
  | Undefined_token tok   -> fprintf fmt "undefined token '%s'" tok
  | Unfinished_string        -> fprintf fmt "unfinished string"
  | Unfinished_comment  -> fprintf fmt "unfinished comment"
  | Syntax_error               -> fprintf fmt "syntax error"
  | String_Syntax_error s              -> fprintf fmt "syntax error in %s" s
  | Unfinished_annot        -> fprintf fmt "unfinished annotation"
  | Unfinished_node_spec -> fprintf fmt "unfinished node specification"
  | Annot_error s              -> fprintf fmt "impossible to parse the following annotation:@.%s@.@?" s
  | Node_spec_error s       -> fprintf fmt "Impossible to parse the following node specification:@.%s@.@?" s

let report_error (loc, err) =
  eprintf "Syntax error: %a%a@."
    pp_error err
    Location.pp_loc loc

let header parsing_fun token_fun lexbuf =
  try
    let ast = parsing_fun token_fun lexbuf in
    Parsing.clear_parser ();
    ast
  with
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Error (loc, Syntax_error))

let prog parsing_fun token_fun lexbuf =
  try
    let ast = parsing_fun token_fun lexbuf in
    Parsing.clear_parser ();
    ast
  with
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Error (loc, Syntax_error))

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
