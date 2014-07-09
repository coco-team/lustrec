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
