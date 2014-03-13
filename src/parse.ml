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
