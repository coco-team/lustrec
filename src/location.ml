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

type t = { loc_start: Lexing.position; loc_end: Lexing.position }
let dummy_loc = {loc_start=Lexing.dummy_pos; loc_end=Lexing.dummy_pos}

let input_name = ref ""

let curr lexbuf = {
  loc_start = lexbuf.Lexing.lex_start_p;
  loc_end = lexbuf.Lexing.lex_curr_p
}

let init lexbuf fname =
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = fname;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  }
      
let symbol_rloc () = 
  {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ()
  }
    

open Format

let print loc =
  let filename = loc.loc_start.Lexing.pos_fname in
  let line = loc.loc_start.Lexing.pos_lnum in
  let start_char =
    loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol
  in
  let end_char =
    loc.loc_end.Lexing.pos_cnum - loc.loc_start.Lexing.pos_cnum + start_char
  in
  let (start_char, end_char) =
    if start_char < 0 then (0,1) else (start_char, end_char)
  in
  print_string ("File \""^filename^"\", line ");
  print_int line;
  print_string ", characters ";
  print_int start_char;
  print_string "-";
  print_int end_char;
  print_string ":";
  print_newline ()


let pp_loc fmt loc =
  let filename = loc.loc_start.Lexing.pos_fname in
  let line = loc.loc_start.Lexing.pos_lnum in
  let start_char =
    loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol
  in
  let end_char =
    loc.loc_end.Lexing.pos_cnum - loc.loc_start.Lexing.pos_cnum + start_char
  in
  let (start_char, end_char) =
    if start_char < 0 then (0,1) else (start_char, end_char)
  in
  Format.fprintf fmt "File \"%s\", line %i, characters %i-%i:" filename line start_char end_char

let pp_c_loc fmt loc =
  let filename = loc.loc_start.Lexing.pos_fname in
  let line = loc.loc_start.Lexing.pos_lnum in
  Format.fprintf fmt "#line %i \"%s\"" line filename

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
