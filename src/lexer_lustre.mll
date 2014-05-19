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

{
open Parser_lustre
open Utils

exception Error of Location.t

(* As advised by Caml documentation. This way a single lexer rule is
   used to handle all the possible keywords. *)
let keyword_table =
  create_hashtable 20 [
  "function", FUNCTION;
  "struct", STRUCT;
  "enum", ENUM;
  "automaton", AUTOMATON;
  "state", STATE;
  "until", UNTIL;
  "unless", UNLESS;
  "last", LAST;
  "resume", RESUME;
  "restart", RESTART;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "merge", MERGE;
  "arrow", ARROW;
  "fby", FBY;
  "when", WHEN;
  "whenot", WHENNOT;
  "every", EVERY;
  "node", NODE;
  "let", LET;
  "tel", TEL;
  "returns", RETURNS;
  "var", VAR;
  "imported", IMPORTED;
  "wcet", WCET;
  "type", TYPE;
  "int", TINT;
  "bool", TBOOL;
  "float", TFLOAT;
  "real", TREAL;
  "clock", TCLOCK;
  "not", NOT;
  "tail", TAIL;
  "and", AND;
  "or", OR;
  "xor", XOR;
  "mod", MOD;
  "pre", PRE;
  "div", DIV;
  "const", CONST;
  "assert", ASSERT;
  "lib", LIB;
  "prototype", PROTOTYPE;
]


(* Buffer for parsing specification/annotation *)
let buf = Buffer.create 1024

let make_annot lexbuf s = 
  try
    let ann = LexerLustreSpec.annot s in
    ANNOT ann
  with _ -> (Format.eprintf "Impossible to parse the following annotation:@.%s@.@?" s; exit 1)

let make_spec lexbuf s = 
  try
    let ns = LexerLustreSpec.spec s in
    NODESPEC ns
  with _ -> (Format.eprintf "Impossible to parse the following node specification:@.%s@.@?" s; exit 1)
   
}

let newline = ('\010' | '\013' | "\013\010")
let notnewline = [^ '\010' '\013']
let blank = [' ' '\009' '\012']

rule token = parse
| "--@" { Buffer.clear buf;
	  spec_singleline lexbuf }
| "(*@" { Buffer.clear buf; 
	  spec_multiline 0 lexbuf }
| "--!" { Buffer.clear buf; 
	  annot_singleline lexbuf }
| "(*!" { Buffer.clear buf; 
	  annot_multiline 0 lexbuf }
| "(*"
    { comment 0 lexbuf }
| "--" [^ '!' '@'] notnewline* (newline|eof)
    { incr_line lexbuf;
      token lexbuf }
| newline
    { incr_line lexbuf;
      token lexbuf }
| blank +
    {token lexbuf}
| ['0'-'9'] ['0'-'9']* '.' ['0'-'9']*
    {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
| ['0'-'9']+ 
    {INT (int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']+ '.' ['0'-'9']+ ('E'|'e') ('+'|'-') ['0'-'9'] ['0'-'9']* as s {REAL s}
| "tel." {TEL}
| "tel;" {TEL}
| "#open" { OPEN }
| ['_' 'a'-'z' 'A'-'Z'] [ '_' 'a'-'z' 'A'-'Z' '0'-'9']*
    {let s = Lexing.lexeme lexbuf in
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      IDENT s}
| "->" {ARROW}
| "=>" {IMPL}
| "<=" {LTE}
| ">=" {GTE}
| "<>" {NEQ}
| '<' {LT}
| '>' {GT}
| "!=" {NEQ}
| '-' {MINUS}
| '+' {PLUS}
| '/' {DIV}
| '*' {MULT}
| '=' {EQ}
| '(' {LPAR}
| ')' {RPAR}
| '[' {LBRACKET}
| ']' {RBRACKET}
| '{' {LCUR}
| '}' {RCUR}
| ';' {SCOL}
| ':' {COL}
| ',' {COMMA}
| '=' {EQ}
| '/' {DIV}
| "&&" {AMPERAMPER}
| "||" {BARBAR}
| "::" {COLCOL}
| "^" {POWER}
| '"' {QUOTE}
| eof { EOF }
| _ { raise (Error (Location.curr lexbuf)) }

and comment n = parse
| eof
    { raise (Error (Location.curr lexbuf)) }
| "(*"
    { comment (n+1) lexbuf }
| "*)"
    { if n > 0 then comment (n-1) lexbuf else token lexbuf }
| newline
    { incr_line lexbuf;
      comment n lexbuf }
| _ { comment n lexbuf }

and annot_singleline = parse
  | newline { incr_line lexbuf; make_annot lexbuf (Buffer.contents buf) }
  | _ as c { Buffer.add_char buf c; annot_singleline lexbuf }

and annot_multiline n = parse
  | "*)" as s { 
    if n > 0 then 
      (Buffer.add_string buf s; annot_multiline (n-1) lexbuf) 
    else 
      make_annot lexbuf (Buffer.contents buf) }
  | "(*" as s { Buffer.add_string buf s; annot_multiline (n+1) lexbuf }
  | newline as s { incr_line lexbuf; Buffer.add_string buf s; annot_multiline n lexbuf }
  | _ as c { Buffer.add_char buf c; annot_multiline n lexbuf }

and spec_singleline = parse
  | newline { incr_line lexbuf; make_spec lexbuf (Buffer.contents buf) }
  | _ as c { Buffer.add_char buf c; spec_singleline lexbuf }

and spec_multiline n = parse
  | "*)" as s { if n > 0 then 
      (Buffer.add_string buf s; spec_multiline (n-1) lexbuf) 
    else 
      make_spec lexbuf (Buffer.contents buf) }
  | "(*" as s { Buffer.add_string buf s; spec_multiline (n+1) lexbuf }
  | newline as s { incr_line lexbuf; Buffer.add_string buf s; spec_multiline n lexbuf }
  | _ as c { Buffer.add_char buf c; spec_multiline n lexbuf }

