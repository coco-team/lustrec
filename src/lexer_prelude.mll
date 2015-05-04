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

{
open Parser_prelude
open Utils

exception Error of Location.t

(* As advised by Caml documentation. This way a single lexer rule is
   used to handle all the possible keywords. *)
let keyword_table =
  create_hashtable 20 [
  "true", TRUE;
  "false", FALSE;
  "stateless", STATELESS;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "merge", MERGE;
  "arrow", ARROW;
  "fby", FBY;
  "when", WHEN;
  "whennot", WHENNOT;
  "every", EVERY;
  "node", NODE;
  "sensor", SENSOR;
  "actuator", ACTUATOR;
  "let", LET;
  "tel", TEL;
  "returns", RETURNS;
  "var", VAR;
  "imported", IMPORTED;
  "wcet", WCET;
  "int", TINT;
  "bool", TBOOL;
  "float", TFLOAT;
  "real", TREAL;
  "clock", TCLOCK;
  "rate", RATE;
  "due", DUE;
  "not", NOT;
  "tail", TAIL;
  "and", AND;
  "or", OR;
  "xor", OR;
  "mod", MOD;
  "pre", PRE;
  "div", DIV;
  "const", CONST;
  "include", INCLUDE
]

(* Update line number for location info *)
let incr_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let newline = ('\010' | '\013' | "\013\010")
let notnewline = [^ '\010' '\013']
let blank = [' ' '\009' '\012']

rule token = parse
|  "/*"
    { comment 0 lexbuf }
| "--" notnewline* (newline|eof)
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
| ['0'-'9']+ '.' ['0'-'9']+ 'E' ('+'|'-') ['0'-'9'] ['0'-'9'] as s {REAL s}
| "tel." {TEL}
| "tel;" {TEL}
| ['_' 'A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
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
| ';' {SCOL}
| ':' {COL}
| ',' {COMMA}
| '=' {EQ}
| '/' {DIV}
| "&&" {AMPERAMPER}
| "||" {BARBAR}
| "*^" {UCLOCK}
| "/^" {DCLOCK}
| "~>" {PHCLOCK}
| "::" {COLCOL}
| "^" {POWER}
| '"' {QUOTE}
| eof { EOF }
| _ { raise (Error (Location.curr lexbuf)) }
and comment n = parse
| eof
    { raise (Error (Location.curr lexbuf)) }
| "/*"
    { comment (n+1) lexbuf }
| "*/"
    { if n > 0 then comment (n-1) lexbuf else token lexbuf }
| newline
    { incr_line lexbuf;
      comment n lexbuf }
| _ { comment n lexbuf }
