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
open Parser_lustre
open Utils

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
  (* "float", TFLOAT; *)
  "real", TREAL;
  "clock", TCLOCK;
  "not", NOT;
  "tail", TAIL;
  "true", TRUE;
  "false", FALSE;
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
  "c_code", CCODE; (* not sure how it is used *)
  "matlab", MATLAB; (* same as above *)
]


(* Buffer for parsing specification/annotation *)
let buf = Buffer.create 1024

let make_annot lexbuf s = 
  try
    let ann = LexerLustreSpec.annot s in
    ANNOT ann
  with LexerLustreSpec.Error loc -> raise (Parse.Error (Location.shift (Location.curr lexbuf) loc, Parse.Annot_error s))

let make_spec lexbuf s = 
  try
    let ns = LexerLustreSpec.spec s in
    NODESPEC ns
  with LexerLustreSpec.Error loc -> raise (Parse.Error (Location.shift (Location.curr lexbuf) loc, Parse.Node_spec_error s))

(*
let make_kind_spec lexbuf s =
    let s_lexbuf = Lexing.from_string s in
    let _ = KindLustreParser.contract_in_block KindLustreLexer.token s_lexbuf in
    let dummy_ns = { Lustre_types.requires = []; ensures = []; behaviors = []; spec_loc = Location.dummy_loc} in
    NODESPEC dummy_ns

let make_spec = make_kind_spec*)
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
| ((['0'-'9']+ as l)  '.' (['0'-'9']* as r) ('E'|'e') (('+'|'-')? ['0'-'9']+ as exp)) as s
    {REAL (Num.num_of_string (l^r), String.length r + -1 * int_of_string exp , s)}
| ((['0'-'9']+ as l) '.' (['0'-'9']* as r)) as s
    {REAL (Num.num_of_string (l^r), String.length r, s)}
| ['0'-'9']+ 
    {INT (int_of_string (Lexing.lexeme lexbuf)) }
| "tel." {TEL}
| "tel;" {TEL}
| "#open" { OPEN }
| ['_' 'a'-'z'] [ '_' 'a'-'z' 'A'-'Z' '0'-'9']*
    {let s = Lexing.lexeme lexbuf in
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      IDENT s}
| ['A'-'Z'] [ '_' 'a'-'z' 'A'-'Z' '0'-'9']*
    {let s = Lexing.lexeme lexbuf in
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      UIDENT s}
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
| _ { raise (Parse.Error (Location.curr lexbuf, Parse.Undefined_token (Lexing.lexeme lexbuf))) }

and comment n = parse
| eof
    { raise (Parse.Error (Location.curr lexbuf, Parse.Unfinished_comment)) }
| "(*"
    { comment (n+1) lexbuf }
| "*)"
    { if n > 0 then comment (n-1) lexbuf else token lexbuf }
| newline
    { incr_line lexbuf;
      comment n lexbuf }
| _ { comment n lexbuf }

and annot_singleline = parse
  | eof { make_annot lexbuf (Buffer.contents buf) }
  | newline { incr_line lexbuf; make_annot lexbuf (Buffer.contents buf) }
  | _ as c { Buffer.add_char buf c; annot_singleline lexbuf }

and annot_multiline n = parse
  | eof { raise (Parse.Error (Location.curr lexbuf, Parse.Unfinished_annot)) }
  | "*)" as s { 
    if n > 0 then 
      (Buffer.add_string buf s; annot_multiline (n-1) lexbuf) 
    else 
      make_annot lexbuf (Buffer.contents buf) }
  | "(*" as s { Buffer.add_string buf s; annot_multiline (n+1) lexbuf }
  | newline as s { incr_line lexbuf; Buffer.add_string buf s; annot_multiline n lexbuf }
  | _ as c { Buffer.add_char buf c; annot_multiline n lexbuf }

and spec_singleline = parse
  | eof { make_spec lexbuf (Buffer.contents buf) }
  | newline { incr_line lexbuf; make_spec lexbuf (Buffer.contents buf) }
  | _ as c { Buffer.add_char buf c; spec_singleline lexbuf }

and spec_multiline n = parse
  | eof { raise (Parse.Error (Location.curr lexbuf, Parse.Unfinished_node_spec)) }
  | "*)" as s { if n > 0 then 
      (Buffer.add_string buf s; spec_multiline (n-1) lexbuf) 
    else 
      make_spec lexbuf (Buffer.contents buf) }
  | "(*" as s { Buffer.add_string buf s; spec_multiline (n+1) lexbuf }
  | newline as s { incr_line lexbuf; Buffer.add_string buf s; spec_multiline n lexbuf }
  | _ as c { Buffer.add_char buf c; spec_multiline n lexbuf }

