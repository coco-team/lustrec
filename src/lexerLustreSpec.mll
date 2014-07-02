{

  (* open ParserLustreSpec *)
  open Parser_lustre
  open Utils

  let str_buf = Buffer.create 1024

  exception Error of Location.t

(* As advised by Caml documentation. This way a single lexer rule is
   used to handle all the possible keywords. *)
let keyword_table =
  create_hashtable 20 [
  (* "true", TRUE; *)
  (* "false", FALSE; *)
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
  "not", NOT;
  "tail", TAIL;
  "and", AND;
  "or", OR;
  "xor", OR;
  "mod", MOD;
  "pre", PRE;
  "div", DIV;
  "const", CONST;
  (* "include", INCLUDE; *)
  "assert", ASSERT;
  "ensures", ENSURES;
  "requires", REQUIRES;
  "observer", OBSERVER;
  "invariant", INVARIANT;
  "behavior", BEHAVIOR;
  "assumes", ASSUMES;
  "exists", EXISTS;
  "forall", FORALL;
  ]

}


let newline = ('\010' | '\013' | "\013\010")
let notnewline = [^ '\010' '\013']
let blank = [' ' '\009' '\012']

rule token = parse
  | "(*"
      { comment_line 0 lexbuf }
  | "--" notnewline* (newline|eof)
      { incr_line lexbuf;
      token lexbuf }
  | newline
      { incr_line lexbuf;
	token lexbuf }
  | blank +
      {token lexbuf}
  | '-'? ['0'-'9'] ['0'-'9']* '.' ['0'-'9']*
      {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | '-'? ['0'-'9']+ 
      {INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '-'? ['0'-'9']+ '.' ['0'-'9']+ ('E'|'e') ('+'|'-') ['0'-'9'] ['0'-'9'] as s {REAL s}
 (* | '/' (['_' 'A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* '/')+ as s
      {IDENT s}
 *)
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
  | "::" {COLCOL}
  | "^" {POWER}
  | '"' { Buffer.clear str_buf; string_parse lexbuf }
  | eof { EOF }
  | _ { raise (Error (Location.curr lexbuf)) }
and comment_line n = parse
| eof
    { raise (Error (Location.curr lexbuf)) }
| "(*"
    { comment_line (n+1) lexbuf }
| "*)"
    { if n > 0 then comment_line (n-1) lexbuf else token lexbuf }
| newline
    { incr_line lexbuf;
      comment_line n lexbuf }
| _ { comment_line n lexbuf }
and string_parse = parse
  | "\\\"" as s { Buffer.add_string str_buf s; string_parse lexbuf}
  | '"' { STRING (Buffer.contents str_buf) }
  | _ as c  { Buffer.add_char str_buf c; string_parse lexbuf }

{

  let annot s =
    let lb = Lexing.from_string s in
   try
     Parser_lustre.lustre_annot(* ParserLustreSpec.lustre_annot *) token lb
   with Parsing.Parse_error as _e -> (
     Format.eprintf "Lexing error at position %a:@.unexpected token %s@.@?"
       (fun fmt p -> Format.fprintf fmt "%s l%i c%i" p.Lexing.pos_fname p.Lexing.pos_lnum p.Lexing.pos_cnum) lb.Lexing.lex_curr_p
       (Lexing.lexeme lb);
     raise Parsing.Parse_error)
     

  let spec s =
    let lb = Lexing.from_string s in
    Parser_lustre.lustre_spec (*ParserLustreSpec.lustre_spec*) token lb

}
