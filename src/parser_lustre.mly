/* ----------------------------------------------------------------------------
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
 *---------------------------------------------------------------------------- */

%{
open LustreSpec
open Corelang
open Dimension
open Utils

let get_loc () = Location.symbol_rloc ()
let mktyp x = mktyp (get_loc ()) x
let mkclock x = mkclock (get_loc ()) x
let mkvar_decl x = mkvar_decl (get_loc ()) x
let mkexpr x = mkexpr (get_loc ()) x
let mkeexpr x = mkeexpr (get_loc ()) x 
let mkeq x = mkeq (get_loc ()) x
let mkassert x = mkassert (get_loc ()) x
let mktop_decl x = mktop_decl (get_loc ()) x
let mkpredef_call x = mkpredef_call (get_loc ()) x
(*let mkpredef_unary_call x = mkpredef_unary_call (get_loc ()) x*)

let mkdim_int i = mkdim_int (get_loc ()) i
let mkdim_bool b = mkdim_bool (get_loc ()) b
let mkdim_ident id = mkdim_ident (get_loc ()) id
let mkdim_appl f args = mkdim_appl (get_loc ()) f args
let mkdim_ite i t e = mkdim_ite (get_loc ()) i t e

let mkannots annots = { annots = annots; annot_loc = get_loc () }

let add_node loc own msg hashtbl name value =
  try
    match (Hashtbl.find hashtbl name).top_decl_desc, value.top_decl_desc with
    | Node _        , ImportedNode _ when own   -> ()
    | ImportedNode _, _                         -> Hashtbl.add hashtbl name value
    | Node _        , _                         -> raise (Error (loc, Already_bound_symbol msg))
    | _                                         -> assert false
  with
    Not_found                                   -> Hashtbl.add hashtbl name value


let add_symbol loc msg hashtbl name value =
 if Hashtbl.mem hashtbl name
 then raise (Error (loc, Already_bound_symbol msg))
 else Hashtbl.add hashtbl name value

let check_symbol loc msg hashtbl name =
 if not (Hashtbl.mem hashtbl name)
 then raise (Error (loc, Unbound_symbol msg))
 else ()

let check_node_symbol msg name value =
 if Hashtbl.mem node_table name
 then () (* TODO: should we check the types here ? *)
 else Hashtbl.add node_table name value

%}

%token <int> INT
%token <string> REAL
%token <float> FLOAT
%token <string> STRING
%token AUTOMATON STATE UNTIL UNLESS RESTART RESUME LAST
%token STATELESS ASSERT OPEN QUOTE FUNCTION
%token <string> IDENT
%token <LustreSpec.expr_annot> ANNOT
%token <LustreSpec.node_annot> NODESPEC
%token LBRACKET RBRACKET LCUR RCUR LPAR RPAR SCOL COL COMMA COLCOL 
%token AMPERAMPER BARBAR NOT POWER
%token IF THEN ELSE
%token UCLOCK DCLOCK PHCLOCK TAIL
%token MERGE FBY WHEN WHENNOT EVERY
%token NODE LET TEL RETURNS VAR IMPORTED SENSOR ACTUATOR WCET TYPE CONST
%token STRUCT ENUM
%token TINT TFLOAT TREAL TBOOL TCLOCK
%token RATE DUE
%token EQ LT GT LTE GTE NEQ
%token AND OR XOR IMPL
%token MULT DIV MOD
%token MINUS PLUS UMINUS
%token PRE ARROW
%token REQUIRES ENSURES OBSERVER
%token INVARIANT BEHAVIOR ASSUMES
%token EXISTS FORALL
%token PROTOTYPE LIB
%token EOF

%nonassoc prec_exists prec_forall
%nonassoc COMMA
%left MERGE IF
%nonassoc ELSE
%right ARROW FBY
%left WHEN WHENNOT UCLOCK DCLOCK PHCLOCK
%right COLCOL
%right IMPL
%left OR XOR BARBAR
%left AND AMPERAMPER
%left NOT
%nonassoc INT
%nonassoc EQ LT GT LTE GTE NEQ
%left MINUS PLUS
%left MULT DIV MOD
%left UMINUS
%left POWER
%left PRE LAST
%nonassoc RBRACKET
%nonassoc LBRACKET

%start prog
%type <LustreSpec.top_decl list> prog

%start header
%type <bool -> LustreSpec.top_decl list> header

%start lustre_annot
%type <LustreSpec.expr_annot> lustre_annot

%start lustre_spec
%type <LustreSpec.node_annot> lustre_spec

%%

prog:
 open_list typ_def_list top_decl_list EOF { $1 @ (List.rev $3) }

header:
 open_list typ_def_list top_decl_header_list EOF { (fun own -> ($1 @ (List.rev ($3 own)))) }

open_list:
  { [] }
| open_lusi open_list { $1 :: $2 }

open_lusi:
| OPEN QUOTE IDENT QUOTE { mktop_decl (Open (true, $3))}
| OPEN LT IDENT GT { mktop_decl (Open (false, $3)) }

top_decl_list:
  top_decl {[$1]}
| top_decl_list top_decl {$2::$1}


top_decl_header_list:
  top_decl_header {(fun own -> [$1 own]) }
| top_decl_header_list top_decl_header {(fun own -> ($2 own)::($1 own)) }

state_annot:
  FUNCTION { true }
| NODE { false }

top_decl_header:
| CONST cdecl_list { let top = mktop_decl (Consts (List.rev $2)) in fun _ -> top }
| nodespec_list state_annot IDENT LPAR vdecl_list SCOL_opt RPAR RETURNS LPAR vdecl_list SCOL_opt RPAR  prototype_opt in_lib_opt SCOL
    {let nd = mktop_decl (ImportedNode
                            {nodei_id = $3;
                             nodei_type = Types.new_var ();
                             nodei_clock = Clocks.new_var true;
                             nodei_inputs = List.rev $5;
                             nodei_outputs = List.rev $10;
			     nodei_stateless = $2;
			     nodei_spec = $1;
			     nodei_prototype = $13;
			     nodei_in_lib = $14;})
    in
     check_node_symbol ("node " ^ $3) $3 nd; 
     let loc = get_loc () in
     (fun own -> add_node loc own ("node " ^ $3) node_table $3 nd; nd) }

prototype_opt:
 { None }
| PROTOTYPE IDENT { Some $2}

in_lib_opt:
{ None }
| LIB IDENT {Some $2} 

top_decl:
| CONST cdecl_list { mktop_decl (Consts (List.rev $2)) }
| nodespec_list state_annot IDENT LPAR vdecl_list SCOL_opt RPAR RETURNS LPAR vdecl_list SCOL_opt RPAR SCOL_opt locals LET eq_list TEL 
    {let eqs, asserts, annots = $16 in
     let nd = mktop_decl (Node
                            {node_id = $3;
                             node_type = Types.new_var ();
                             node_clock = Clocks.new_var true;
                             node_inputs = List.rev $5;
                             node_outputs = List.rev $10;
                             node_locals = List.rev $14;
			     node_gencalls = [];
			     node_checks = [];
			     node_asserts = asserts; 
                             node_eqs = eqs;
			     node_dec_stateless = $2;
			     node_stateless = None;
			     node_spec = $1;
			     node_annot = annots})
     in
     let loc = Location.symbol_rloc () in
     add_node loc true ("node " ^ $3) node_table $3 nd; nd}

nodespec_list:
 { None }
| NODESPEC nodespec_list { 
  (function 
  | None    -> (fun s1 -> Some s1) 
  | Some s2 -> (fun s1 -> Some (merge_node_annot s1 s2))) $2 $1 }

typ_def_list:
    /* empty */ {}
| typ_def SCOL typ_def_list {$1;$3}

typ_def:
  TYPE IDENT EQ typeconst {
    try
      let loc = Location.symbol_rloc () in
      add_symbol loc ("type " ^ $2) type_table (Tydec_const $2) (get_repr_type $4)
    with Not_found-> assert false }
| TYPE IDENT EQ ENUM LCUR tag_list RCUR { Hashtbl.add type_table (Tydec_const $2) (Tydec_enum ($6 (Tydec_const $2))) }
| TYPE IDENT EQ STRUCT LCUR field_list RCUR { Hashtbl.add type_table (Tydec_const $2) (Tydec_struct ($6 (Tydec_const $2))) }

array_typ_decl:
                            { fun typ -> typ }
 | POWER dim array_typ_decl { fun typ -> $3 (Tydec_array ($2, typ)) }

typeconst:
  TINT array_typ_decl  { $2 Tydec_int }
| TBOOL array_typ_decl { $2 Tydec_bool  }
| TREAL array_typ_decl { $2 Tydec_real  }
| TFLOAT array_typ_decl { $2 Tydec_float }
| IDENT array_typ_decl { 
        let loc = Location.symbol_rloc () in
	check_symbol loc ("type " ^ $1) type_table (Tydec_const $1); $2 (Tydec_const $1) }
| TBOOL TCLOCK  { Tydec_clock Tydec_bool }
| IDENT TCLOCK  { Tydec_clock (Tydec_const $1) }

tag_list:
  IDENT
  { let loc = Location.symbol_rloc () in 
    (fun t -> 
      add_symbol loc ("tag " ^ $1) tag_table $1 t; $1 :: []) }
| tag_list COMMA IDENT
      {       
	let loc = Location.symbol_rloc () in
	(fun t -> add_symbol loc ("tag " ^ $3)tag_table $3 t; $3 :: ($1 t)) 
      }
      
field_list:
  { (fun t -> []) }
| field_list IDENT COL typeconst SCOL
      {
	let loc = Location.symbol_rloc () in
	(fun t -> add_symbol loc ("field " ^ $2) field_table $2 t; ($1 t) @ [ ($2, $4) ]) }
      
eq_list:
  { [], [], [] }
| eq eq_list {let eql, assertl, annotl = $2 in ($1::eql), assertl, annotl}
| assert_ eq_list {let eql, assertl, annotl = $2 in eql, ($1::assertl), annotl}
| ANNOT eq_list {let eql, assertl, annotl = $2 in eql, assertl, $1::annotl}
| automaton eq_list {let eql, assertl, annotl = $2 in ($1::eql), assertl, annotl}

automaton:
 AUTOMATON IDENT handler_list { failwith "not implemented" }

handler_list:
     { [] }
| handler handler_list { $1::$2 }

handler:
 STATE IDENT ARROW unless_list locals LET eq_list TEL until_list { () }

unless_list:
    { [] }
| unless unless_list { $1::$2 }

until_list:
    { [] }
| until until_list { $1::$2 }

unless:
  UNLESS expr RESTART IDENT { }
| UNLESS expr RESUME IDENT  { }

until:
  UNTIL expr RESTART IDENT { }
| UNTIL expr RESUME IDENT  { }

assert_:
| ASSERT expr SCOL {mkassert ($2)}

eq:
       ident_list      EQ expr SCOL {mkeq (List.rev $1,$3)}
| LPAR ident_list RPAR EQ expr SCOL {mkeq (List.rev $2,$5)}

lustre_spec:
| contract EOF { $1 }

contract:
requires ensures behaviors { { requires = $1; ensures = $2; behaviors = $3; spec_loc = get_loc () } }
 
requires:
{ [] }
| REQUIRES qexpr SCOL requires { $2::$4 }

ensures:
{ [] }
| ENSURES qexpr SCOL ensures { $2 :: $4 }
| OBSERVER IDENT LPAR tuple_expr RPAR SCOL ensures { 
  mkeexpr (mkexpr ((Expr_appl ($2, mkexpr (Expr_tuple $4), None)))) :: $7
}

behaviors:
{ [] }
| BEHAVIOR IDENT COL assumes ensures behaviors { ($2,$4,$5,get_loc ())::$6 }

assumes:
{ [] }
| ASSUMES qexpr SCOL assumes { $2::$4 } 

/* WARNING: UNUSED RULES */
tuple_qexpr:
| qexpr COMMA qexpr {[$3;$1]}
| tuple_qexpr COMMA qexpr {$3::$1}

qexpr:
| expr { mkeexpr $1 }
  /* Quantifiers */
| EXISTS vdecl SCOL qexpr %prec prec_exists { extend_eexpr [Exists, $2] $4 } 
| FORALL vdecl SCOL qexpr %prec prec_forall { extend_eexpr [Forall, $2] $4 }


tuple_expr:
    expr COMMA expr {[$3;$1]}
| tuple_expr COMMA expr {$3::$1}

// Same as tuple expr but accepting lists with single element
array_expr:
  expr {[$1]}
| expr COMMA array_expr {$1::$3}

dim_list:
  dim RBRACKET { fun base -> mkexpr (Expr_access (base, $1)) }
| dim RBRACKET LBRACKET dim_list { fun base -> $4 (mkexpr (Expr_access (base, $1))) }

expr:
/* constants */
  INT {mkexpr (Expr_const (Const_int $1))}
| REAL {mkexpr (Expr_const (Const_real $1))}
| FLOAT {mkexpr (Expr_const (Const_float $1))}
/* Idents or type enum tags */
| IDENT {
  if Hashtbl.mem tag_table $1
  then mkexpr (Expr_const (Const_tag $1))
  else mkexpr (Expr_ident $1)}
| LPAR ANNOT expr RPAR
    {update_expr_annot $3 $2}
| LPAR expr RPAR
    {$2}
| LPAR tuple_expr RPAR
    {mkexpr (Expr_tuple (List.rev $2))}

/* Array expressions */
| LBRACKET array_expr RBRACKET { mkexpr (Expr_array $2) }
| expr POWER dim { mkexpr (Expr_power ($1, $3)) }
| expr LBRACKET dim_list { $3 $1 }

/* Temporal operators */
| PRE expr 
    {mkexpr (Expr_pre $2)}
| expr ARROW expr 
    {mkexpr (Expr_arrow ($1,$3))}
| expr FBY expr 
    {(*mkexpr (Expr_fby ($1,$3))*)
      mkexpr (Expr_arrow ($1, mkexpr (Expr_pre $3)))}
| expr WHEN IDENT 
    {mkexpr (Expr_when ($1,$3,tag_true))}
| expr WHENNOT IDENT
    {mkexpr (Expr_when ($1,$3,tag_false))}
| expr WHEN IDENT LPAR IDENT RPAR
    {mkexpr (Expr_when ($1, $5, $3))}
| MERGE IDENT handler_expr_list
    {mkexpr (Expr_merge ($2,$3))}

/* Applications */
| IDENT LPAR expr RPAR
    {mkexpr (Expr_appl ($1, $3, None))}
| IDENT LPAR expr RPAR EVERY IDENT
    {mkexpr (Expr_appl ($1, $3, Some ($6, tag_true)))}
| IDENT LPAR expr RPAR EVERY IDENT LPAR IDENT RPAR
    {mkexpr (Expr_appl ($1, $3, Some ($8, $6))) }
| IDENT LPAR tuple_expr RPAR
    {mkexpr (Expr_appl ($1, mkexpr (Expr_tuple (List.rev $3)), None))}
| IDENT LPAR tuple_expr RPAR EVERY IDENT
    {mkexpr (Expr_appl ($1, mkexpr (Expr_tuple (List.rev $3)), Some ($6, tag_true))) }
| IDENT LPAR tuple_expr RPAR EVERY IDENT LPAR IDENT RPAR
    {mkexpr (Expr_appl ($1, mkexpr (Expr_tuple (List.rev $3)), Some ($8, $6))) }

/* Boolean expr */
| expr AND expr 
    {mkpredef_call "&&" [$1;$3]}
| expr AMPERAMPER expr 
    {mkpredef_call "&&" [$1;$3]}
| expr OR expr 
    {mkpredef_call "||" [$1;$3]}
| expr BARBAR expr 
    {mkpredef_call "||" [$1;$3]}
| expr XOR expr 
    {mkpredef_call "xor" [$1;$3]}
| NOT expr 
    {mkpredef_call "not" [$2]}
| expr IMPL expr 
    {mkpredef_call "impl" [$1;$3]}

/* Comparison expr */
| expr EQ expr 
    {mkpredef_call "=" [$1;$3]}
| expr LT expr 
    {mkpredef_call "<" [$1;$3]}
| expr LTE expr 
    {mkpredef_call "<=" [$1;$3]}
| expr GT expr 
    {mkpredef_call ">" [$1;$3]}
| expr GTE  expr 
    {mkpredef_call ">=" [$1;$3]}
| expr NEQ expr 
    {mkpredef_call "!=" [$1;$3]}

/* Arithmetic expr */
| expr PLUS expr 
    {mkpredef_call "+" [$1;$3]}
| expr MINUS expr 
    {mkpredef_call "-" [$1;$3]}
| expr MULT expr 
    {mkpredef_call "*" [$1;$3]}
| expr DIV expr 
    {mkpredef_call "/" [$1;$3]}
| MINUS expr %prec UMINUS
  {mkpredef_call "uminus" [$2]}
| expr MOD expr 
    {mkpredef_call "mod" [$1;$3]}

/* If */
| IF expr THEN expr ELSE expr
    {mkexpr (Expr_ite ($2, $4, $6))}

handler_expr_list:
   { [] }
| handler_expr handler_expr_list { $1 :: $2 }

handler_expr:
 LPAR IDENT ARROW expr RPAR { ($2, $4) }

signed_const_array:
| signed_const { [$1] }
| signed_const COMMA signed_const_array { $1 :: $3 }

signed_const_struct:
| IDENT EQ signed_const { [ ($1, $3) ] }
| IDENT EQ signed_const COMMA signed_const_struct { ($1, $3) :: $5 }

signed_const:
  INT {Const_int $1}
| REAL {Const_real $1}
| FLOAT {Const_float $1}
| IDENT {Const_tag $1}
| MINUS INT {Const_int (-1 * $2)}
| MINUS REAL {Const_real ("-" ^ $2)}
| MINUS FLOAT {Const_float (-1. *. $2)}
| LCUR signed_const_struct RCUR { Const_struct $2 }
| LBRACKET signed_const_array RBRACKET { Const_array $2 }

dim:
   INT { mkdim_int $1 }
| LPAR dim RPAR { $2 }
| IDENT { mkdim_ident $1 }
| dim AND dim 
    {mkdim_appl "&&" [$1;$3]}
| dim AMPERAMPER dim 
    {mkdim_appl "&&" [$1;$3]}
| dim OR dim 
    {mkdim_appl "||" [$1;$3]}
| dim BARBAR dim 
    {mkdim_appl "||" [$1;$3]}
| dim XOR dim 
    {mkdim_appl "xor" [$1;$3]}
| NOT dim 
    {mkdim_appl "not" [$2]}
| dim IMPL dim 
    {mkdim_appl "impl" [$1;$3]}

/* Comparison dim */
| dim EQ dim 
    {mkdim_appl "=" [$1;$3]}
| dim LT dim 
    {mkdim_appl "<" [$1;$3]}
| dim LTE dim 
    {mkdim_appl "<=" [$1;$3]}
| dim GT dim 
    {mkdim_appl ">" [$1;$3]}
| dim GTE  dim 
    {mkdim_appl ">=" [$1;$3]}
| dim NEQ dim 
    {mkdim_appl "!=" [$1;$3]}

/* Arithmetic dim */
| dim PLUS dim 
    {mkdim_appl "+" [$1;$3]}
| dim MINUS dim 
    {mkdim_appl "-" [$1;$3]}
| dim MULT dim 
    {mkdim_appl "*" [$1;$3]}
| dim DIV dim 
    {mkdim_appl "/" [$1;$3]}
| MINUS dim %prec UMINUS
  {mkdim_appl "uminus" [$2]}
| dim MOD dim 
    {mkdim_appl "mod" [$1;$3]}
/* If */
| IF dim THEN dim ELSE dim
    {mkdim_ite $2 $4 $6}

locals:
  {[]}
| VAR vdecl_list SCOL {$2}

vdecl_list:
    vdecl {$1}
| vdecl_list SCOL vdecl {$3 @ $1}

vdecl:
/* Useless no ?*/    ident_list
    {List.map mkvar_decl 
        (List.map (fun id -> (id, mktyp Tydec_any, mkclock Ckdec_any, false)) $1)}

| ident_list COL typeconst clock 
    {List.map mkvar_decl (List.map (fun id -> (id, mktyp $3, $4, false)) $1)}
| CONST ident_list COL typeconst /* static parameters don't have clocks */
    {List.map mkvar_decl (List.map (fun id -> (id, mktyp $4, mkclock Ckdec_any, true)) $2)}

cdecl_list:
  cdecl SCOL { [$1] }
| cdecl_list cdecl SCOL { $2::$1 }

cdecl:
    IDENT EQ signed_const {
      let c = {
	const_id = $1;
	const_loc = Location.symbol_rloc ();
        const_type = Types.new_var ();
	const_value = $3;
      } in
      Hashtbl.add consts_table $1 c; c
    }

clock:
    {mkclock Ckdec_any}
| when_list
    {mkclock (Ckdec_bool (List.rev $1))}

when_cond:
    WHEN IDENT {($2, tag_true)}
| WHENNOT IDENT {($2, tag_false)}
| WHEN IDENT LPAR IDENT RPAR {($4, $2)}

when_list:
    when_cond {[$1]}
| when_list when_cond {$2::$1}

ident_list:
  IDENT {[$1]}
| ident_list COMMA IDENT {$3::$1}

SCOL_opt:
    SCOL {} | {}


lustre_annot:
lustre_annot_list EOF { { annots = $1; annot_loc = get_loc () } }

lustre_annot_list:
  { [] } 
| kwd COL qexpr SCOL lustre_annot_list { ($1,$3)::$5 }
| IDENT COL qexpr SCOL lustre_annot_list { ([$1],$3)::$5 }
| INVARIANT COL qexpr SCOL lustre_annot_list{ (["invariant"],$3)::$5 }
| OBSERVER COL qexpr SCOL lustre_annot_list { (["observer"],$3)::$5 }

kwd:
DIV { [] }
| DIV IDENT kwd { $2::$3}

%%
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)


