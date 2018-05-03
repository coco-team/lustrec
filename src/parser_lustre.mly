/********************************************************************/
/*                                                                  */
/*  The LustreC compiler toolset   /  The LustreC Development Team  */
/*  Copyright 2012 -    --   ONERA - CNRS - INPT                    */
/*                                                                  */
/*  LustreC is free software, distributed WITHOUT ANY WARRANTY      */
/*  under the terms of the GNU Lesser General Public License        */
/*  version 2.1.                                                    */
/*                                                                  */
/********************************************************************/

%{
open Utils
open Lustre_types
open Corelang
open Dimension
open Parse

let get_loc () = Location.symbol_rloc ()

let mkident x = x, get_loc ()
let mktyp x = mktyp (get_loc ()) x
let mkclock x = mkclock (get_loc ()) x
let mkvar_decl x loc = mkvar_decl loc ~orig:true x
let mkexpr x = mkexpr (get_loc ()) x
let mkeexpr x = mkeexpr (get_loc ()) x 
let mkeq x = mkeq (get_loc ()) x
let mkassert x = mkassert (get_loc ()) x
let mktop_decl itf x = mktop_decl (get_loc ()) (Location.get_module ()) itf x
let mkpredef_call x = mkpredef_call (get_loc ()) x
(*let mkpredef_unary_call x = mkpredef_unary_call (get_loc ()) x*)

let mkdim_int i = mkdim_int (get_loc ()) i
let mkdim_bool b = mkdim_bool (get_loc ()) b
let mkdim_ident id = mkdim_ident (get_loc ()) id
let mkdim_appl f args = mkdim_appl (get_loc ()) f args
let mkdim_ite i t e = mkdim_ite (get_loc ()) i t e

let mkannots annots = { annots = annots; annot_loc = get_loc () }

let node_stack : ident list ref = ref []
let debug_calls () = Format.eprintf "call stack: %a@.@?" (Utils.fprintf_list ~sep:", " Format.pp_print_string) !node_stack
let push_node nd =  node_stack:= nd :: !node_stack
let pop_node () = try node_stack := List.tl !node_stack with _ -> assert false
let get_current_node () = try List.hd !node_stack with _ -> assert false

let rec fby expr n init =
  if n<=1 then
    mkexpr (Expr_arrow (init, mkexpr (Expr_pre expr)))
  else
    mkexpr (Expr_arrow (init, mkexpr (Expr_pre (fby expr (n-1) init))))
  
%}

%token <int> INT
%token <Num.num * int * string> REAL

%token <string> STRING
%token AUTOMATON STATE UNTIL UNLESS RESTART RESUME LAST
%token STATELESS ASSERT OPEN QUOTE FUNCTION
%token <string> IDENT
%token <string> UIDENT
%token TRUE FALSE
%token <Lustre_types.expr_annot> ANNOT
%token <Lustre_types.node_annot> NODESPEC
%token LBRACKET RBRACKET LCUR RCUR LPAR RPAR SCOL COL COMMA COLCOL 
%token AMPERAMPER BARBAR NOT POWER
%token IF THEN ELSE
%token UCLOCK DCLOCK PHCLOCK TAIL
%token MERGE FBY WHEN WHENNOT EVERY
%token NODE LET TEL RETURNS VAR IMPORTED SENSOR ACTUATOR WCET TYPE CONST
%token STRUCT ENUM
%token TINT TREAL TBOOL TCLOCK
%token RATE DUE
%token EQ LT GT LTE GTE NEQ
%token AND OR XOR IMPL
%token MULT DIV MOD
%token MINUS PLUS UMINUS
%token PRE ARROW
%token REQUIRES ENSURES OBSERVER
%token INVARIANT BEHAVIOR ASSUMES CCODE MATLAB
%token EXISTS FORALL
%token PROTOTYPE LIB
%token EOF

%nonassoc prec_exists prec_forall
%nonassoc COMMA
%nonassoc EVERY
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
%type <Lustre_types.top_decl list> prog

%start header
%type <Lustre_types.top_decl list> header

%start lustre_annot
%type <Lustre_types.expr_annot> lustre_annot

%start lustre_spec
%type <Lustre_types.node_annot> lustre_spec

%start signed_const
%type <Lustre_types.constant> signed_const

%start expr
%type <Lustre_types.expr> expr

%start stmt_list
%type <Lustre_types.statement list * Lustre_types.assert_t list * Lustre_types.expr_annot list > stmt_list

%start vdecl_list
%type <Lustre_types.var_decl list> vdecl_list
%%

module_ident:
  UIDENT { $1 }
| IDENT  { $1 }

tag_ident:
  UIDENT  { $1 }
| TRUE    { tag_true }
| FALSE   { tag_false }

node_ident:
  UIDENT { $1 }
| IDENT  { $1 }

node_ident_decl:
 node_ident { push_node $1; $1 }

vdecl_ident:
  UIDENT { mkident $1 }
| IDENT  { mkident $1 }

const_ident:
  UIDENT { $1 }
| IDENT  { $1 }

type_ident:
  IDENT { $1 }

prog:
 open_list typ_def_prog top_decl_list EOF { $1 @ $2 @ (List.rev $3) }

typ_def_prog:
 typ_def_list { $1 false }

header:
 open_list typ_def_header top_decl_header_list EOF { $1 @ $2 @ (List.rev $3) }

typ_def_header:
 typ_def_list { $1 true }

open_list:
  { [] }
| open_lusi open_list { $1 :: $2 }

open_lusi:
| OPEN QUOTE module_ident QUOTE { mktop_decl false (Open (true, $3))}
| OPEN LT module_ident GT { mktop_decl false (Open (false, $3)) }

top_decl_list:
   {[]}
| top_decl_list top_decl {$2@$1}


top_decl_header_list:
   { [] }
| top_decl_header_list top_decl_header { $2@$1 }

state_annot:
  FUNCTION { true }
| NODE { false }

top_decl_header:
| CONST cdecl_list { List.rev ($2 true) }
| nodespec_list state_annot node_ident LPAR vdecl_list SCOL_opt RPAR RETURNS LPAR vdecl_list SCOL_opt RPAR  prototype_opt in_lib_list SCOL
    {let nd = mktop_decl true (ImportedNode
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
     (*add_imported_node $3 nd;*) [nd] }

prototype_opt:
 { None }
| PROTOTYPE node_ident { Some $2}

in_lib_list:
{ [] }
| LIB module_ident in_lib_list { $2::$3 } 

top_decl:
| CONST cdecl_list { List.rev ($2 false) }
| nodespec_list state_annot node_ident_decl LPAR vdecl_list SCOL_opt RPAR RETURNS LPAR vdecl_list SCOL_opt RPAR SCOL_opt locals LET stmt_list TEL 
    {
     let stmts, asserts, annots = $16 in
      (* Declaring eqs annots *)
      List.iter (fun ann -> 
	List.iter (fun (key, _) -> 
	  Annotations.add_node_ann $3 key
	) ann.annots
      ) annots;
     (* Building the node *)
     let nd = mktop_decl false (Node
				  {node_id = $3;
				   node_type = Types.new_var ();
				   node_clock = Clocks.new_var true;
				   node_inputs = List.rev $5;
				   node_outputs = List.rev $10;
				   node_locals = List.rev $14;
				   node_gencalls = [];
				   node_checks = [];
				   node_asserts = asserts; 
				   node_stmts = stmts;
				   node_dec_stateless = $2;
				   node_stateless = None;
				   node_spec = $1;
				   node_annot = annots})
     in
     pop_node ();
     (*add_node $3 nd;*) [nd] }

nodespec_list:
 { None }
| NODESPEC nodespec_list { 
  (function 
  | None    -> (fun s1 -> Some s1) 
  | Some s2 -> (fun s1 -> Some (merge_node_annot s1 s2))) $2 $1 }

typ_def_list:
    /* empty */             { (fun itf -> []) }
| typ_def SCOL typ_def_list { (fun itf -> let ty1 = ($1 itf) in ty1 :: ($3 itf)) }

typ_def:
  TYPE type_ident EQ typ_def_rhs { (fun itf ->
			       let typ = mktop_decl itf (TypeDef { tydef_id = $2;
								   tydef_desc = $4
							})
			       in (*add_type itf $2 typ;*) typ) }

typ_def_rhs:
  typeconst                   { $1 }
| ENUM LCUR tag_list RCUR     { Tydec_enum (List.rev $3) }
| STRUCT LCUR field_list RCUR { Tydec_struct (List.rev $3) }

array_typ_decl:
 %prec POWER                { fun typ -> typ }
 | POWER dim array_typ_decl { fun typ -> $3 (Tydec_array ($2, typ)) }

typeconst:
  TINT array_typ_decl   { $2 Tydec_int }
| TBOOL array_typ_decl  { $2 Tydec_bool  }
| TREAL array_typ_decl  { $2 Tydec_real  }
/* | TFLOAT array_typ_decl { $2 Tydec_float } */
| type_ident array_typ_decl  { $2 (Tydec_const $1) }
| TBOOL TCLOCK          { Tydec_clock Tydec_bool }
| IDENT TCLOCK          { Tydec_clock (Tydec_const $1) }

tag_list:
  UIDENT                { $1 :: [] }
| tag_list COMMA UIDENT { $3 :: $1 }
      
field_list:                           { [] }
| field_list IDENT COL typeconst SCOL { ($2, $4) :: $1 }
      
stmt_list:
  { [], [], [] }
| eq stmt_list {let eql, assertl, annotl = $2 in ((Eq $1)::eql), assertl, annotl}
| assert_ stmt_list {let eql, assertl, annotl = $2 in eql, ($1::assertl), annotl}
| ANNOT stmt_list {let eql, assertl, annotl = $2 in eql, assertl, $1::annotl}
| automaton stmt_list {let eql, assertl, annotl = $2 in ((Aut $1)::eql), assertl, annotl}

automaton:
 AUTOMATON type_ident handler_list { Automata.mkautomata (get_loc ()) $2 $3 }

handler_list:
     { [] }
| handler handler_list { $1::$2 }

handler:
 STATE UIDENT COL unless_list locals LET stmt_list TEL until_list { Automata.mkhandler (get_loc ()) $2 $4 $9 $5 $7 }

unless_list:
    { [] }
| unless unless_list { $1::$2 }

until_list:
    { [] }
| until until_list { $1::$2 }

unless:
  UNLESS expr RESTART UIDENT { (get_loc (), $2, true, $4)  }
| UNLESS expr RESUME UIDENT  { (get_loc (), $2, false, $4) }

until:
  UNTIL expr RESTART UIDENT { (get_loc (), $2, true, $4)  }
| UNTIL expr RESUME UIDENT  { (get_loc (), $2, false, $4) }

assert_:
| ASSERT expr SCOL {mkassert ($2)}

eq:
       ident_list      EQ expr SCOL {mkeq (List.rev (List.map fst $1), $3)}
| LPAR ident_list RPAR EQ expr SCOL {mkeq (List.rev (List.map fst $2), $5)}

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
| OBSERVER node_ident LPAR tuple_expr RPAR SCOL ensures { 
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
| REAL {let c,e,s = $1 in mkexpr (Expr_const (Const_real (c,e,s)))}
| STRING {mkexpr (Expr_const (Const_string $1))}

/* | FLOAT {mkexpr (Expr_const (Const_float $1))}*/
/* Idents or type enum tags */
| IDENT { mkexpr (Expr_ident $1) }
| tag_ident { mkexpr (Expr_ident $1) (*(Expr_const (Const_tag $1))*) }
| LPAR ANNOT expr RPAR
    {update_expr_annot (get_current_node ()) $3 $2}
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
| expr WHEN vdecl_ident
    {mkexpr (Expr_when ($1,fst $3,tag_true))}
| expr WHENNOT vdecl_ident
    {mkexpr (Expr_when ($1,fst $3,tag_false))}
| expr WHEN tag_ident LPAR vdecl_ident RPAR
    {mkexpr (Expr_when ($1, fst $5, $3))}
| MERGE vdecl_ident handler_expr_list
    {mkexpr (Expr_merge (fst $2,$3))}

/* Applications */
| node_ident LPAR expr RPAR
    {mkexpr (Expr_appl ($1, $3, None))}
| node_ident LPAR expr RPAR EVERY expr
    {mkexpr (Expr_appl ($1, $3, Some $6))}
| node_ident LPAR tuple_expr RPAR
    {
      let id=$1 in
      let args=List.rev $3 in
      match id, args with
      | "fbyn", [expr;n;init] ->
	let n = match n.expr_desc with
	  | Expr_const (Const_int n) -> n
	  | _ -> assert false
	in
	fby expr n init
      | _ -> mkexpr (Expr_appl ($1, mkexpr (Expr_tuple args), None))
    }
| node_ident LPAR tuple_expr RPAR EVERY expr
    {
      let id=$1 in
      let args=List.rev $3 in
      let clock=$6 in
      if id="fby" then
	assert false (* TODO Ca veut dire quoi fby (e,n,init) every c *)
      else
	mkexpr (Expr_appl (id, mkexpr (Expr_tuple args), Some clock)) 
    }

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
 LPAR tag_ident ARROW expr RPAR { ($2, $4) }

signed_const_array:
| signed_const { [$1] }
| signed_const COMMA signed_const_array { $1 :: $3 }

signed_const_struct:
| IDENT EQ signed_const { [ ($1, $3) ] }
| IDENT EQ signed_const COMMA signed_const_struct { ($1, $3) :: $5 }

signed_const:
  INT {Const_int $1}
| REAL {let c,e,s =$1 in Const_real (c,e,s)}
/* | FLOAT {Const_float $1} */
| tag_ident {Const_tag $1}
| MINUS INT {Const_int (-1 * $2)}
| MINUS REAL {let c,e,s = $2 in Const_real (Num.minus_num c, e, "-" ^ s)}
/* | MINUS FLOAT {Const_float (-1. *. $2)} */
| LCUR signed_const_struct RCUR { Const_struct $2 }
| LBRACKET signed_const_array RBRACKET { Const_array $2 }

dim:
   INT { mkdim_int $1 }
| LPAR dim RPAR { $2 }
| UIDENT { mkdim_ident $1 }
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
| VAR local_vdecl_list SCOL {$2}

vdecl_list:
  vdecl {$1}
| vdecl_list SCOL vdecl {$3 @ $1}

vdecl:
  ident_list COL typeconst clock 
    { List.map (fun (id, loc) -> mkvar_decl (id, mktyp $3, $4, false, None, None) loc) $1 }
| CONST ident_list /* static parameters don't have clocks */
    { List.map (fun (id, loc) -> mkvar_decl (id, mktyp Tydec_any, mkclock Ckdec_any, true, None, None) loc) $2 }
| CONST ident_list COL typeconst /* static parameters don't have clocks */
    { List.map (fun (id, loc) -> mkvar_decl (id, mktyp $4, mkclock Ckdec_any, true, None, None) loc) $2 }

local_vdecl_list:
  local_vdecl {$1}
| local_vdecl_list SCOL local_vdecl {$3 @ $1}

local_vdecl:
/* Useless no ?*/    ident_list
    { List.map (fun (id, loc) -> mkvar_decl (id, mktyp Tydec_any, mkclock Ckdec_any, false, None, None) loc) $1 }
| ident_list COL typeconst clock 
    { List.map (fun (id, loc) -> mkvar_decl (id, mktyp $3, $4, false, None, None) loc) $1 }
| CONST vdecl_ident EQ expr /* static parameters don't have clocks */
    { let (id, loc) = $2 in [ mkvar_decl (id, mktyp Tydec_any, mkclock Ckdec_any, true, Some $4, None) loc] }
| CONST vdecl_ident COL typeconst EQ expr /* static parameters don't have clocks */
    { let (id, loc) = $2 in [ mkvar_decl (id, mktyp $4, mkclock Ckdec_any, true, Some $6, None) loc] }

cdecl_list:
  cdecl SCOL { (fun itf -> [$1 itf]) }
| cdecl cdecl_list SCOL { (fun itf -> let c1 = ($1 itf) in c1::($2 itf)) }

cdecl:
    const_ident EQ signed_const {
      (fun itf -> 
       let c = mktop_decl itf (Const {
				   const_id = $1;
				   const_loc = Location.symbol_rloc ();
				   const_type = Types.new_var ();
				   const_value = $3})
       in
       (*add_const itf $1 c;*) c)
    }

clock:
    {mkclock Ckdec_any}
| when_list
    {mkclock (Ckdec_bool (List.rev $1))}

when_cond:
  WHEN IDENT {($2, tag_true)}
| WHENNOT IDENT {($2, tag_false)}
| WHEN tag_ident LPAR IDENT RPAR {($4, $2)}

when_list:
    when_cond {[$1]}
| when_list when_cond {$2::$1}

ident_list:
  vdecl_ident {[$1]}
| ident_list COMMA vdecl_ident {$3::$1}

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
| CCODE COL qexpr SCOL lustre_annot_list{ (["c_code"],$3)::$5 }
| MATLAB COL qexpr SCOL lustre_annot_list{ (["matlab"],$3)::$5 }


kwd:
DIV { [] }
| DIV IDENT kwd { $2::$3}

%%
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)


