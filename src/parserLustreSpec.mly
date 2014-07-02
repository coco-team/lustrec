%{
  open Utils
  open Corelang
  open LustreSpec
  
  let mkexpr x = mkexpr (Location.symbol_rloc ()) x
  let mkpredef_call x = mkpredef_call (Location.symbol_rloc ()) x
  let mkpredef_unary_call x = mkpredef_unary_call (Location.symbol_rloc ()) x

  let mkeexpr x = mkeexpr (Location.symbol_rloc ()) x
  (*
  let mkepredef_call x = mkepredef_call (Location.symbol_rloc ()) x
  let mkepredef_unary_call x = mkepredef_unary_call (Location.symbol_rloc ()) x
  *)

  let mktyp x = mktyp (Location.symbol_rloc ()) x
  let mkvar_decl x = mkvar_decl (Location.symbol_rloc ()) x
  let mkclock x = mkclock (Location.symbol_rloc ()) x

%}


%token <int> INT
%token <string> REAL
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE STATELESS ASSERT INCLUDE QUOTE
%token <string> IDENT
%token LPAR RPAR SCOL COL COMMA COLCOL
%token AMPERAMPER BARBAR NOT POWER
%token IF THEN ELSE
%token UCLOCK DCLOCK PHCLOCK TAIL
%token MERGE FBY WHEN WHENNOT EVERY
%token NODE LET TEL RETURNS VAR IMPORTED SENSOR ACTUATOR WCET TYPE CONST
%token TINT TFLOAT TREAL TBOOL TCLOCK
%token RATE DUE
%token EQ LT GT LTE GTE NEQ
%token AND OR XOR IMPL
%token MULT DIV MOD
%token MINUS PLUS UMINUS
%token PRE ARROW
%token EOF
%token REQUIRES ENSURES OBSERVER
%token INVARIANT BEHAVIOR ASSUMES
%token EXISTS FORALL

%nonassoc prec_exists prec_forall
%nonassoc COMMA POWER
%left MERGE IF
%nonassoc ELSE
%right ARROW FBY
%left WHEN WHENNOT UCLOCK DCLOCK PHCLOCK
%right COLCOL
%right IMPL
%left OR XOR BARBAR
%left AND AMPERAMPER
%left NOT
%nonassoc EQ LT GT LTE GTE NEQ
%left MINUS PLUS
%left MULT DIV MOD
%left PRE 
%nonassoc UMINUS

%start lustre_annot
%type <LustreSpec.expr_annot> lustre_annot

%start lustre_spec
%type <LustreSpec.node_annot> lustre_spec

%%

lustre_spec:
| contract EOF { $1 }

contract:
requires ensures behaviors { { requires = $1; ensures = $2; behaviors = $3; } }
 
requires:
{ [] }
| REQUIRES qexpr SCOL requires { $2::$4 }

ensures:
{ [] }
| ENSURES qexpr SCOL ensures { (EnsuresExpr $2) :: $4 }
| OBSERVER IDENT LPAR tuple_qexpr RPAR SCOL ensures { (SpecObserverNode($2,$4)) :: $7 }

behaviors:
{ [] }
| BEHAVIOR IDENT COL assumes ensures behaviors { ($2,$4,$5)::$6 }

assumes:
{ [] }
| ASSUMES qexpr SCOL assumes { $2::$4 } 

tuple_qexpr:
| qexpr COMMA qexpr {[$3;$1]}
| tuple_qexpr COMMA qexpr {$3::$1}


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
    {mkpredef_unary_call "not" $2}
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
  {mkpredef_unary_call "uminus" $2}
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

qexpr:
| expr { mkeexpr [] $1 }
  /* Quantifiers */
| EXISTS vdecl SCOL qexpr %prec prec_exists { extend_eepxr (Exists, $2) $4 } 
| FORALL vdecl SCOL qexpr %prec prec_forall { extend_eepxr (Forall, $2) $4 }

vdecl:
| ident_list COL typ clock 
    {List.map mkvar_decl (List.map (fun id -> (id, $3, $4, false)) $1)}
| CONST ident_list COL typ clock 
    {List.map mkvar_decl (List.map (fun id -> (id, $4, $5, true)) $2)}


ident_list:
  IDENT {[$1]}
| ident_list COMMA IDENT {$3::$1}


typ:
    {mktyp Tydec_any}
| TINT {mktyp Tydec_int}
| IDENT {
  try 
    mktyp (Hashtbl.find Corelang.type_table (Tydec_const $1))
  with Not_found -> raise (Corelang.Error (Location.symbol_rloc(), Corelang.Unbound_symbol ("type " ^ $1)))
}
| TFLOAT {mktyp Tydec_float}
| TREAL {mktyp Tydec_real}
| TBOOL {mktyp Tydec_bool}
| TCLOCK {mktyp (Tydec_clock Tydec_bool) }
| typ POWER INT {mktyp Tydec_any (*(mktyptuple $3 $1)*)}
| typ POWER IDENT {mktyp Tydec_any (*(mktyptuple (try 
					match get_const $3 with Const_int i -> i with _ -> failwith "Const power error") $1)*)}

clock:
    {mkclock Ckdec_any}
| when_list
    {mkclock (Ckdec_bool (List.rev $1))}

when_cond:
    WHEN IDENT {($2, tag_true)}
| WHENNOT IDENT {($2, tag_false)}

when_list:
    when_cond {[$1]}
| when_list when_cond {$2::$1}


const:
| INT {Const_int $1}
| REAL {Const_real $1}
| FLOAT {Const_float $1}
| TRUE {Const_bool true}
| FALSE {Const_bool false}
| STRING {Const_string $1}

lustre_annot:
lustre_annot_list EOF { $1 }

lustre_annot_list:
  { [] } 
| kwd COL expr SCOL lustre_annot_list { ($1,$3)::$5 }
| IDENT COL expr SCOL lustre_annot_list { ([$1],$3)::$5 }
| INVARIANT COL expr SCOL lustre_annot_list{ (["invariant"],$3)::$5 }
| OBSERVER COL expr SCOL lustre_annot_list { (["observer"],$3)::$5 }

kwd:
DIV { [] }
| DIV IDENT kwd { $2::$3}
