%{
  open Utils
  open Corelang
  open LustreSpec
  
  let mkeexpr x = mkeexpr (Location.symbol_rloc ()) x
  let mkepredef_call x = mkepredef_call (Location.symbol_rloc ()) x
  let mkepredef_unary_call x = mkepredef_unary_call (Location.symbol_rloc ()) x
  
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
| REQUIRES expr SCOL requires { $2::$4 }

ensures:
{ [] }
| ENSURES expr SCOL ensures { (EnsuresExpr $2) :: $4 }
| OBSERVER IDENT LPAR tuple_expr RPAR SCOL ensures { (SpecObserverNode($2,$4)) :: $7 }

behaviors:
{ [] }
| BEHAVIOR IDENT COL assumes ensures behaviors { ($2,$4,$5)::$6 }

assumes:
{ [] }
| ASSUMES expr SCOL assumes { $2::$4 } 

tuple_expr:
| expr COMMA expr {[$3;$1]}
| tuple_expr COMMA expr {$3::$1}

expr:
| const {mkeexpr (EExpr_const $1)} 
| IDENT 
    {mkeexpr (EExpr_ident $1)}
| LPAR expr RPAR
    {$2}
| LPAR tuple_expr RPAR
    {mkeexpr (EExpr_tuple (List.rev $2))}
| expr ARROW expr 
    {mkeexpr (EExpr_arrow ($1,$3))}
| expr FBY expr 
    {mkeexpr (EExpr_fby ($1,$3))}
| expr WHEN IDENT 
    {mkeexpr (EExpr_when ($1,$3))}
| MERGE LPAR IDENT COMMA expr COMMA expr RPAR
    {mkeexpr (EExpr_merge ($3,$5,$7))}
| IDENT LPAR expr RPAR
    {mkeexpr (EExpr_appl ($1, $3, None))}
| IDENT LPAR expr RPAR EVERY IDENT
    {mkeexpr (EExpr_appl ($1, $3, Some $6))}
| IDENT LPAR tuple_expr RPAR
    {mkeexpr (EExpr_appl ($1, mkeexpr (EExpr_tuple (List.rev $3)), None))}
| IDENT LPAR tuple_expr RPAR EVERY IDENT
    {mkeexpr (EExpr_appl ($1, mkeexpr (EExpr_tuple (List.rev $3)), Some $6)) }

/* Boolean expr */
| expr AND expr 
    {mkepredef_call "&&" [$1;$3]}
| expr AMPERAMPER expr 
    {mkepredef_call "&&" [$1;$3]}
| expr OR expr 
    {mkepredef_call "||" [$1;$3]}
| expr BARBAR expr 
    {mkepredef_call "||" [$1;$3]}
| expr XOR expr 
    {mkepredef_call "xor" [$1;$3]}
| NOT expr 
    {mkepredef_unary_call "not" $2}
| expr IMPL expr 
    {mkepredef_call "impl" [$1;$3]}

/* Comparison expr */
| expr EQ expr 
    {mkepredef_call "=" [$1;$3]}
| expr LT expr 
    {mkepredef_call "<" [$1;$3]}
| expr LTE expr 
    {mkepredef_call "<=" [$1;$3]}
| expr GT expr 
    {mkepredef_call ">" [$1;$3]}
| expr GTE  expr 
    {mkepredef_call ">=" [$1;$3]}
| expr NEQ expr 
    {mkepredef_call "!=" [$1;$3]}

/* Arithmetic expr */
| expr PLUS expr 
    {mkepredef_call "+" [$1;$3]}
| expr MINUS expr 
    {mkepredef_call "-" [$1;$3]}
| expr MULT expr 
    {mkepredef_call "*" [$1;$3]}
| expr DIV expr 
    {mkepredef_call "/" [$1;$3]}
| MINUS expr %prec UMINUS
    {mkepredef_unary_call "uminus" $2}
| expr MOD expr 
    {mkepredef_call "mod" [$1;$3]}

/* Temp op */
| PRE expr 
    {mkeexpr (EExpr_pre $2)}

/* If */
| IF expr THEN expr ELSE expr
    {mkepredef_call "ite" [$2;$4;$6]}

/* Quantifiers */
| EXISTS vdecl SCOL expr %prec prec_exists {mkeexpr (EExpr_exists ($2, $4))} 
| FORALL vdecl SCOL expr %prec prec_forall {mkeexpr (EExpr_forall ($2, $4))}

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
  with Not_found -> raise (Corelang.Error (Corelang.Unbound_symbol ("type " ^ $1), Location.symbol_rloc()))
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
| INT {EConst_int $1}
| REAL {EConst_real $1}
| FLOAT {EConst_float $1}
| TRUE {EConst_bool true}
| FALSE {EConst_bool false}
| STRING {EConst_string $1}

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
