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
  
type ident = Utils.ident
type rat = Utils.rat
type tag = Utils.tag
type label = Utils.ident

type type_dec =
    {ty_dec_desc: type_dec_desc;
     ty_dec_loc: Location.t}

and type_dec_desc =
  | Tydec_any
  | Tydec_int
  | Tydec_real
  (* | Tydec_float *)
  | Tydec_bool
  | Tydec_clock of type_dec_desc
  | Tydec_const of ident
  | Tydec_enum of ident list
  | Tydec_struct of (ident * type_dec_desc) list
  | Tydec_array of Dimension.dim_expr * type_dec_desc

type typedec_desc =
    {tydec_id: ident}

type typedef_desc =
    {tydef_id: ident;
     tydef_desc: type_dec_desc}

type clock_dec =
    {ck_dec_desc: clock_dec_desc;
     ck_dec_loc: Location.t}

and clock_dec_desc =
  | Ckdec_any
  | Ckdec_bool of (ident * ident) list


type constant =
  | Const_int of int
  | Const_real of Num.num * int * string (* (a, b, c) means a * 10^-b. c is the original string *)
  | Const_array of constant list
  | Const_tag of label
  | Const_string of string (* used only for annotations *)
  | Const_modeid of string (* used only for annotations *)
  | Const_struct of (label * constant) list

type quantifier_type = Exists | Forall

type var_decl =
    {var_id: ident;
     var_orig:bool;
     var_dec_type: type_dec;
     var_dec_clock: clock_dec;
     var_dec_const: bool;
     var_dec_value: expr option;
     mutable var_parent_nodeid: ident option;
     mutable var_type: Types.type_expr;
     mutable var_clock: Clocks.clock_expr;
     var_loc: Location.t}

(** The core language and its ast. Every element of the ast contains its
    location in the program text. The type and clock of an ast element
    is mutable (and initialized to dummy values). This avoids to have to
    duplicate ast structures (e.g. ast, typed_ast, clocked_ast). *)



(* The tag of an expression is a unique identifier used to distinguish
   different instances of the same node *)
and expr =
    {expr_tag: tag;
     expr_desc: expr_desc;
     mutable expr_type: Types.type_expr;
     mutable expr_clock: Clocks.clock_expr;
     mutable expr_delay: Delay.delay_expr;
     mutable expr_annot: expr_annot option;
     expr_loc: Location.t}

and expr_desc =
  | Expr_const of constant
  | Expr_ident of ident
  | Expr_tuple of expr list
  | Expr_ite   of expr * expr * expr
  | Expr_arrow of expr * expr
  | Expr_fby of expr * expr
  | Expr_array of expr list
  | Expr_access of expr * Dimension.dim_expr
  | Expr_power of expr * Dimension.dim_expr
  | Expr_pre of expr
  | Expr_when of expr * ident * label
  | Expr_merge of ident * (label * expr) list
  | Expr_appl of call_t

and call_t = ident * expr * expr option
     (* The third part denotes the boolean condition for resetting *)

and eq =
    {eq_lhs: ident list;
     eq_rhs: expr;
     eq_loc: Location.t}

  (* The tag of an expression is a unique identifier used to distinguish
     different instances of the same node *)
and  eexpr =
    {eexpr_tag: tag;
     eexpr_qfexpr: expr;
     eexpr_quantifiers: (quantifier_type * var_decl list) list;
     mutable eexpr_type: Types.type_expr;
     mutable eexpr_clock: Clocks.clock_expr;
     mutable eexpr_normalized: (var_decl * eq list * var_decl list) option;
     eexpr_loc: Location.t}

and expr_annot =
 {annots: (string list * eexpr) list;
  annot_loc: Location.t}

type contract_mode =
  { mode_id: ident; require: eexpr list; ensure: eexpr list; mode_loc: Location.t}

type contract_import =
  { import_nodeid: ident; inputs: expr list; outputs: expr list; import_loc: Location.t }
    
type contract_desc = 
  {
(* TODO: 
   local variables 
   rename: assume/guarantee
           in behavior mode (id, requires/ensures)
   import contract
*)
       consts: var_decl list;
       locals: var_decl list;
       assume: eexpr list;
       guarantees: eexpr list;
       modes: contract_mode list;
       imports: contract_import list; 
       spec_loc: Location.t;
}


type offset =
| Index of Dimension.dim_expr
| Field of label

type assert_t =
    {
      assert_expr: expr;
      assert_loc: Location.t;
    }

type statement =
| Eq of eq
| Aut of automata_desc

and automata_desc =
  {aut_id : ident;
   aut_handlers: handler_desc list;
   aut_loc: Location.t}

and handler_desc =
  {hand_state: ident;
   hand_unless: (Location.t * expr * bool * ident) list;
   hand_until: (Location.t * expr * bool * ident) list;
   hand_locals: var_decl list;
   hand_stmts: statement list;
   hand_asserts: assert_t list;
   hand_annots: expr_annot list;
   hand_loc: Location.t}

type node_desc =
    {node_id: ident;
     mutable node_type: Types.type_expr;
     mutable node_clock: Clocks.clock_expr;
     node_inputs: var_decl list;
     node_outputs: var_decl list;
     node_locals: var_decl list;
     mutable node_gencalls: expr list;
     mutable node_checks: Dimension.dim_expr list;
     node_asserts: assert_t list;
     node_stmts: statement list;
     mutable node_dec_stateless: bool;
     mutable node_stateless: bool option;
     node_spec: contract_desc option;
     node_annot: expr_annot list;
    }

type imported_node_desc =
    {nodei_id: ident;
     mutable nodei_type: Types.type_expr;
     mutable nodei_clock: Clocks.clock_expr;
     nodei_inputs: var_decl list;
     nodei_outputs: var_decl list;
     nodei_stateless: bool;
     nodei_spec: contract_desc option;
     (* nodei_annot: expr_annot list; *)
     nodei_prototype: string option;
     nodei_in_lib: string list;
    }

type const_desc =
    {const_id: ident;
     const_loc: Location.t;
     const_value: constant;
     mutable const_type: Types.type_expr;
    }

  
type top_decl_desc =
| Node of node_desc
| Const of const_desc
| ImportedNode of imported_node_desc
| Open of bool * string (* the boolean set to true denotes a local
			   lusi vs a lusi installed at system level *)
| TypeDef of typedef_desc
    
type top_decl =
    {top_decl_desc: top_decl_desc;      (* description of the symbol *)
     top_decl_owner: Location.filename; (* the module where it is defined *)
     top_decl_itf: bool;                (* header or source file ? *)
     top_decl_loc: Location.t}          (* the location where it is defined *)

type program = top_decl list

type dep_t = Dep of
    bool
  * ident
  * (top_decl list)
  * bool (* is stateful *)




(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
