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

open LustreSpec
(*
(** The core language and its ast. *)
type ident = Utils.ident
type label = Utils.ident
type rat = Utils.rat
type tag = Utils.tag

type constant =
  | Const_int of int
  | Const_real of string
  | Const_float of float
  | Const_array of constant list
  | Const_tag  of label
  | Const_struct of (label * constant) list

val dummy_type_dec: type_dec

type type_dec = LustreSpec.type_dec

type clock_dec = LustreSpec.clock_dec
val dummy_clock_dec: clock_dec

type var_decl = LustreSpec.var_decl

type expr =
  {expr_tag: tag; (* Unique identifier *)
   expr_desc: expr_desc;
   mutable expr_type: Types.type_expr;
   mutable expr_clock: Clocks.clock_expr;
   mutable expr_delay: Delay.delay_expr; (* Used for the initialisation check *)
   mutable expr_annot: LustreSpec.expr_annot option; (* Spec *)
   expr_loc: Location.t}

and expr_desc =
| Expr_const of constant
| Expr_ident of ident
| Expr_tuple of expr list
| Expr_ite   of expr * expr * expr
| Expr_arrow of expr * expr
| Expr_fby of expr * expr
(*
| Expr_struct of (label * expr) list
| Expr_field of expr * label
| Expr_update of expr * (label * expr)
*)
| Expr_array of expr list
| Expr_access of expr * Dimension.dim_expr (* acces(e,i) is the i-th element 
					      of array epxression e *)
| Expr_power of expr * Dimension.dim_expr (* power(e,n) is the array of 
					     size n filled with expression e *)
| Expr_pre of expr
| Expr_when of expr * ident * label
| Expr_merge of ident * (label * expr) list
| Expr_appl of call_t
| Expr_uclock of expr * int
| Expr_dclock of expr * int
| Expr_phclock of expr * rat
and call_t = ident * expr * (ident * label) option (* The third part denotes the reseting clock label and value *)

type eq =
    {eq_lhs: ident list;
     eq_rhs: expr;
     eq_loc: Location.t}

type assert_t = 
    {
      assert_expr: expr * eq list;
      assert_loc: Location.t
    } 

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
     node_eqs: eq list;
     mutable node_dec_stateless: bool;
     mutable node_stateless: bool option;
     node_spec: LustreSpec.node_annot option;
     node_annot: LustreSpec.expr_annot option;}

type imported_node_desc =
    {nodei_id: ident;
     mutable nodei_type: Types.type_expr;
     mutable nodei_clock: Clocks.clock_expr;
     nodei_inputs: var_decl list;
     nodei_outputs: var_decl list;
     nodei_stateless: bool;
     nodei_spec: LustreSpec.node_annot option;
     nodei_prototype: string option;
     nodei_in_lib: string option;
}
(*
type imported_fun_desc =
    {fun_id: ident;
     mutable fun_type: Types.type_expr;
     fun_inputs: var_decl list;
     fun_outputs: var_decl list;
     fun_spec: LustreSpec.node_annot option;}
*)
type const_desc = 
    {const_id: ident; 
     const_loc: Location.t; 
     const_value: constant;      
     mutable const_type: Types.type_expr;
    }
(* type sensor_desc = *)
(*     {sensor_id: ident; *)
(*      sensor_wcet: int} *)

(* type actuator_desc = *)
(*     {actuator_id: ident; *)
(*      actuator_wcet: int} *)

type top_decl_desc =
  | Node of node_desc
  | Consts of const_desc list
  | ImportedNode of imported_node_desc
  (* | ImportedFun of imported_fun_desc *)
  (* | SensorDecl of sensor_desc *)
  (* | ActuatorDecl of actuator_desc *)
  | Open of bool * string

type top_decl =
    {top_decl_desc: top_decl_desc;
     top_decl_loc: Location.t}

type program = top_decl list

type error =
    Main_not_found
  | Main_wrong_kind
  | No_main_specified
  | Unbound_symbol of ident
  | Already_bound_symbol of ident
*)
exception Error of Location.t * error

val dummy_type_dec: type_dec
val dummy_clock_dec: clock_dec

val mktyp: Location.t -> type_dec_desc -> type_dec
val mkclock: Location.t -> clock_dec_desc -> clock_dec
val mkvar_decl: Location.t -> ident * type_dec * clock_dec * bool (* is const *) -> var_decl
val var_decl_of_const: const_desc -> var_decl
val mkexpr: Location.t ->  expr_desc -> expr
val mkeq: Location.t -> ident list * expr -> eq
val mkassert: Location.t -> expr -> assert_t
val mktop_decl: Location.t -> top_decl_desc -> top_decl
val mkpredef_call: Location.t -> ident -> expr list -> expr
val mk_new_name: var_decl list -> ident -> ident


val node_table : (ident, top_decl) Hashtbl.t
val node_name: top_decl -> ident
val node_inputs: top_decl -> var_decl list
val node_from_name: ident -> top_decl
val is_generic_node: top_decl -> bool
val is_imported_node: top_decl -> bool

val consts_table: (ident, const_desc) Hashtbl.t
val type_table: (type_dec_desc, type_dec_desc) Hashtbl.t
val get_repr_type: type_dec_desc -> type_dec_desc
val is_user_type: type_dec_desc -> bool
val tag_true: label
val tag_false: label
val tag_table: (label, type_dec_desc) Hashtbl.t
val field_table: (label, type_dec_desc) Hashtbl.t

val get_enum_type_tags: type_dec_desc -> label list

val get_struct_type_fields: type_dec_desc -> (label * type_dec_desc) list

val const_of_bool: bool -> constant
val const_is_bool: constant -> bool
val const_negation: constant -> constant
val const_or: constant -> constant -> constant
val const_and: constant -> constant -> constant
val const_xor: constant -> constant -> constant
val const_impl: constant -> constant -> constant

val get_node_vars: node_desc -> var_decl list
val get_node_var: ident -> node_desc -> var_decl
val get_node_eq: ident -> node_desc -> eq

(* val get_const: ident -> constant *)

val sort_handlers : (label * 'a) list -> (label * 'a) list

val is_eq_expr: expr -> expr -> bool

val pp_error :  Format.formatter -> error -> unit

(* Caution, returns an untyped, unclocked, etc, expression *)
val is_tuple_expr : expr -> bool
val ident_of_expr : expr -> ident
val expr_of_ident : ident -> Location.t -> expr
val expr_list_of_expr : expr -> expr list
val expr_of_expr_list : Location.t -> expr list -> expr
val call_of_expr: expr -> (ident * expr list * (ident * label) option)
val expr_of_dimension: Dimension.dim_expr -> expr
val dimension_of_expr: expr -> Dimension.dim_expr
val dimension_of_const: Location.t -> constant -> Dimension.dim_expr

(* REMOVED, pushed in utils.ml   val new_tag : unit -> tag *)

val add_internal_funs: unit -> unit

val pp_prog_type : Format.formatter -> program -> unit

val pp_prog_clock : Format.formatter -> program -> unit

val get_nodes : program -> node_desc list
 val get_consts : program -> const_desc list 
(* val prog_unfold_consts: program -> program *)

val expr_replace_var: (ident -> ident) -> expr -> expr
val eq_replace_rhs_var: (ident -> bool) -> (ident -> ident) -> eq -> eq

(** rename_prog f_node f_var f_const prog *)
val rename_prog: (ident -> ident) -> (ident -> ident) -> (ident -> ident) -> program -> program

val update_expr_annot: expr -> LustreSpec.expr_annot -> expr

val substitute_expr: var_decl list -> eq list -> expr -> expr

(** Annotation expression related functions *)
val mkeexpr: Location.t ->  expr -> eexpr
val merge_node_annot: node_annot -> node_annot -> node_annot 
val extend_eexpr: (quantifier_type * var_decl list) list -> eexpr -> eexpr
val update_expr_annot: expr -> expr_annot -> expr
(* val mkpredef_call: Location.t -> ident -> eexpr list -> eexpr*)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
