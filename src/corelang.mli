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


open LustreSpec

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
val coretype_equal: type_dec_desc -> type_dec_desc -> bool
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
