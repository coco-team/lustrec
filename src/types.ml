(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT - LIFL             *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *) 
(*  This file was originally from the Prelude compiler              *)
(*                                                                  *) 
(********************************************************************)

(** Types definitions and a few utility functions on types. *)
open Utils
open Dimension

module type BASIC_TYPES =
sig
  type t
  val pp: Format.formatter -> t -> unit
  val pp_c: Format.formatter -> t -> unit
  val is_scalar_type: t -> bool
  val is_numeric_type: t -> bool
  val is_int_type: t -> bool
  val is_real_type: t -> bool
  val is_bool_type: t -> bool
  val is_dimension_type: t -> bool
  val type_int_builder: t
  val type_real_builder: t
  val type_bool_builder: t
  val type_string_builder: t
  val unify: t -> t -> unit
  val is_unifiable: t -> t -> bool
end

module Basic =
struct
  type t =
    | Tstring
    | Tint
    | Treal
    | Tbool
    | Trat (* Actually unused for now. Only place where it can appear is
              in a clock declaration *)

  let type_string_builder = Tstring
  let type_int_builder = Tint
  let type_real_builder = Treal
  let type_bool_builder = Tbool

  open Format
  let pp fmt t =
    match t with
    | Tint ->
       fprintf fmt "int"
    | Treal ->
       fprintf fmt "real"
    | Tstring ->
       fprintf fmt "string"
    | Tbool ->
       fprintf fmt "bool"
    | Trat ->
       fprintf fmt "rat"

  let pp_c = pp
    
  let is_scalar_type t =
    match t with
    | Tbool
    | Tint
    | Treal -> true
    | _ -> false


  let is_numeric_type t =
    match t with
    | Tint
    | Treal -> true
    | _ -> false

  let is_int_type t = t = Tint
  let is_real_type t = t = Treal
  let is_bool_type t = t = Tbool

  let is_dimension_type t =
    match t with
       | Tint
 | Tbool -> true
 | _ -> false

  let is_unifiable b1 b2 = b1 == b2
  let unify _ _ = ()
end


  
module Make(BasicT : BASIC_TYPES) =
struct

  module BasicT = BasicT
  type basic_type = BasicT.t
  type type_expr   =
    {mutable tdesc: type_desc;
     tid: int}
  and type_desc =
    | Tconst of ident (* type constant *)
    | Tbasic of basic_type
    | Tclock of type_expr (* A type expression explicitely tagged as carrying a clock *)
    | Tarrow of type_expr * type_expr
    | Ttuple of type_expr list
    | Tenum of ident list
    | Tstruct of (ident * type_expr) list
    | Tarray of dim_expr * type_expr
    | Tstatic of dim_expr * type_expr (* a type carried by a dimension expression *)
    | Tlink of type_expr (* During unification, make links instead of substitutions *)
    | Tvar (* Monomorphic type variable *)
    | Tunivar (* Polymorphic type variable *)

  (*   {mutable tdesc: type_desc; *)
  (*    tid: int} *)

  (* and type_desc = *)
  (*   | Tconst of ident (\* type constant *\) *)
  (*   | Tbasic of BasicT.t *)
  (*   | Tclock of type_expr (\* A type expression explicitely tagged as carrying a clock *\) *)
  (*   | Tarrow of type_expr * type_expr *)
  (*   | Ttuple of type_expr list *)
  (*   | Tenum of ident list *)
  (*   | Tstruct of (ident * type_expr) list *)
  (*   | Tarray of dim_expr * type_expr *)
  (*   | Tstatic of dim_expr * type_expr (\* a type carried by a dimension expression *\) *)
  (*   | Tlink of type_expr (\* During unification, make links instead of substitutions *\) *)
  (*   | Tvar (\* Monomorphic type variable *\) *)
  (*   | Tunivar (\* Polymorphic type variable *\) *)

  type error =
      Unbound_value of ident  
    | Already_bound of ident
    | Already_defined of ident
    | Undefined_var of ISet.t
    | Declared_but_undefined of ident
    | Unbound_type of ident
    | Not_a_dimension
    | Not_a_constant
    | Assigned_constant of ident
    | WrongArity of int * int
    | WrongMorphism of int * int
    | Type_mismatch of ident
    | Type_clash of type_expr * type_expr
    | Poly_imported_node of ident

exception Unify of type_expr * type_expr
exception Error of Location.t * error

let mk_basic t = Tbasic t

     
(* Pretty-print*)
open Format

let rec print_struct_ty_field pp_basic fmt (label, ty) =
  fprintf fmt "%a : %a" pp_print_string label (print_ty_param pp_basic) ty
and print_ty_param pp_basic fmt ty =
  let print_ty = print_ty_param pp_basic in
  match ty.tdesc with
  | Tvar ->
    fprintf fmt "_%s" (name_of_type ty.tid)
  | Tbasic t -> pp_basic fmt t
  | Tclock t ->
    fprintf fmt "%a clock" print_ty t
  | Tstatic (d, t) ->
    fprintf fmt "(%a:%a)" Dimension.pp_dimension d print_ty t
  | Tconst t ->
    fprintf fmt "%s" t
  | Tarrow (ty1,ty2) ->
    fprintf fmt "%a -> %a" print_ty ty1 print_ty ty2
  | Ttuple tylist ->
    fprintf fmt "(%a)"
      (Utils.fprintf_list ~sep:" * " print_ty) tylist
  | Tenum taglist ->
    fprintf fmt "enum {%a }"
      (Utils.fprintf_list ~sep:", " pp_print_string) taglist
  | Tstruct fieldlist ->
    fprintf fmt "struct {%a }"
      (Utils.fprintf_list ~sep:"; " (print_struct_ty_field pp_basic)) fieldlist
  | Tarray (e, ty) ->
    fprintf fmt "%a^%a" print_ty ty Dimension.pp_dimension e
  | Tlink ty ->
      print_ty fmt ty
  | Tunivar ->
    fprintf fmt "'%s" (name_of_type ty.tid)

let print_ty = print_ty_param BasicT.pp
 
    
let rec print_node_struct_ty_field fmt (label, ty) =
  fprintf fmt "%a : %a" pp_print_string label print_node_ty ty
and print_node_ty fmt ty =
  match ty.tdesc with
  | Tvar -> begin
    (*Format.eprintf "DEBUG:Types.print_node@.";*)
    fprintf fmt "_%s" (name_of_type ty.tid)
  end
  | Tbasic t -> BasicT.pp fmt t
  | Tclock t ->
    fprintf fmt "%a clock" print_node_ty t
  | Tstatic (_, t) ->
    fprintf fmt "%a" print_node_ty t
  | Tconst t ->
    fprintf fmt "%s" t
  | Tarrow (ty1,ty2) ->
    fprintf fmt "%a -> %a" print_node_ty ty1 print_node_ty ty2
  | Ttuple tylist ->
    fprintf fmt "(%a)"
      (Utils.fprintf_list ~sep:"*" print_node_ty) tylist
  | Tenum taglist ->
    fprintf fmt "enum {%a }"
      (Utils.fprintf_list ~sep:", " pp_print_string) taglist
  | Tstruct fieldlist ->
    fprintf fmt "struct {%a }"
      (Utils.fprintf_list ~sep:"; " print_node_struct_ty_field) fieldlist
  | Tarray (e, ty) ->
    fprintf fmt "%a^%a" print_node_ty ty Dimension.pp_dimension e
  | Tlink ty ->
      print_node_ty fmt ty
  | Tunivar ->
    fprintf fmt "'%s" (name_of_type ty.tid)

let pp_error fmt = function
  | Unbound_value id ->
    fprintf fmt "Unknown value %s@." id
  | Unbound_type id ->
    fprintf fmt "Unknown type %s@." id
  | Already_bound id ->
    fprintf fmt "%s is already declared@." id
  | Already_defined id ->
    fprintf fmt "Multiple definitions of variable %s@." id
  | Not_a_constant ->
    fprintf fmt "This expression is not a constant@."
  | Assigned_constant id ->
    fprintf fmt "The constant %s cannot be assigned@." id
  | Not_a_dimension ->
    fprintf fmt "This expression is not a valid dimension@."
  | WrongArity (ar1, ar2) ->
    fprintf fmt "Expecting %d argument(s), found %d@." ar1 ar2
  | WrongMorphism (ar1, ar2) ->
    fprintf fmt "Expecting %d argument(s) for homomorphic extension, found %d@." ar1 ar2
  | Type_mismatch id ->
    fprintf fmt "Definition and declaration of type %s don't agree@." id
  | Undefined_var vset ->
    fprintf fmt "No definition provided for variable(s): %a@."
      (Utils.fprintf_list ~sep:"," pp_print_string)
      (ISet.elements vset)
  | Declared_but_undefined id ->
     fprintf fmt "%s is declared but not defined@." id
  | Type_clash (ty1,ty2) ->
      Utils.reset_names ();
    fprintf fmt "Expected type %a, got type %a@." print_ty ty1 print_ty ty2
  | Poly_imported_node id ->
    fprintf fmt "Imported nodes cannot have a polymorphic type@."


let new_id = ref (-1)

let rec bottom =
  { tdesc = Tlink bottom; tid = -666 }

let new_ty desc =
  incr new_id; {tdesc = desc; tid = !new_id }

let new_var () =
  new_ty Tvar

let new_univar () =
  new_ty Tunivar

let rec repr =
  function
    {tdesc = Tlink t'} ->
      repr t'
  | t -> t

let get_static_value ty =
  match (repr ty).tdesc with
  | Tstatic (d, _) -> Some d
  | _              -> None

let get_field_type ty label =
  match (repr ty).tdesc with
  | Tstruct fl -> (try Some (List.assoc label fl) with Not_found -> None)
  | _          -> None

let rec is_static_type ty =
  match (repr ty).tdesc with
  | Tstatic (_, ty) -> true
  | _     -> false

let rec is_scalar_type ty =
  match (repr ty).tdesc with
  | Tstatic (_, ty) -> is_scalar_type ty
  | Tbasic t -> BasicT.is_scalar_type t
  | _     -> false

let rec is_numeric_type ty =
 match (repr ty).tdesc with
 | Tstatic (_, ty) -> is_numeric_type ty
 | Tbasic t -> BasicT.is_numeric_type t
 | _     -> false
    
let rec is_real_type ty =
 match (repr ty).tdesc with
 | Tstatic (_, ty) -> is_real_type ty
 | Tbasic t -> BasicT.is_real_type t
 | _     -> false

let rec is_int_type ty =
 match (repr ty).tdesc with
 | Tstatic (_, ty) -> is_int_type ty
 | Tbasic t -> BasicT.is_int_type t
 | _     -> false

let rec is_bool_type ty =
 match (repr ty).tdesc with
 | Tstatic (_, ty) -> is_bool_type ty
 | Tbasic t -> BasicT.is_bool_type t
 | _     -> false

let rec is_const_type ty c =
  match (repr ty).tdesc with
  | Tstatic (_, ty) -> is_const_type ty c
  | Tconst c' -> c = c'
  | _     -> false

let get_clock_base_type ty =
 match (repr ty).tdesc with
 | Tclock ty -> Some ty
 | _         -> None

let unclock_type ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tclock ty' -> ty'
  | _          -> ty

let rec is_dimension_type ty =
 match (repr ty).tdesc with
 | Tbasic t -> BasicT.is_dimension_type t
 | Tclock ty'
 | Tstatic (_, ty') -> is_dimension_type ty'
 | _                -> false

let dynamic_type ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tstatic (_, ty') -> ty'
  | _                -> ty

let is_tuple_type ty =
 match (repr ty).tdesc with
 | Ttuple _         -> true
 | _                -> false

let map_tuple_type f ty =
  let ty = dynamic_type ty in
  match ty.tdesc with
  | (Ttuple ty_list) -> { ty with tdesc = Ttuple (List.map f ty_list) }
  | _                -> f ty

let rec is_struct_type ty =
 match (repr ty).tdesc with
 | Tstruct _        -> true
 | Tstatic (_, ty') -> is_struct_type ty'
 | _                -> false

let struct_field_type ty field =
  match (dynamic_type ty).tdesc with
  | Tstruct fields ->
    (try
       List.assoc field fields
     with Not_found -> assert false)
  | _              -> assert false

let rec is_array_type ty =
 match (repr ty).tdesc with
 | Tarray _         -> true
 | Tstatic (_, ty') -> is_array_type ty' (* looks strange !? *)
 | _                -> false

let array_type_dimension ty =
  match (dynamic_type ty).tdesc with
  | Tarray (d, _) -> d
  | _             -> (Format.eprintf "internal error: Types.array_type_dimension %a@." print_ty ty; assert false)

let rec array_type_multi_dimension ty =
  match (dynamic_type ty).tdesc with
  | Tarray (d, ty') -> d :: array_type_multi_dimension ty'
  | _               -> []

let array_element_type ty =
  match (dynamic_type ty).tdesc with
  | Tarray (_, ty') -> ty'
  | _               -> (Format.eprintf "internal error: Types.array_element_type %a@." print_ty ty; assert false)

let rec array_base_type ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tarray (_, ty')
  | Tstatic (_, ty') -> array_base_type ty'
  | _                -> ty

let is_address_type ty =
  is_array_type ty || is_struct_type ty || (is_real_type ty && !Options.mpfr)

let rec is_generic_type ty =
 match (dynamic_type ty).tdesc with
  | Tarray (d, ty') ->
    (not (Dimension.is_dimension_const d)) || (is_generic_type ty')
  | _               -> false

(** Splits [ty] into the [lhs,rhs] of an arrow type. Expects an arrow type
    (ensured by language syntax) *)
let rec split_arrow ty =
  match (repr ty).tdesc with
  | Tarrow (tin,tout) -> tin,tout
  | Tstatic (_, ty')  -> split_arrow ty'
    (* Functions are not first order, I don't think the var case
       needs to be considered here *)
  | _ -> Format.eprintf "type %a is not a map@.Unable to split@.@?" print_ty ty; assert false

(** Returns the type corresponding to a type list. *)
let type_of_type_list tyl =
  if (List.length tyl) > 1 then
    new_ty (Ttuple tyl)
  else
    List.hd tyl

let rec type_list_of_type ty =
 match (repr ty).tdesc with
 | Tstatic (_, ty) -> type_list_of_type ty
 | Ttuple tl       -> tl
 | _               -> [ty]

(** [is_polymorphic ty] returns true if [ty] is polymorphic. *)
let rec is_polymorphic ty =
  match ty.tdesc with
  | Tenum _ | Tvar | Tbasic _ | Tconst _ -> false
  | Tclock ty -> is_polymorphic ty
  | Tarrow (ty1,ty2) -> (is_polymorphic ty1) || (is_polymorphic ty2)
  | Ttuple tl -> List.exists (fun t -> is_polymorphic t) tl
  | Tstruct fl -> List.exists (fun (_, t) -> is_polymorphic t) fl
  | Tlink t' -> is_polymorphic t'
  | Tarray (d, ty)
  | Tstatic (d, ty) -> Dimension.is_polymorphic d || is_polymorphic ty
  | Tunivar -> true


let mktyptuple nb typ =
  let array = Array.make nb typ in
  Ttuple (Array.to_list array)

let type_desc t = t.tdesc



let type_int = mk_basic BasicT.type_int_builder
let type_real = mk_basic BasicT.type_real_builder
let type_bool = mk_basic BasicT.type_bool_builder
let type_string = mk_basic BasicT.type_string_builder
    
end


module type S = 
sig
  module BasicT: BASIC_TYPES 
  type basic_type = BasicT.t
  type type_expr   =
    {mutable tdesc: type_desc;
     tid: int}
  and type_desc =
    | Tconst of ident (* type constant *)
    | Tbasic of basic_type
    | Tclock of type_expr (* A type expression explicitely tagged as carrying a clock *)
    | Tarrow of type_expr * type_expr
    | Ttuple of type_expr list
    | Tenum of ident list
    | Tstruct of (ident * type_expr) list
    | Tarray of dim_expr * type_expr
    | Tstatic of dim_expr * type_expr (* a type carried by a dimension expression *)
    | Tlink of type_expr (* During unification, make links instead of substitutions *)
    | Tvar (* Monomorphic type variable *)
    | Tunivar (* Polymorphic type variable *)

  type error =
      Unbound_value of ident  
    | Already_bound of ident
    | Already_defined of ident
    | Undefined_var of ISet.t
    | Declared_but_undefined of ident
    | Unbound_type of ident
    | Not_a_dimension
    | Not_a_constant
    | Assigned_constant of ident
    | WrongArity of int * int
    | WrongMorphism of int * int
    | Type_mismatch of ident
    | Type_clash of type_expr * type_expr
    | Poly_imported_node of ident

	  exception Unify of type_expr * type_expr
	  exception Error of Location.t * error

  val is_real_type: type_expr -> bool
  val is_int_type: type_expr -> bool
  val is_bool_type: type_expr -> bool
  val is_const_type: type_expr -> ident -> bool
  val is_static_type: type_expr -> bool
  val is_array_type: type_expr -> bool
  val is_dimension_type: type_expr -> bool
  val is_address_type: type_expr -> bool
  val is_generic_type: type_expr -> bool
  val print_ty: Format.formatter -> type_expr -> unit
  val repr: type_expr -> type_expr
  val dynamic_type: type_expr -> type_expr
  val type_desc: type_expr -> type_desc
  val new_var: unit -> type_expr
  val new_univar: unit -> type_expr
  val new_ty: type_desc -> type_expr
  val type_int: type_desc
  val type_real: type_desc
  val type_bool: type_desc
  val type_string: type_desc
  val array_element_type: type_expr -> type_expr
  val type_list_of_type: type_expr -> type_expr list
  val print_node_ty: Format.formatter -> type_expr -> unit
  val get_clock_base_type: type_expr -> type_expr option
  val get_static_value: type_expr -> Dimension.dim_expr option
  val is_tuple_type: type_expr -> bool
  val type_of_type_list: type_expr list -> type_expr
  val split_arrow: type_expr -> type_expr * type_expr
  val unclock_type: type_expr -> type_expr
  val bottom: type_expr
  val map_tuple_type: (type_expr -> type_expr) -> type_expr -> type_expr
  val array_base_type: type_expr -> type_expr
  val array_type_dimension: type_expr -> Dimension.dim_expr
  val pp_error: Format.formatter -> error -> unit
  val struct_field_type: type_expr -> ident -> type_expr
  val array_type_multi_dimension: type_expr -> Dimension.dim_expr list
end (* with type type_expr = BasicT.t type_expr_gen *)

module type Sbasic = S with type BasicT.t = Basic.t 
  
module Main : Sbasic = Make (Basic)
include Main 


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
