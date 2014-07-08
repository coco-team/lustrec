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

(** Types definitions and a few utility functions on types. *)
open Utils
open Dimension

type type_expr =
    {mutable tdesc: type_desc;
     tid: int}

and type_desc =
  | Tconst of ident (* type constant *)
  | Tint
  | Treal
  | Tbool
  | Trat (* Actually unused for now. Only place where it can appear is
            in a clock declaration *)
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
  | Undefined_var of (unit IMap.t)
  | Declared_but_undefined of ident
  | Unbound_type of ident
  | Not_a_dimension
  | Not_a_constant
  | Assigned_constant of ident
  | WrongArity of int * int
  | WrongMorphism of int * int
  | Type_clash of type_expr * type_expr
  | Poly_imported_node of ident

exception Unify of type_expr * type_expr
exception Error of Location.t * error

(* Pretty-print*)
open Format

let rec print_struct_ty_field fmt (label, ty) =
  fprintf fmt "%a : %a" pp_print_string label print_ty ty
and print_ty fmt ty =
  match ty.tdesc with
  | Tvar ->
    fprintf fmt "_%s" (name_of_type ty.tid)
  | Tint ->
    fprintf fmt "int"
  | Treal ->
    fprintf fmt "real"
  | Tbool ->
    fprintf fmt "bool"
  | Tclock t ->
    fprintf fmt "%a clock" print_ty t
  | Tstatic (d, t) ->
    fprintf fmt "(%a:%a)" Dimension.pp_dimension d print_ty t
  | Tconst t ->
    fprintf fmt "%s" t
  | Trat ->
    fprintf fmt "rat"
  | Tarrow (ty1,ty2) ->
    fprintf fmt "%a -> %a" print_ty ty1 print_ty ty2
  | Ttuple tylist ->
    fprintf fmt "(%a)"
      (Utils.fprintf_list ~sep:"*" print_ty) tylist
  | Tenum taglist ->
    fprintf fmt "enum {%a }"
      (Utils.fprintf_list ~sep:", " pp_print_string) taglist
  | Tstruct fieldlist ->
    fprintf fmt "struct {%a }"
      (Utils.fprintf_list ~sep:"; " print_struct_ty_field) fieldlist
  | Tarray (e, ty) ->
    fprintf fmt "%a^%a" print_ty ty Dimension.pp_dimension e
  | Tlink ty ->
      print_ty fmt ty
  | Tunivar ->
    fprintf fmt "'%s" (name_of_type ty.tid)

let rec print_node_struct_ty_field fmt (label, ty) =
  fprintf fmt "%a : %a" pp_print_string label print_node_ty ty
and print_node_ty fmt ty =
  match ty.tdesc with
  | Tvar -> begin
(*Format.eprintf "DEBUG:Types.print_node@.";*)
    fprintf fmt "_%s" (name_of_type ty.tid)
end
  | Tint ->
    fprintf fmt "int"
  | Treal ->
    fprintf fmt "real"
  | Tbool ->
    fprintf fmt "bool"
  | Tclock t ->
    fprintf fmt "%a clock" print_node_ty t
  | Tstatic (_, t) ->
    fprintf fmt "%a" print_node_ty t
  | Tconst t ->
    fprintf fmt "%s" t
  | Trat ->
    fprintf fmt "rat"
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
  | Undefined_var vmap ->
    fprintf fmt "No definition provided for variable(s): %a@."
      (Utils.fprintf_list ~sep:"," pp_print_string)
      (fst (Utils.list_of_imap vmap))
  | Declared_but_undefined id ->
     fprintf fmt "Node %s is declared but not defined@." id
  | Type_clash (ty1,ty2) ->
      Utils.reset_names ();
    fprintf fmt "Expected type %a, got type %a@." print_ty ty1 print_ty ty2
  | Poly_imported_node id ->
    fprintf fmt "Imported nodes cannot have a polymorphic type@."


let new_id = ref (-1)

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

let is_numeric_type ty =
 match (repr ty).tdesc with
 | Tint
 | Treal -> true
 | _     -> false

let is_bool_type ty =
 match (repr ty).tdesc with
 | Tbool -> true
 | _     -> false

let get_clock_base_type ty =
 match (repr ty).tdesc with
 | Tclock ty -> Some ty
 | _         -> None

let rec is_dimension_type ty =
 match (repr ty).tdesc with
 | Tint
 | Tbool -> true
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

let rec is_array_type ty =
 match (repr ty).tdesc with
 | Tarray _         -> true
 | Tstatic (_, ty') -> is_array_type ty' (* looks strange !? *)
 | _                -> false

let array_type_dimension ty =
  match (dynamic_type ty).tdesc with
  | Tarray (d, _) -> d
  | _             -> assert false

let rec array_type_multi_dimension ty =
  match (dynamic_type ty).tdesc with
  | Tarray (d, ty') -> d :: array_type_multi_dimension ty'
  | _               -> []

let array_element_type ty =
  match (dynamic_type ty).tdesc with
  | Tarray (_, ty') -> ty'
  | _               -> assert false

let rec array_base_type ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tarray (_, ty')
  | Tstatic (_, ty') -> array_base_type ty'
  | _                -> ty

let is_address_type ty =
  is_array_type ty || is_struct_type ty

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

let type_list_of_type ty =
 match (repr ty).tdesc with
 | Ttuple tl -> tl
 | _         -> [ty]

(** [is_polymorphic ty] returns true if [ty] is polymorphic. *)
let rec is_polymorphic ty =
  match ty.tdesc with
  | Tenum _ | Tvar | Tint | Treal | Tbool | Trat | Tconst _ -> false
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


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
