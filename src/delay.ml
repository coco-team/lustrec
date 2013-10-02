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

(** Types definitions and a few utility functions on delay types. *)
(** Delay analysis by type polymorphism instead of constraints *)
open Utils

type delay_expr =
    {mutable ddesc: delay_desc;
     did: int}

and delay_desc =
  | Dvar (* Monomorphic type variable *)
  | Dundef
  | Darrow of delay_expr * delay_expr
  | Dtuple of delay_expr list
  | Dlink of delay_expr (* During unification, make links instead of substitutions *)
  | Dunivar (* Polymorphic type variable *)
type error =
  | Delay_clash of delay_expr * delay_expr

exception Unify of delay_expr * delay_expr
exception Error of Location.t * error

let new_id = ref (-1)

let new_delay desc =
  incr new_id; {ddesc = desc; did = !new_id }

let new_var () =
  new_delay Dvar

let new_univar () =
  new_delay Dunivar

let rec repr =
  function
    {ddesc = Dlink i'} ->
      repr i'
  | i -> i

(** Splits [ty] into the [lhs,rhs] of an arrow type. Expects an arrow type
    (ensured by language syntax) *)
let split_arrow de =
  match (repr de).ddesc with
  | Darrow (din,dout) -> din,dout
    (* Functions are not first order, I don't think the var case
       needs to be considered here *)
  | _ -> failwith "Internal error: not an arrow type"

(** Returns the type corresponding to a type list. *)
let delay_of_delay_list de =
  if (List.length de) > 1 then
    new_delay (Dtuple de)
  else
    List.hd de

(** [is_polymorphic de] returns true if [de] is polymorphic. *)
let rec is_polymorphic de =
  match de.ddesc with
  | Dvar  -> false
  | Dundef -> false
  | Darrow (de1,de2) -> (is_polymorphic de1) || (is_polymorphic de2)
  | Dtuple dl -> List.exists is_polymorphic dl
  | Dlink d' -> is_polymorphic d'
  | Dunivar -> true

(* Pretty-print*)
open Format
  
let rec print_delay fmt de =
  match de.ddesc with
  | Dvar ->
    fprintf fmt "'_%s" (name_of_type de.did)
  | Dundef ->
    fprintf fmt "1"
  | Darrow (de1,de2) ->
    fprintf fmt "%a->%a" print_delay de1 print_delay de2
  | Dtuple delist ->
    fprintf fmt "(%a)"
      (Utils.fprintf_list ~sep:"*" print_delay) delist
  | Dlink de ->
      print_delay fmt de
  | Dunivar ->
    fprintf fmt "'%s" (name_of_delay de.did)

let pp_error fmt = function
  | Delay_clash (de1,de2) ->
      Utils.reset_names ();
    fprintf fmt "Expected delay %a, got delay %a@." print_delay de1 print_delay de2
