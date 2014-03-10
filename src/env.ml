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

(** Generic inference environments. Used both for typing and
    clock-calculus. *)
open Utils

(* Same namespace for nodes, variables and constants *)
let initial = IMap.empty

let add_value env ident ty =
  IMap.add ident ty env

let lookup_value env ident =
  IMap.find ident env

let exists_value env ident =
  IMap.mem ident env

let iter env f = IMap.iter f env

(* Merges x and y. In case of conflicting definitions,
   overwrites definitions in x by definitions in y *)
let overwrite x y =
  IMap.merge (
    fun k _old _new -> match _new with
      | Some _ -> _new
      | _ -> _old
  ) x y

open Format

let pp_env pp_fun fmt env =
  let (lid,lty) = list_of_imap env in
  let l' = List.combine lid lty in
  let pp_fun fmt (id,value) =
    Format.fprintf fmt "%s |-> %a" id pp_fun value
  in
  Format.fprintf fmt "{ @[<v 2>%a@] }" (fprintf_list ~sep:"@," pp_fun) l'

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
