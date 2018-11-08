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

(** Generic inference environments. Used both for typing and
    clock-calculus. *)
open Utils

type 'a t  = 'a IMap.t
(* Same namespace for nodes, variables and constants *)
let initial = IMap.empty

let add_value env ident ty =
  IMap.add ident ty env

let lookup_value env ident =
  IMap.find ident env

let exists_value env ident =
  IMap.mem ident env

let iter env f = IMap.iter f env
let fold = IMap.fold

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
