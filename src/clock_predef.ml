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

(** Predefined operator clocks *)
open Clocks

let ck_tuple cl = new_ck (Ctuple cl) true

let ck_bin_univ =
  let univ = new_univar () in
  new_ck (Carrow (new_ck (Ctuple [univ;univ]) true, univ)) true

let ck_ite =
  let univ = new_univar () in
  new_ck (Carrow (new_ck (Ctuple [univ;univ;univ]) true, univ)) true

let ck_nullary_univ =
  let univ = new_univar () in
  univ

let ck_unary_univ =
  let univ = new_univar () in
  new_ck (Carrow (univ, univ)) true

let ck_bool_to_clock =
  let univ = new_univar () in
  let cuniv = new_carrier Carry_var false in
  new_ck (Carrow (univ, new_ck (Ccarrying (cuniv, univ)) false))

let ck_clock_to_bool =
  let univ = new_univar () in
  let cuniv = new_carrier Carry_var false in
  new_ck (Carrow (new_ck (Ccarrying (cuniv, univ)) false, univ))

let ck_carrier id ck =
 new_ck (Ccarrying (new_carrier (Carry_const id) true, ck)) true
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
