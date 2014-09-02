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

(** Base types and predefined operator types. *)
open Delay

let delay_zero () = new_univar ()

let delay_un =
  new_delay Dundef

let delay_nullary_poly_op =
  let univ = new_univar () in
  univ

let delay_unary_poly_op =
  let univ = new_univar () in
  new_delay (Darrow (univ, univ))

let delay_binary_poly_op =
  let univ = new_univar () in
  new_delay (Darrow (new_delay (Dtuple [univ;univ]), univ))

let delay_ternary_poly_op =
  let univ = new_univar () in
  new_delay (Darrow (new_delay (Dtuple [univ;univ;univ]), univ))



(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
