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

(** Base types and predefined operator types. *)
open Delay

let delay_zero () = new_univar ()

let delay_un =
  new_delay Dundef

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
