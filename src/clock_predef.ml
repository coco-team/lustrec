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

(** Predefined operator clocks *)
open Clocks

let ck_bin_univ =
  let univ = new_univar () in
  new_ck (Carrow (new_ck (Ctuple [univ;univ]) true, univ)) true

let ck_ite =
  let univ = new_univar () in
  new_ck (Carrow (new_ck (Ctuple [univ;univ;univ]) true, univ)) true

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

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
