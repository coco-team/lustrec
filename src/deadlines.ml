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

open Utils
open Format

type deadline_word = int array * int array

let dword_create ud =
  [||],[|ud|]

let pat_length dw =
  let (dpref, dpat) = dw in
  Array.length dpat

let pref_length dw =
  let (dpref, dpat) = dw in
  Array.length dpref

(* [nth_ud dw n] returns the nth value of dw *)
let nth_ud dw n =
  let (dpref, dpat) = dw in
  let (lpref, lpat) = Array.length dpref, Array.length dpat in
  if n < lpref then
    dpref.(n)
  else
    let pos = (n-lpref) mod lpat in
    dpat.(pos)

let add_int dw n =
  let (dpref, dpat) = dw in
  (Array.map (fun ud -> ud+n) dpref,Array.map (fun ud -> ud+n) dpat)

let dw_map2 f dw1 dw2 =
  let (dpref1, dpat1), (dpref2, dpat2) = dw1, dw2 in
  let (lpref1, lpref2) = Array.length dpref1, Array.length dpref2 in
  let (lpat1, lpat2) = Array.length dpat1, Array.length dpat2 in
  let lpref = max lpref1 lpref2 in
  let lpat = lcm lpat1 lpat2 in
  let pref,pat = Array.make lpref 0, Array.make lpat 0 in
  for i=0 to lpref-1 do
    pref.(i) <- f (nth_ud dw1 i) (nth_ud dw2 i)
  done;
  for i=0 to lpat-1 do
    pat.(i) <- f (nth_ud dw1 (i+lpref)) (nth_ud dw2 (i+lpref))
  done;
  pref,pat
 
let add_dw dw1 dw2 =
  dw_map2 (+) dw1 dw2

(* [min_dw dw1 dw2] returns the minimum, point-by-point, of [dw1] and [dw2].
   Can only be used on fully instanciated deadlines. *)
let min_dw dw1 dw2 =
  dw_map2 (min) dw1 dw2

let print_dw dw =
  let (dpref, dpat) = dw in
  if (Array.length dpref > 0) then
    pp_array dpref print_int "" "." ".";
  pp_array dpat print_int "(" ")" "."

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
