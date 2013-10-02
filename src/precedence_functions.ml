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

open Task_graph

(* Precedences as defined by g_ops(n) (see thesis manuscript) *)

(* g_ops(n) *)
let rec gops ops n =
  match ops with
  | [] -> n
  | (Gfby _)::rest ->
      gops rest (n+1)
  | (Guclock k)::rest ->
      gops rest (k*n)
  | (Gdclock k)::rest ->
      gops rest (int_of_float (ceil ((float_of_int n)/. (float_of_int k))))
  | (Gphclock _)::rest ->
      gops rest n
  | Gtail::rest ->
      if n = 0 then gops rest 0 else
      gops rest (n-1)
  | (Gconcat _)::rest ->
      gops rest (n+1)

(* pref(ops) *)
let pref_size ops =
  let rec aux ops =
    match ops with
    | [] -> 0
    | (Guclock k)::rest ->
        let props = aux rest in
        int_of_float (ceil ((float_of_int props) /. (float_of_int k)))
    | (Gdclock k)::rest ->
        let props = aux rest in
        (props -1)*k+1
    | Gtail::rest ->
        (aux rest)+1
    | (Gfby _)::rest | (Gconcat _)::rest ->
        let props = aux rest in
        max (props -1) 0
    | (Gphclock _)::rest ->
        aux rest
  in
  max (aux ops) 0

(* P(ops) *)
let rec periodicity ops =
  match ops with
  | [] -> 1
  | (Guclock k)::rest ->
      let pops = periodicity rest in
      pops/(Utils.gcd k pops)
  | (Gdclock k)::rest ->
      k*(periodicity rest)
  | (Gfby _)::rest ->
      periodicity rest
  | (Gphclock _)::rest | Gtail::rest | (Gconcat _)::rest ->
      periodicity rest

(* Returns the non-redundant precedence relation as defined in RTAS, ie
   the set of pairs (n,n') such that ti.n->tj.n' (in the first HP) *)
let prec_relation ops =
  let spref = pref_size ops in
  let spat = periodicity ops in
  let pref = Hashtbl.create spref in
  let pat = Hashtbl.create spat in
  let aux tbl n =
    if gops ops n <> gops ops (n+1) then
      Hashtbl.add tbl n (gops ops n)
  in
  for i=0 to spref-1 do
    aux pref i;
  done;
  for i=0 to spat-1 do
    aux pat i;
  done;
  (pref,pat)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
