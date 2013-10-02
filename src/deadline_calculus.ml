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

open Format
open Corelang
open Task_graph
open Task_set
open Deadlines
open Precedence_functions
(** Encodes precedences by adjusting task deadlines. The names of the
    functions below refer to the names in the thesis manuscript *)

(* ----- Core functions *)
let rec t_ops ops t =
  match ops with
  | [] -> t
  | (Gfby _)::rest ->
      t_ops rest t
  | (Guclock k)::rest ->
      k*(t_ops rest t)
  | (Gdclock k)::rest ->
      (t_ops rest t)/k
  | (Gphclock _)::rest ->
      t_ops rest t
  | Gtail::rest ->
      t_ops rest t
  | (Gconcat _)::rest ->
      t_ops rest t

let rec r_ops ops r t =
  match ops with
  | [] -> r
  | (Gfby _)::rest ->
      r_ops rest r t
  | (Guclock _)::rest ->
      r_ops rest r t
  | (Gdclock _)::rest ->
      r_ops rest r t
  | (Gphclock (a,b))::rest ->
      (r_ops rest r t) - a*(t_ops rest t)/b
  | Gtail::rest ->
      (r_ops rest r t) - (t_ops rest t)
  | (Gconcat _)::rest ->
      (r_ops rest r t) + (t_ops rest t)

let delta_ops ops ti tj rj n =
  (gops ops n)*tj-n*ti+rj-(r_ops ops rj tj)

(* delta_ops(ti,tj,rj) *)
let delta_word ops ti tj rj =
  let lpref = pref_size ops in
  let lpat = periodicity ops in
  Array.init lpref (fun n -> delta_ops ops ti tj rj n),
  Array.init lpat (fun n -> delta_ops ops ti tj rj (n+lpref))

(* P'(ops) *)
let rec periodicity' wj ops =
  match ops with
  | [] -> pat_length wj
  | (Guclock k)::rest ->
      let pops = periodicity' wj rest in
      pops/(Utils.gcd k pops)
  | (Gdclock k):: rest ->
      k*(periodicity' wj rest)
  | (Gfby _)::rest ->
      periodicity' wj rest
  | (Gphclock _)::rest | Gtail::rest | (Gconcat _)::rest ->
      periodicity' wj rest

let rec pref_size' wj ops =
  let rec aux ops =
    match ops with
    | [] -> pref_length wj
    | (Guclock k)::rest ->
        let props = aux rest in
        int_of_float (ceil ((float_of_int props) /. (float_of_int k)))
    | (Gdclock k)::rest ->
        let props = aux rest in
        max ((props -1)*k+1) 0
    | Gtail::rest ->
        (aux rest)+1
    | (Gfby _)::rest | (Gconcat _)::rest ->
        let props = aux rest in
        max (props -1) 0
    | (Gphclock _)::rest ->
        aux rest
  in
  max (aux ops) 0

let w_ops wj ops =
  let lpref = pref_size' wj ops in
  let lpat = periodicity' wj ops in
  Array.init lpref (fun n -> nth_ud wj (gops ops n)),
  Array.init lpat (fun n -> nth_ud wj (gops ops n))

(* Update deadlines to encode the precedence [from_v]-[annots]->[to_v]. *)
let update_dd task_set from_v to_v annots =
  let task_from = task_of_vertex task_set from_v in
  let task_to = task_of_vertex task_set to_v in
  let wi = task_from.task_deadline in
  let ti = task_from.task_period in
  let wj = task_to.task_deadline in
  let tj = task_to.task_period in
  let rj = task_to.task_release in
  let cj = task_to.task_wcet in
  let delta = delta_word annots ti tj rj in
  let wops = w_ops wj annots in
  let cstr = add_int (add_dw delta wops) (-cj) in
  task_from.task_deadline <- min_dw wi cstr

(* ----- Functions for the topological traversal *)
let count_succs g =
  (* Count successors of tasks, ie count the successors of all the variables
     of the task. *)
  let nb_succs = Hashtbl.create 30 in
  let add_succs v nb =
    let tid = taskid_of_vertex v in
    let nb' =
      try Hashtbl.find nb_succs tid with Not_found -> 0 in
    Hashtbl.replace nb_succs tid (nb'+nb)
  in
  Hashtbl.iter
    (fun vid v ->
      let nb =
        Hashtbl.fold
          (fun succ annot n ->
            if is_delayed_prec annot then n else n+1)
          v.vertex_succs 0 in
      add_succs vid nb)
    g.graph;
  nb_succs

(* Compute the initial set of vertices that have no successors *)
let no_succs task_set nb_succs =
  Hashtbl.fold
    (fun tid nb acc ->
      if nb = 0 then
        let t = Hashtbl.find task_set tid in
        (inputs_of_task t)@acc
      else
        acc)
    nb_succs []

(* Compute the new set of vertices that have no successors. *)
let new_no_succs task_set nb_succs no_succs lost_one_succ =
  let tid = taskid_of_vertex lost_one_succ in
  let n = Hashtbl.find nb_succs tid in
  Hashtbl.replace nb_succs tid (n-1);
  if n = 1 then
    no_succs@(inputs_of_task (task_of_vertex task_set lost_one_succ))
  else
    no_succs

let dd_prog g task_set exp_main =
  let nb_succs = count_succs g in
  (* Traversal of the graph as a classic reverse topological sort. *)
  let rec topo_traversal no_succs =
    match no_succs with
    | [] -> ()
    | v::rest ->
        let pred = (Hashtbl.find g.graph v).vertex_pred in
        match pred with
        | None -> topo_traversal rest
        | Some (p, annot) ->
            if not (is_delayed_prec annot) then
              begin
                update_dd task_set p v annot;
                let no_succs' = new_no_succs task_set nb_succs rest p in
                topo_traversal no_succs'
              end
            else
              topo_traversal rest
  in
  let no_succs = no_succs task_set nb_succs in
  topo_traversal no_succs

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
