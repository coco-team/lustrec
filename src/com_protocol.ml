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

(** Compute the communication protocols of a task graph. *)

open Deadlines
open Precedence_functions
open Corelang
open Task_graph
open Task_set
open Format

(* Number of times instance [n] is consumed *)
let conso_ops ops n =
  (gops ops (n+1)) - (gops ops n)

(* Lifespan of instance [n] *)
let lifespan ti ops tj n =
  let span_start = abs_release ti n in
  if (conso_ops ops n) =0 then
    (span_start,span_start)
  else
    let first_conso_release = abs_release tj (gops ops n) in
    let span_end = first_conso_release + (conso_ops ops n) * tj.task_period in
    (span_start,span_end)

(* The number of cells of the buffer *)
let nb_cells ti ops tj =
  let total_length = (pref_size ops) + (periodicity ops) in
  let max_intersects = ref 0 in
  for k = 0 to total_length -1 do
    let (sp_start,sp_end) = lifespan ti ops tj k in
    let sp_size = sp_end - sp_start in
    (* the following is an overapproximation *)
    let approx_intersect =
      int_of_float (ceil ((float_of_int sp_size) /. (float_of_int ti.task_period)))
    in
    if approx_intersect > !max_intersects then
      max_intersects := approx_intersect
  done;
  if contains_init ops then
    !max_intersects + 1 
  else
    !max_intersects

(* Pattern that the writer must follow. *)
let write_pattern ti ops tj =
  let write_pref = Array.make (pref_size ops) true in
  for k = 0 to (pref_size ops) -1 do
    if (conso_ops ops k) >= 1 then
      write_pref.(k) <- true
    else
      write_pref.(k) <- false
  done;
  let write_pat = Array.make (periodicity ops) true in
  for k = 0 to (periodicity ops) -1 do
    if (conso_ops ops (k+(pref_size ops))) >= 1 then
      write_pat.(k) <- true
    else
      write_pat.(k) <- false
  done;
  write_pref,write_pat

(* Pattern that the reader must follow *)
let read_pattern ti ops tj =
  (* TODO: concat operator ! *)
  let read_pattern_pref = ref [] in
  if (gops ops 0) > 0 then (* First reads the init of the fby/concat *)
    begin
      for k=0 to (gops ops 0)-2 do
        read_pattern_pref := false::!read_pattern_pref
      done;
      read_pattern_pref := true::!read_pattern_pref
    end;
  for k = 0 to (pref_size ops) - 1 do
    let dep_ij_k = (conso_ops ops k) in
    if  dep_ij_k >= 1 then
      begin
        for k'=0 to dep_ij_k -2 do
          read_pattern_pref := false::!read_pattern_pref
        done;
        read_pattern_pref := true::!read_pattern_pref
      end
  done;
  let read_pattern_pat = ref [] in
  for k = 0 to (periodicity ops) - 1 do
    let dep_ij_k = (conso_ops ops (k+(pref_size ops))) in
    if  dep_ij_k >= 1 then
      begin
        for k'=0 to dep_ij_k -2 do
          read_pattern_pat := false::!read_pattern_pat
        done;
        read_pattern_pat := true::!read_pattern_pat
      end
  done;
  (Array.of_list (List.rev !read_pattern_pref),
   Array.of_list (List.rev !read_pattern_pat))

let vertex_of_vdecl t vdecl =
  match t.task_kind with
  | StdTask -> NodeVar (vdecl.var_id, t.task_id)
  | Sensor | Actuator -> Var vdecl.var_id

(* Returns the initial value of a precedence annotation (if any). Assumes the
   same init value is used for each fby/concat in annots *)
let rec init_of_annots annots =
  match annots with
  | [] -> None
  | (Gfby cst)::_ -> Some cst
  | (Gconcat cst)::_ -> Some cst
  | _::rest -> init_of_annots rest

(* Computes the communication protocol of a task output *)
let proto_output g task_set t vout =
  let vertex_out_id = vertex_of_vdecl t vout in
  let vertex_out = Hashtbl.find g.graph vertex_out_id in
  let succs = vertex_out.vertex_succs in
  Hashtbl.fold
    (fun succ_id annot protos ->
      let task_from = task_of_vertex task_set vertex_out_id in
      let task_to = task_of_vertex task_set succ_id in
      let bsize = nb_cells task_from annot task_to in
      let pref,pat = write_pattern task_from annot task_to in
      let init = init_of_annots annot in
      let proto = {wbuf_size = bsize;
                   write_pref = pref;
                   write_pat = pat;
                   wbuf_init = init} in
      let succ_ref = vref_of_vertex_id succ_id in
      (succ_ref,proto)::protos)
    succs []

(* Computes the communication protocol of a task input *)
let proto_input g task_set t vin =
  let vertex_in_id = vertex_of_vdecl t vin in
  let vertex_in = Hashtbl.find g.graph vertex_in_id in
  let pred = vertex_in.vertex_pred in
  match pred with
  | None -> failwith "internal error"
  | Some (vpred,annot) ->
      let task_from = task_of_vertex task_set vpred in
      let task_to = task_of_vertex task_set vertex_in_id in
      let bsize = nb_cells task_from annot task_to in
      let pref,pat = read_pattern task_from annot task_to in
      let init = init_of_annots annot in
      let proto = {rbuf_size = bsize;
                   change_pref = pref;
                   change_pat = pat;
                   rbuf_init = init} in
      let pred_ref = vref_of_vertex_id vpred in
      pred_ref, proto

(* Computes the communication protocols for each variable of a task *)
let proto_task g task_set t =
  t.task_inputs <-
    List.map (fun (vdecl,_,_) ->
      let pred, proto = proto_input g task_set t vdecl in
      (vdecl, pred, proto)) t.task_inputs;
  t.task_outputs <-
    List.map (fun (vdecl,_) -> (vdecl,proto_output g task_set t vdecl)) t.task_outputs

(* Computes all the communication protocols of the task set *)
let proto_prog g task_set =
  Hashtbl.iter (fun _ t -> proto_task g task_set t) task_set

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
