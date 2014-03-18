(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2013, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 * Copyright (C) 2012-2013, INPT, Toulouse, FRANCE
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

(* This module is used for the lustre to C compiler *)

open Utils
open LustreSpec
open Corelang
open Graph
open Causality


(* Tests whether [v] is a root of graph [g], i.e. a source *)
let is_graph_root v g =
 IdentDepGraph.in_degree g v = 0

(* Computes the set of graph roots, i.e. the sources of acyclic graph [g] *)
let graph_roots g =
 IdentDepGraph.fold_vertex
   (fun v roots -> if is_graph_root v g then v::roots else roots)
   g []

(* Topological sort with a priority for variables belonging in the same equation lhs.
   For variables still unrelated, standard compare is used to choose the minimal element.
   This priority is used since it helps a lot in factorizing generated code.
   In the following functions:
   - [eq_equiv] is the equivalence relation between vars of the same equation lhs
   - [g] the (imperative) graph to be topologically sorted
   - [pending] is the set of unsorted root variables so far, equivalent to the last sorted var
   - [frontier] is the set of unsorted root variables so far, not belonging in [pending]
   - [sort] is the resulting topological order
*)
(* Adds successors of [v] in graph [g] in [pending] or [frontier] sets, wrt [eq_equiv],
   then removes [v] from [g] 
*)
let add_successors eq_equiv g v pending frontier =
  let succs_v = IdentDepGraph.succ g v in
  begin
    IdentDepGraph.remove_vertex g v;
    List.iter (fun v' -> if is_graph_root v' g then (if eq_equiv v v' then pending := ISet.add v' !pending else frontier := ISet.add v' !frontier)) succs_v;
  end

(* Chooses the next var to be sorted, taking priority into account.
   Modifies [pending] and [frontier] accordingly.
*)
let next_element eq_equiv g sort pending frontier =
  begin
    if ISet.is_empty !pending
    then
      begin
	let choice = ISet.min_elt !frontier in
      (*Format.eprintf "-1-> %s@." choice;*)
	frontier := ISet.remove choice !frontier;
	let (p, f) = ISet.partition (eq_equiv choice) !frontier in
	pending := p;
	frontier := f;
	add_successors eq_equiv g choice pending frontier;
	if not (ExprDep.is_instance_var choice) then sort := choice :: !sort
      end
    else
      begin
	let choice = ISet.min_elt !pending in
      (*Format.eprintf "-2-> %s@." choice;*)
	pending := ISet.remove choice !pending;
	add_successors eq_equiv g choice pending frontier;
	if not (ExprDep.is_instance_var choice) then sort := choice :: !sort
      end
  end


(* Topological sort of dependency graph [g], with priority.
 *)
let topological_sort eq_equiv g =
  let roots = graph_roots g in
  assert (roots <> []);
  let frontier = ref (List.fold_right ISet.add roots ISet.empty) in
  let pending = ref ISet.empty in
  let sorted = ref [] in
  begin
    while not (ISet.is_empty !frontier && ISet.is_empty !pending)
    do
      (*Format.eprintf "frontier = {%a}, pending = {%a}@."
	(fun fmt -> ISet.iter (fun e -> Format.pp_print_string fmt e)) !frontier
	(fun fmt -> ISet.iter (fun e -> Format.pp_print_string fmt e)) !pending;*)
      next_element eq_equiv g sorted pending frontier;
    done;
    IdentDepGraph.clear g;
    !sorted
  end

(* Computes the last dependency
*)

(* Computes the death table of [node] wrt dep graph [g] and topological [sort].
   The death table is a mapping: ident -> Set(ident) such that:
   death x is the set of local variables which get dead (i.e. unused) 
   after x is evaluated, but were until live.
*)
let death_table node g sort =
  let death = Hashtbl.create 23 in
  let sort  = ref (List.rev sort) in
  let buried  = ref ISet.empty in
  begin
    buried := ExprDep.node_memory_variables node;
    buried := List.fold_left (fun dead (v : var_decl) -> ISet.add v.var_id dead) !buried node.node_outputs;
    (* We could also try to reuse input variables, due to C parameter copying semantics *)
    buried := List.fold_left (fun dead (v : var_decl) -> ISet.add v.var_id dead) !buried node.node_inputs;
    while (!sort <> [])
    do
      let head = List.hd !sort in
      let dead = IdentDepGraph.fold_succ
	(fun tgt dead -> if not (ExprDep.is_instance_var tgt || ISet.mem tgt !buried) then ISet.add tgt dead else dead)
	g head ISet.empty in
      buried := ISet.union !buried dead;
      Hashtbl.add death head dead;
      sort := List.tl !sort
    done;
    IdentDepGraph.clear g;
    death
  end

let pp_death_table fmt death =
  begin
    Format.fprintf fmt "{ /* death table */@.";
    Hashtbl.iter (fun s t -> Format.fprintf fmt "%s -> { %a }@." s (Utils.fprintf_list ~sep:", " Format.pp_print_string) (ISet.elements t)) death;
    Format.fprintf fmt "}@."
  end

let schedule_node n  =
  try
    let eq_equiv = ExprDep.node_eq_equiv n in
    let eq_equiv v1 v2 =
      try
	Hashtbl.find eq_equiv v1 = Hashtbl.find eq_equiv v2
      with Not_found -> false in
    let n', g = global_dependency n in
    Log.report ~level:5 (fun fmt -> Format.eprintf "dependency graph for node %s: %a" n'.node_id pp_dep_graph g);
    let gg = IdentDepGraph.copy g in
    let sort = topological_sort eq_equiv g in
    let death = death_table n gg sort in
    Log.report ~level:5 (fun fmt -> Format.eprintf "death table for node %s: %a" n'.node_id pp_death_table death);
    n', sort, death
(* let sorted = TopologicalDepGraph.fold (fun x res -> if ExprDep.is_instance_var x then res else x::res) g []*)
  with (Causality.Cycle v) as exc ->
    pp_error Format.err_formatter v;
    raise exc


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
