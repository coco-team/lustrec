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

type schedule_report =
{
  (* a schedule computed wrt the dependency graph *)
  schedule : ident list list;
  (* the set of unused variables (no output or mem depends on them) *)
  unused_vars : ISet.t;
  (* the table mapping each local var to its in-degree *)
  fanin_table : (ident, int) Hashtbl.t;
  (* the table mapping each assignment to a reusable variable *)
  reuse_table : (ident, var_decl) Hashtbl.t
}

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

(* Checks whether the currently scheduled variable [choice]
   is an output of a call, possibly among others *)
let is_call_output choice g =
  List.for_all ExprDep.is_instance_var (IdentDepGraph.succ g choice)

(* Adds successors of [v] in graph [g] in [pending] or [frontier] sets, wrt [eq_equiv],
   then removes [v] from [g] 
*)
let add_successors eq_equiv g v pending frontier =
  let succs_v = IdentDepGraph.succ g v in
  begin
    IdentDepGraph.remove_vertex g v;
    List.iter 
      (fun v' -> 
	if is_graph_root v' g then 
	  (if eq_equiv v v' then 
	      pending := ISet.add v' !pending 
	   else
	      frontier := ISet.add v' !frontier)
      ) succs_v;
  end

(* Chooses the next var to be sorted, taking priority into account.
   Modifies [pending] and [frontier] accordingly.
*)
let next_element eq_equiv g sort call pending frontier =
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
	call := is_call_output choice g;
	add_successors eq_equiv g choice pending frontier;
	if not (ExprDep.is_ghost_var choice)
	then sort := [choice] :: !sort
      end
    else
      begin
	let choice = ISet.min_elt !pending in
      (*Format.eprintf "-2-> %s@." choice;*)
	pending := ISet.remove choice !pending;
	add_successors eq_equiv g choice pending frontier;
	if not (ExprDep.is_ghost_var choice)
	then sort := (if !call
		      then (choice :: List.hd !sort) :: List.tl !sort
		      else [choice] :: !sort)
      end
  end


(* Topological sort of dependency graph [g], with priority.
 *)
let topological_sort eq_equiv g =
  let roots = graph_roots g in
  assert (roots <> []);
  let call = ref false in
  let frontier = ref (List.fold_right ISet.add roots ISet.empty) in
  let pending = ref ISet.empty in
  let sorted = ref [] in
  begin
    while not (ISet.is_empty !frontier && ISet.is_empty !pending)
    do
      (*Format.eprintf "frontier = {%a}, pending = {%a}@."
	(fun fmt -> ISet.iter (fun e -> Format.pp_print_string fmt e)) !frontier
	(fun fmt -> ISet.iter (fun e -> Format.pp_print_string fmt e)) !pending;*)
      next_element eq_equiv g sorted call pending frontier;
    done;
    IdentDepGraph.clear g;
    !sorted
  end

let schedule_node n =
  try
    let eq_equiv = ExprDep.node_eq_equiv n in
    let eq_equiv v1 v2 =
      try
	Hashtbl.find eq_equiv v1 = Hashtbl.find eq_equiv v2
      with Not_found -> false in

    let n', g = global_dependency n in
    Log.report ~level:5 
      (fun fmt -> 
	Format.fprintf fmt
	  "dependency graph for node %s: %a" 
	  n'.node_id
	  pp_dep_graph g
      );
    
    (* TODO X: extend the graph with inputs (adapt the causality analysis to deal with inputs
     compute: coi predecessors of outputs
     warning (no modification) when memories are non used (do not impact output) or when inputs are not used (do not impact output)
       DONE !
     *)

    let gg = IdentDepGraph.copy g in
    let sort = topological_sort eq_equiv g in
    let unused = Liveness.compute_unused_variables n gg in
    let fanin = Liveness.compute_fanin n gg in

    let disjoint = Disjunction.clock_disjoint_map (get_node_vars n) in
    
    Log.report ~level:2 
      (fun fmt -> 
	Format.fprintf fmt
	  "clock disjoint map for node %s: %a" 
	  n'.node_id
	  Disjunction.pp_disjoint_map disjoint
      );

    let reuse = Hashtbl.create 23 (*Liveness.compute_reuse_policy n sort disjoint gg*) in
    Log.report ~level:2 
      (fun fmt -> 
	Format.fprintf fmt
	  "reuse policy for node %s: %a" 
	  n'.node_id
	  Liveness.pp_reuse_policy reuse
      );
 
    n', { schedule = sort; unused_vars = unused; fanin_table = fanin; reuse_table = reuse }
  with (Causality.Cycle v) as exc ->
    pp_error Format.err_formatter v;
    raise exc

let schedule_prog prog =
  List.fold_right (
    fun top_decl (accu_prog, sch_map)  ->
      match top_decl.top_decl_desc with
	| Node nd -> 
	  let nd', report = schedule_node nd in
	  {top_decl with top_decl_desc = Node nd'}::accu_prog, 
	  IMap.add nd.node_id report sch_map
	| _ -> top_decl::accu_prog, sch_map
    ) 
    prog
    ([],IMap.empty)

let pp_eq_schedule fmt vl =
  match vl with
  | []  -> assert false
  | [v] -> Format.fprintf fmt "%s" v
  | _   -> Format.fprintf fmt "(%a)" (fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) vl
 
let pp_schedule fmt node_schs =
 IMap.iter
   (fun nd report ->
     Format.fprintf fmt "%s schedule: %a@."
       nd
       (fprintf_list ~sep:" ; " pp_eq_schedule) report.schedule)
   node_schs

let pp_fanin_table fmt node_schs =
  IMap.iter
    (fun nd report ->
      Format.fprintf fmt "%s : %a" nd Liveness.pp_fanin report.fanin_table)
    node_schs

let pp_warning_unused fmt node_schs =
 IMap.iter
   (fun nd report ->
     let unused = report.unused_vars in
     if not (ISet.is_empty unused)
     then
       let nd = match (Corelang.node_from_name nd).top_decl_desc with Node nd -> nd | _ -> assert false in
       ISet.iter
	 (fun u -> 
	   Format.fprintf fmt "Warning: variable '%s' seems unused@.%a@."
	     u
	     Location.pp_loc (get_node_var u nd).var_loc)
	 unused
   )
   node_schs

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
