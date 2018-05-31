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

open Utils
open Lustre_types
open Corelang
open Graph
open Causality

type schedule_report =
{
  (* the scheduled node *)
  node : node_desc;
  (* a schedule computed wrt the dependency graph *)
  schedule : ident list list;
  (* the set of unused variables (no output or mem depends on them) *)
  unused_vars : ISet.t;
  (* the table mapping each local var to its in-degree *)
  fanin_table : (ident, int) Hashtbl.t;
  (* the dependency graph *)
  dep_graph   : IdentDepGraph.t;
  (* the table mapping each assignment to a reusable variable *)
  (*reuse_table : (ident, var_decl) Hashtbl.t*)
}

(* Topological sort with a priority for variables belonging in the same equation lhs.
   For variables still unrelated, standard compare is used to choose the minimal element.
   This priority is used since it helps a lot in factorizing generated code.
   Moreover, the dependency graph is browsed in a depth-first manner whenever possible,
   to improve the behavior of optimization algorithms applied in forthcoming compilation steps. 
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
 List.exists ExprDep.is_instance_var (IdentDepGraph.succ g choice)

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

(* Filters out normalization variables and renames instance variables to keep things readable,
   in a case of a dependency error *)
let filter_original n vl =
 List.fold_right (fun v res ->
   if ExprDep.is_instance_var v then Format.sprintf "node %s" (ExprDep.undo_instance_var v) :: res else
   let vdecl = get_node_var v n in
   if vdecl.var_orig then v :: res else res) vl []

let schedule_node n =
  (* let node_vars = get_node_vars n in *)
  let eq_equiv = ExprDep.node_eq_equiv n in
  let eq_equiv v1 v2 =
    try
      Hashtbl.find eq_equiv v1 = Hashtbl.find eq_equiv v2
    with Not_found -> false in

  let n', g = global_dependency n in
  
  (* TODO X: extend the graph with inputs (adapt the causality analysis to deal with inputs
     compute: coi predecessors of outputs
     warning (no modification) when memories are non used (do not impact output) or when inputs are not used (do not impact output)
     DONE !
  *)

  let gg = IdentDepGraph.copy g in
  let sort = topological_sort eq_equiv g in
  let unused = Liveness.compute_unused_variables n gg in
  let fanin = Liveness.compute_fanin n gg in
  { node = n'; schedule = sort; unused_vars = unused; fanin_table = fanin; dep_graph = gg; }


let compute_node_reuse_table report =
  let disjoint = Disjunction.clock_disjoint_map (get_node_vars report.node) in
  let reuse = Liveness.compute_reuse_policy report.node report.schedule disjoint report.dep_graph in
(*
    if !Options.print_reuse
    then
      begin
	Log.report ~level:0 
	  (fun fmt -> 
	    Format.fprintf fmt
	      "OPT:%B@." (try (Hashtbl.iter (fun s1 v2 -> if s1 = v2.var_id then raise Not_found) reuse; false) with Not_found -> true)
	  );
	Log.report ~level:0 
	  (fun fmt -> 
	    Format.fprintf fmt
	      "OPT:clock disjoint map for node %s: %a" 
	      n'.node_id
	      Disjunction.pp_disjoint_map disjoint
	  );
	Log.report ~level:0 
	  (fun fmt -> 
	    Format.fprintf fmt
	      "OPT:reuse policy for node %s: %a" 
	      n'.node_id
	      Liveness.pp_reuse_policy reuse
	  );
      end;
*)
    reuse


let schedule_prog prog =
  List.fold_right (
    fun top_decl (accu_prog, sch_map)  ->
      match top_decl.top_decl_desc with
      | Node nd ->
	let report = schedule_node nd in
	{top_decl with top_decl_desc = Node report.node}::accu_prog, 
	IMap.add nd.node_id report sch_map
	| _ -> top_decl::accu_prog, sch_map
    ) 
    prog
    ([],IMap.empty)
  

let compute_prog_reuse_table report =
  IMap.map compute_node_reuse_table report

(* removes inlined local variables from schedule report, 
   which are now useless *)
let remove_node_inlined_locals locals report =
  let is_inlined v = IMap.exists (fun l _ -> v = l) locals in
  let schedule' =
    List.fold_right (fun heads q -> let heads' = List.filter (fun v -> not (is_inlined v)) heads
				    in if heads' = [] then q else heads'::q)
      report.schedule [] in
  begin
    IMap.iter (fun v _ -> Hashtbl.remove report.fanin_table v) locals;
    IMap.iter (fun v _ -> let iv = ExprDep.mk_instance_var v
			  in Liveness.replace_in_dep_graph v iv report.dep_graph) locals;
    { report with schedule = schedule' }
  end

let remove_prog_inlined_locals removed reuse =
  IMap.mapi (fun id -> remove_node_inlined_locals (IMap.find id removed)) reuse

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
      Format.fprintf fmt "%s: %a" nd Liveness.pp_fanin report.fanin_table)
    node_schs

let pp_dep_graph fmt node_schs =
  IMap.iter
    (fun nd report ->
      Format.fprintf fmt "%s dependency graph: %a" nd pp_dep_graph report.dep_graph)
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
	   let vu = get_node_var u nd in
	   if vu.var_orig
	   then Format.fprintf fmt "  Warning: variable '%s' seems unused@,  %a@,@," u Location.pp_loc vu.var_loc)
	 unused
   )
   node_schs


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
