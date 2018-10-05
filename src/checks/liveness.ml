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

type context =
{
  mutable evaluated : Disjunction.CISet.t;
  dep_graph : IdentDepGraph.t;
  disjoint : (ident, Disjunction.CISet.t) Hashtbl.t;
  policy : (ident, var_decl) Hashtbl.t;
}

(* computes the in-degree for each local variable of node [n], according to dep graph [g].
*)
let compute_fanin n g =
  let locals = ISet.diff (ExprDep.node_local_variables n) (ExprDep.node_memory_variables n) in
  let inputs = ExprDep.node_input_variables n in
  let fanin = Hashtbl.create 23 in
  begin
    IdentDepGraph.iter_vertex
      (fun v ->
	if ISet.mem v locals
	then Hashtbl.add fanin v (IdentDepGraph.in_degree g v) else
	if ExprDep.is_read_var v && not (ISet.mem v inputs)
	then Hashtbl.add fanin (ExprDep.undo_read_var v) (IdentDepGraph.in_degree g v)) g;
    fanin
  end
 
let pp_fanin fmt fanin =
  begin
    Format.fprintf fmt "{ /* locals fanin: */@.";
    Hashtbl.iter (fun s t -> Format.fprintf fmt "%s -> %d@." s t) fanin;
    Format.fprintf fmt "}@."
  end

(* computes the cone of influence of a given [var] wrt a dependency graph [g].
*)
let cone_of_influence g var =
 (*Format.printf "coi: %s@." var;*)
 let frontier = ref (ISet.add var ISet.empty) in
 let coi = ref ISet.empty in
 while not (ISet.is_empty !frontier)
 do
   let head = ISet.min_elt !frontier in
   (*Format.printf "head: %s@." head;*)
   frontier := ISet.remove head !frontier;
   if ExprDep.is_read_var head then coi := ISet.add (ExprDep.undo_read_var head) !coi;
   List.iter (fun s -> frontier := ISet.add s !frontier) (IdentDepGraph.succ g head);
 done;
 !coi

let compute_unused_variables n g =
  let inputs = ExprDep.node_input_variables n in
  let mems = ExprDep.node_memory_variables n in
  let outputs = ExprDep.node_output_variables n in
  ISet.fold
    (fun var unused -> ISet.diff unused (cone_of_influence g var))
    (ISet.union outputs mems)
    (ISet.union inputs mems)

(* computes the set of potentially reusable variables.
   We don't reuse input variables, due to possible aliasing *)
let node_reusable_variables node =
  let mems = ExprDep.node_memory_variables node in
  List.fold_left
    (fun acc l ->
      if ISet.mem l.var_id mems then acc else Disjunction.CISet.add l acc)
    Disjunction.CISet.empty
    node.node_locals

let kill_instance_variables ctx inst =
  IdentDepGraph.remove_vertex ctx.dep_graph inst

let kill_root ctx head =
  IdentDepGraph.iter_succ (IdentDepGraph.remove_edge ctx.dep_graph head.var_id) ctx.dep_graph head.var_id

(* Recursively removes useless variables,
   i.e. [ctx.evaluated] variables that are current roots of the dep graph [ctx.dep_graph]
   - [evaluated] is the set of already evaluated variables,
     wrt the scheduling
   - does only remove edges, not variables themselves
   - yet, instance variables are removed
*)
let remove_roots ctx =
  let rem = ref true in
  let remaining = ref ctx.evaluated in
  while !rem
  do
    rem := false;
    let all_roots = graph_roots ctx.dep_graph in
    let inst_roots, var_roots = List.partition (fun v -> ExprDep.is_instance_var v && v <> Causality.world) all_roots in
    let frontier_roots = Disjunction.CISet.filter (fun v -> List.mem v.var_id var_roots) !remaining in
    if not (Disjunction.CISet.is_empty frontier_roots && inst_roots = []) then
      begin
	rem := true;
	List.iter (kill_instance_variables ctx) inst_roots;
	Disjunction.CISet.iter (kill_root ctx) frontier_roots;
	remaining := Disjunction.CISet.diff !remaining frontier_roots
      end
  done
 
(* checks whether a variable is aliasable,
   depending on its (address) type *)
let is_aliasable var =
  (not (!Options.mpfr && Types.is_real_type var.var_type)) && Types.is_address_type var.var_type
 
(* checks whether a variable [v] is an input of the [var] equation, with an address type.
   if so, [var] could not safely reuse/alias [v], should [v] be dead in the caller node,
   because [v] may not be dead in the callee node when [var] is assigned *)
let is_aliasable_input node var =
  let eq_var = get_node_eq var node in
  let inputs_var =
    match NodeDep.get_callee eq_var.eq_rhs with
    | None           -> []
    | Some (_, args) -> List.fold_right (fun e r -> match e.expr_desc with Expr_ident id -> id::r | _ -> r) args [] in
  fun v -> is_aliasable v && List.mem v.var_id inputs_var

(* replace variable [v] by [v'] in graph [g].
   [v'] is a dead variable
*)
let replace_in_dep_graph v v' g =
  begin
    IdentDepGraph.add_vertex g v';
    IdentDepGraph.iter_succ (fun s -> IdentDepGraph.add_edge g v' s) g v;
    IdentDepGraph.iter_pred (fun p -> IdentDepGraph.add_edge g p v') g v;
    IdentDepGraph.remove_vertex g v
  end

let pp_reuse_policy fmt policy =
  begin
    Format.fprintf fmt "{ /* reuse policy */@.";
    Hashtbl.iter (fun s t -> Format.fprintf fmt "%s -> %s@." s t.var_id) policy;
    Format.fprintf fmt "}@."
  end

let pp_context fmt ctx =
  begin
    Format.fprintf fmt "{ /*BEGIN context */@.";
    Format.fprintf fmt "eval=%a;@." Disjunction.pp_ciset ctx.evaluated;
    Format.fprintf fmt "graph=%a;@." pp_dep_graph ctx.dep_graph;
    Format.fprintf fmt "disjoint=%a;@." Disjunction.pp_disjoint_map ctx.disjoint;
    Format.fprintf fmt "policy=%a;@." pp_reuse_policy ctx.policy;
    Format.fprintf fmt "/* END context */ }@.";
  end

(* computes the reusable dependencies of variable [var] in graph [g],
   once [var] has been evaluated
   - [locals] is the set of potentially reusable variables
   - [evaluated] is the set of evaluated variables
   - [quasi] is the set of quasi-reusable variables
   - [reusable] is the set of dead/reusable dependencies of [var] in graph [g]
   - [policy] is the reuse map (which domain is [evaluated])
*)
let compute_dependencies heads ctx =
  begin
    (*Log.report ~level:6 (fun fmt -> Format.fprintf fmt "compute_reusable_dependencies %a %a %a@." Disjunction.pp_ciset locals Printers.pp_var_name var pp_context ctx);*)
    List.iter (kill_root ctx) heads;
    remove_roots ctx;
  end

let compute_evaluated heads ctx =
  begin
    List.iter (fun head -> ctx.evaluated <- Disjunction.CISet.add head ctx.evaluated) heads;
  end

(* tests whether a variable [v] may be (re)used instead of [var]. The conditions are:
   - [v] has been really used ([v] is its own representative)
   - same type
   - [v] is not an aliasable input of the equation defining [var]
   - [v] is not one of the current heads (which contain [var])
   - [v] is not currently in use
 *)
let eligible node ctx heads var v =
     Hashtbl.find ctx.policy v.var_id == v
  && Typing.eq_ground (Types.unclock_type var.var_type) (Types.unclock_type v.var_type)
  && not (is_aliasable_input node var.var_id v)
  && not (List.exists (fun h -> h.var_id = v.var_id) heads)
  && (*let repr_v = Hashtbl.find ctx.policy v.var_id*)
     not (Disjunction.CISet.exists (fun p -> IdentDepGraph.mem_edge ctx.dep_graph p.var_id v.var_id) ctx.evaluated)

let compute_reuse node ctx heads var =
  let disjoint = Hashtbl.find ctx.disjoint var.var_id in
  let locally_reusable v =
    IdentDepGraph.fold_pred (fun p r -> r && Disjunction.CISet.exists (fun d -> p = d.var_id) disjoint) ctx.dep_graph v.var_id true in
  let eligibles = Disjunction.CISet.filter (eligible node ctx heads var) ctx.evaluated in
  Log.report ~level:7 (fun fmt -> Format.fprintf fmt "eligibles:%a@." Disjunction.pp_ciset eligibles);
  let quasi_dead, live = Disjunction.CISet.partition locally_reusable eligibles in
  Log.report ~level:7 (fun fmt -> Format.fprintf fmt "live:%a@." Disjunction.pp_ciset live);
  try
    let disjoint_live = Disjunction.CISet.inter disjoint live in
    Log.report ~level:7 (fun fmt -> Format.fprintf fmt "disjoint live:%a@." Disjunction.pp_ciset disjoint_live);
    let reuse = Disjunction.CISet.max_elt disjoint_live in
    begin
      IdentDepGraph.add_edge ctx.dep_graph var.var_id reuse.var_id;
      Hashtbl.add ctx.policy var.var_id reuse;
      ctx.evaluated <- Disjunction.CISet.add var ctx.evaluated;
      (*Format.eprintf "%s reused by live@." var.var_id;*)
    end
  with Not_found ->
  try
    let dead = Disjunction.CISet.filter (fun v -> is_graph_root v.var_id ctx.dep_graph) quasi_dead in
    Log.report ~level:7 (fun fmt -> Format.fprintf fmt "dead:%a@." Disjunction.pp_ciset dead);
    let reuse = Disjunction.CISet.choose dead in
    begin
      IdentDepGraph.add_edge ctx.dep_graph var.var_id reuse.var_id;
      Hashtbl.add ctx.policy var.var_id reuse;
      ctx.evaluated <- Disjunction.CISet.add var ctx.evaluated;
      (*Format.eprintf "%s reused by dead %s@." var.var_id reuse.var_id;*)
    end
      with Not_found ->
    begin
      Hashtbl.add ctx.policy var.var_id var;
      ctx.evaluated <- Disjunction.CISet.add var ctx.evaluated;
    end

let compute_reuse_policy node schedule disjoint g =
  let sort = ref schedule in
  let ctx = { evaluated = Disjunction.CISet.empty;
	      dep_graph = g;
	      disjoint  = disjoint;
	      policy    = Hashtbl.create 23; } in
  while !sort <> []
  do
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "new context:%a@." pp_context ctx);
    let heads = List.map (fun v -> get_node_var v node) (List.hd !sort) in
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "NEW HEADS:");
    List.iter (fun head -> Log.report ~level:6 (fun fmt -> Format.fprintf fmt "%s (%a)" head.var_id Printers.pp_node_eq (get_node_eq head.var_id node))) heads;
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "@.");
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "COMPUTE_DEPENDENCIES@.");
    compute_dependencies heads ctx;
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "new context:%a@." pp_context ctx);
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "COMPUTE_REUSE@.");
    List.iter (compute_reuse node ctx heads) heads;
    (*compute_evaluated heads ctx;*)
    List.iter (fun head -> Log.report ~level:6 (fun fmt -> Format.fprintf fmt "reuse %s instead of %s@." (Hashtbl.find ctx.policy head.var_id).var_id head.var_id)) heads;
    sort := List.tl !sort;
  done;
  IdentDepGraph.clear ctx.dep_graph;
  ctx.policy

(* Reuse policy:
   - could reuse variables with the same type exactly only (simple).
   - reusing variables with different types would involve:
     - either dirty castings
     - or complex inclusion expression (for instance: array <-> array cell, struct <-> struct field) to be able to reuse only some parts of structured data.
     ... it seems too complex and potentially unsafe
   - for node instance calls: output variables could NOT reuse aliasable input variables, 
     even if inputs become dead, because the correctness would depend on the scheduling
     of the callee (so, the compiling strategy could NOT be modular anymore).
   - once a policy is set, we need to:
     - replace each variable by its reuse alias.
     - simplify resulting equations, as we may now have:
        x = x;                     --> ;           for scalar vars
       or:
        x = &{ f1 = x->f1; f2 = t; } --> x->f2 = t;   for struct vars
 *)


(* the reuse policy seeks to use less local variables
   by replacing local variables, applying the rules
   in the following order:
    1) use another clock disjoint still live variable,
       with the greatest possible disjoint clock
    2) reuse a dead variable
   For the sake of safety, we replace variables by others:
    - with the same type
    - not aliasable (i.e. address type)
*)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
