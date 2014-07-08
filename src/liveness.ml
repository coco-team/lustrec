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

open Utils
open LustreSpec
open Corelang
open Graph
open Causality

(* Computes the last dependency
*)

(* Computes the death table of [node] wrt dep graph [g] and topological [sort].
   The death table is a mapping: ident -> Set(ident) such that:
   death x is the set of local variables which get dead (i.e. unused) 
   after x is evaluated, but were until live.
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
*)

(* computes the in-degree for each local variable of node [n], according to dep graph [g].
*)
let compute_fanin n g =
  let locals = ISet.diff (ExprDep.node_local_variables n) (ExprDep.node_memory_variables n) in
  let fanin = Hashtbl.create 23 in
  begin
    IdentDepGraph.iter_vertex (fun v -> if ISet.mem v locals then Hashtbl.add fanin v (IdentDepGraph.in_degree g v)) g;
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

(* Recursively removes useless variables,
   i.e. variables that are current roots of the dep graph [g]
   and returns [locals] and [evaluated] such roots *)
let remove_local_roots locals evaluated g =
  let rem = ref true in
  let roots = ref Disjunction.CISet.empty in
  while !rem
  do
    rem := false;
    let new_roots = graph_roots g in
    let reusable_roots = Disjunction.CISet.filter (fun v -> (List.mem v.var_id new_roots) && (Disjunction.CISet.mem v locals)) evaluated in
    if not (Disjunction.CISet.is_empty reusable_roots) then
      begin
	rem := true;
	Disjunction.CISet.iter (fun v -> IdentDepGraph.remove_vertex g v.var_id) reusable_roots;
	roots := Disjunction.CISet.union reusable_roots !roots
      end
  done;
  !roots

(* checks whether a variable is aliasable,
   depending on its (address) type *)
let is_aliasable var =
 Types.is_address_type var.var_type
 
(* checks whether a variable [v] is an input of the [var] equation, with an address type.
   if so, [var] could not safely reuse/alias [v], should [v] be dead in the caller node,
   because [v] may not be dead in the callee node when [var] is assigned *)
let is_aliasable_input node var =
  let eq_var = get_node_eq var node in
  let inputs_var =
    match NodeDep.get_callee eq_var.eq_rhs with
    | None           -> []
    | Some (_, args) -> List.fold_right (fun e r -> match e.expr_desc with Expr_ident id -> id::r | _ -> r) args [] in
  fun v -> Types.is_address_type v.var_type && List.mem v.var_id inputs_var

(* merges two variables [v] and [v'] of graph [g].
   [v] is replaced by [v']
*)
let merge_in_dep_graph v v' g =
  begin
    IdentDepGraph.add_vertex g v';
    IdentDepGraph.iter_succ (fun s -> IdentDepGraph.add_edge g v' s) g v;
    IdentDepGraph.iter_pred (fun p -> IdentDepGraph.add_edge g p v') g v;
    IdentDepGraph.remove_vertex g v
  end

(* computes the reusable dependencies of variable [var] in graph [g],
   once [var] has been evaluated
   [dead] is the set of evaluated and dead variables
   [eval] is the set of evaluated variables
*)
let compute_reusable_dependencies locals evaluated reusable var g =
  begin
    Log.report ~level:2 (fun fmt -> Format.fprintf fmt "compute_reusable_dependencies %a %a %a %a %a@." Disjunction.pp_ciset locals Disjunction.pp_ciset !evaluated Disjunction.pp_ciset !reusable Printers.pp_var_name var pp_dep_graph g);
    evaluated := Disjunction.CISet.add var !evaluated;
    IdentDepGraph.iter_succ (IdentDepGraph.remove_edge g var.var_id) g var.var_id;
    reusable := Disjunction.CISet.union (remove_local_roots locals !evaluated g) !reusable;
  end

let compute_reuse_policy node schedule disjoint g =
  let locals = node_reusable_variables node in
  let sort = ref schedule in
  let evaluated = ref Disjunction.CISet.empty in
  let reusable = ref Disjunction.CISet.empty in
  let policy = Hashtbl.create 23 in
  while !sort <> []
  do
    let head = get_node_var (List.hd !sort) node in
    compute_reusable_dependencies locals evaluated reusable head g;
    let aliasable = is_aliasable_input node head.var_id in
    let eligible v = Typing.eq_ground head.var_type v.var_type && not (aliasable v) in
    let reuse =
      try
	let disj = Hashtbl.find disjoint head.var_id in
	Disjunction.CISet.max_elt (Disjunction.CISet.filter (fun v -> (eligible v) && (Disjunction.CISet.mem v !evaluated) && not (Disjunction.CISet.mem v !reusable)) disj)
      with Not_found ->
      try
	Disjunction.CISet.choose (Disjunction.CISet.filter (fun v -> eligible v) !reusable)
      with Not_found -> head in
    reusable := Disjunction.CISet.remove reuse !reusable;
    Disjunction.replace_in_disjoint_map disjoint head reuse;
    merge_in_dep_graph head.var_id reuse.var_id g;
    Hashtbl.add policy head.var_id reuse;
    Log.report ~level:2 (fun fmt -> Format.fprintf fmt "reuse %s instead of %s@." reuse.var_id head.var_id);
    Log.report ~level:1 (fun fmt -> Format.fprintf fmt "new disjoint:%a@." Disjunction.pp_disjoint_map disjoint);
    Log.report ~level:2 
      (fun fmt -> Format.fprintf fmt "new dependency graph:%a@." pp_dep_graph g);
    sort := List.tl !sort;
  done;
  IdentDepGraph.clear g;
  policy

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
     - such simplifications are, until now, only expressible at the C source level...
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

let pp_reuse_policy fmt policy =
  begin
    Format.fprintf fmt "{ /* reuse policy */@.";
    Hashtbl.iter (fun s t -> Format.fprintf fmt "%s -> %s@." s t.var_id) policy;
    Format.fprintf fmt "}@."
  end
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
