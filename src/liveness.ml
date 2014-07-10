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
open LustreSpec
open Corelang
open Graph
open Causality

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

(* checks whether a variable is aliasable,
   depending on its (address) type *)
let is_aliasable var =
 Types.is_address_type var.var_type

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
   and returns [locals] and [evaluated] such roots
   - [locals] is the set of potentially reusable variables
   - [evaluated] is the set of already evaluated variables,
     wrt the scheduling
*)
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

type context =
{
  mutable evaluated : Disjunction.CISet.t;
  mutable quasi : Disjunction.CISet.t;
  mutable reusable : Disjunction.CISet.t;
  disjoint : (ident, Disjunction.CISet.t) Hashtbl.t;
  policy : (ident, var_decl) Hashtbl.t;
}

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
    Format.fprintf fmt "quasi=%a;@." Disjunction.pp_ciset ctx.quasi;
    Format.fprintf fmt "reusable=%a;@." Disjunction.pp_ciset ctx.reusable;
    Format.fprintf fmt "disjoint=%a;@." Disjunction.pp_disjoint_map ctx.disjoint;
    Format.fprintf fmt "policy=%a;@." pp_reuse_policy ctx.policy;
    Format.fprintf fmt "/* END context */ }@.";
  end

let is_reusable_quasi var ctx q =
  (*Log.report ~level:6 (fun fmt -> Format.fprintf fmt "is_reusable_quasi@ var=%s %a q=%s@." var.var_id pp_context ctx q.var_id);*)
  let disjoint = Hashtbl.find ctx.disjoint var.var_id in
  let q = Hashtbl.find ctx.policy q.var_id in
  Disjunction.CISet.for_all
    (fun v -> (Hashtbl.find ctx.policy v.var_id = q) <= (Disjunction.CISet.mem v disjoint || Disjunction.CISet.mem v ctx.quasi))
    ctx.evaluated

let compute_reusable heads var ctx =
  let (reusable', quasi') = Disjunction.CISet.partition (fun q -> (not (List.mem q heads)) && is_reusable_quasi var ctx q) ctx.quasi
  in
  begin
    ctx.quasi <- quasi';
    ctx.reusable <- Disjunction.CISet.fold (fun r' -> Disjunction.CISet.add (Hashtbl.find ctx.policy r'.var_id)) reusable' ctx.reusable;
    ctx.quasi <- Disjunction.CISet.diff ctx.quasi reusable';
    ctx.evaluated <- Disjunction.CISet.diff ctx.evaluated reusable';
  end

(* computes the reusable dependencies of variable [var] in graph [g],
   once [var] has been evaluated
   - [locals] is the set of potentially reusable variables
   - [evaluated] is the set of evaluated variables
   - [quasi] is the set of quasi-reusable variables
   - [reusable] is the set of dead/reusable dependencies of [var] in graph [g]
   - [policy] is the reuse map (which domain is [evaluated])
*)
let compute_dependencies locals heads ctx g =
  begin
    (*Log.report ~level:6 (fun fmt -> Format.fprintf fmt "compute_reusable_dependencies %a %a %a %a@." Disjunction.pp_ciset locals Printers.pp_var_name var pp_context ctx pp_dep_graph g);*)
    List.iter (fun head -> IdentDepGraph.iter_succ (IdentDepGraph.remove_edge g head.var_id) g head.var_id) heads;
    ctx.quasi <- Disjunction.CISet.union (remove_local_roots locals ctx.evaluated g) ctx.quasi;
    List.iter (fun head -> compute_reusable heads head ctx) heads;
  end

let compute_evaluated heads ctx =
  begin
    List.iter (fun head -> ctx.evaluated <- Disjunction.CISet.add head ctx.evaluated) heads;
  end

let compute_reuse node var ctx g =
  let aliasable = is_aliasable_input node var.var_id in
  let eligible v = Typing.eq_ground var.var_type v.var_type && not (aliasable v) in
  try
    let disj = Hashtbl.find ctx.disjoint var.var_id in
    let reuse =
      Hashtbl.find ctx.policy
	(Disjunction.CISet.max_elt (Disjunction.CISet.filter (fun v -> (eligible v) && (Disjunction.CISet.mem v ctx.evaluated) && not (Disjunction.CISet.mem v ctx.reusable)) disj)).var_id in
    begin
      ctx.evaluated <- Disjunction.CISet.add var ctx.evaluated;
      Hashtbl.add ctx.policy var.var_id reuse;
    end
  with Not_found ->
  try
    let reuse = Hashtbl.find ctx.policy (Disjunction.CISet.choose (Disjunction.CISet.filter (fun v -> eligible v) ctx.reusable)).var_id in
    begin
      replace_in_dep_graph var.var_id reuse.var_id g;
      Disjunction.replace_in_disjoint_map ctx.disjoint var reuse;
      ctx.evaluated <- Disjunction.CISet.add reuse ctx.evaluated;
      ctx.reusable <- Disjunction.CISet.remove reuse ctx.reusable;
      Hashtbl.add ctx.policy var.var_id reuse;
    end
      with Not_found ->
    begin
      ctx.evaluated <- Disjunction.CISet.add var ctx.evaluated;
      Hashtbl.add ctx.policy var.var_id var;
    end

let compute_reuse_policy node schedule disjoint g =
  let locals = node_reusable_variables node in
  let sort = ref schedule in
  let ctx = { evaluated = Disjunction.CISet.empty;
	      quasi     = Disjunction.CISet.empty;
	      reusable  = Disjunction.CISet.empty;
	      disjoint  = disjoint;
	      policy    = Hashtbl.create 23; } in
  while !sort <> []
  do
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "new context:%a@." pp_context ctx);
    Log.report ~level:6 
      (fun fmt -> Format.fprintf fmt "new dependency graph:%a@." pp_dep_graph g);
    let heads = List.map (fun v -> get_node_var v node) (List.hd !sort) in
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "NEW HEADS:");
    List.iter (fun head -> Log.report ~level:2 (fun fmt -> Format.fprintf fmt "%s " head.var_id)) heads;
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "@.");
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "COMPUTE_DEPENDENCIES@.");
    compute_dependencies locals heads ctx g;
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "new context:%a@." pp_context ctx);
    Log.report ~level:6 
      (fun fmt -> Format.fprintf fmt "new dependency graph:%a@." pp_dep_graph g);
    Log.report ~level:6 (fun fmt -> Format.fprintf fmt "COMPUTE_REUSE@.");
    List.iter (fun head -> compute_reuse node head ctx g) heads;
    List.iter (fun head -> Log.report ~level:6 (fun fmt -> Format.fprintf fmt "reuse %s instead of %s@." (Hashtbl.find ctx.policy head.var_id).var_id head.var_id)) heads;
    sort := List.tl !sort;
  done;
  IdentDepGraph.clear g;
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

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
