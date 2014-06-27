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

let compute_unused n g =
  let inputs = ExprDep.node_input_variables n in
  let mems = ExprDep.node_memory_variables n in
  let outputs = ExprDep.node_output_variables n in
  ISet.fold
    (fun var unused -> ISet.diff unused (cone_of_influence g var))
    (ISet.union outputs mems)
    (ISet.union inputs mems) 

(* Computes the set of (input and) output and mem variables of [node].
   We try to reuse input variables, due to C parameter copying semantics. *)
let node_non_locals node =
 List.fold_left (fun non_loc v -> ISet.add v.var_id non_loc) (ExprDep.node_memory_variables node) node.node_outputs

(* Recursively removes useless local variables,
   i.e. variables in [non_locals] that are current roots of the dep graph [g] *)
let remove_local_roots non_locals g =
  let rem = ref true in
  let roots = ref ISet.empty in
  while !rem
  do
    rem := false;
    let local_roots = List.filter (fun v -> not (ISet.mem v non_locals)) (graph_roots g) in
    if local_roots <> [] then
      begin
	rem := true;
	List.iter (IdentDepGraph.remove_vertex g) local_roots;
	roots := List.fold_left (fun roots v -> if ExprDep.is_instance_var v then roots else ISet.add v roots) !roots local_roots 
      end
  done;
  !roots

(* Computes the death table of [node] wrt dep graph [g] and topological [sort].
   The death table is a mapping: ident -> Set(ident) such that:
   death x is the set of local variables which get dead (i.e. unused) 
   before x is evaluated, but were until live.
   If death x is not defined, then x is useless.
*)
let death_table node g sort =
  let non_locals = node_non_locals node in
  let death = Hashtbl.create 23 in
  let sort  = ref sort in
  begin
    while (!sort <> [])
    do
      let head = List.hd !sort in
      (* If current var is not already dead, i.e. useless *)
      if IdentDepGraph.mem_vertex g head then
	begin
	  IdentDepGraph.iter_succ (IdentDepGraph.remove_edge g head) g head;
	  let dead = remove_local_roots non_locals g in
	  Hashtbl.add death head dead
	end;
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


(* Reuse policy:
   - could reuse variables with the same type exactly only (simple).
   - reusing variables with different types would involve:
     - either dirty castings
     - or complex inclusion expression (for instance: array <-> array cell, struct <-> struct field) to be able to reuse only some parts of structured data.
     ... it seems too complex and potentially unsafe
   - for node instance calls: output variables could NOT reuse input variables, 
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

(* Replaces [v] by [v'] in set [s] *)
let replace_in_set s v v' =
  if ISet.mem v s then ISet.add v' (ISet.remove v s) else s

(* Replaces [v] by [v'] in death table [death] *)
let replace_in_death_table death v v' =
 Hashtbl.iter (fun k dead -> Hashtbl.replace death k (replace_in_set dead v v')) death

let find_compatible_local node var dead =
 (*Format.eprintf "find_compatible_local %s %s %a@." node.node_id var pp_iset dead;*)
  let typ = (Corelang.node_var var node).var_type in
  let eq_var = node_eq var node in
  let aliasable_inputs =
    match NodeDep.get_callee eq_var.eq_rhs with
    | None           -> []
    | Some (_, args) -> List.fold_right (fun e r -> match e.expr_desc with Expr_ident id -> id::r | _ -> r) args [] in
  let filter v =
    let res =
       ISet.mem v.var_id dead
    && Typing.eq_ground typ v.var_type
    && not (Types.is_address_type v.var_type  && List.mem v.var_id aliasable_inputs) in
    begin
      (*Format.eprintf "filter %a = %s@." Printers.pp_var_name v (if res then "true" else "false");*)
      res
    end in
  try
    Some ((List.find filter node.node_locals).var_id)
  with Not_found -> None

let reuse_policy node sort death =
  let dead = ref ISet.empty in
  let policy = Hashtbl.create 23 in
  let sort = ref sort in
  while !sort <> []
  do
    let head = List.hd !sort in
    if Hashtbl.mem death head then
      begin
	dead := ISet.union (Hashtbl.find death head) !dead;
      end;
    (match find_compatible_local node head !dead with
    | None   -> ()
    | Some l -> replace_in_death_table death head l; Hashtbl.add policy head l);
    sort := List.tl !sort;
  done;
  policy
 
let pp_reuse_policy fmt policy =
  begin
    Format.fprintf fmt "{ /* reuse policy */@.";
    Hashtbl.iter (fun s t -> Format.fprintf fmt "%s -> %s@." s t) policy;
    Format.fprintf fmt "}@."
  end
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
