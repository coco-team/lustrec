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
*)
let death_table node g sort =
  let non_locals = node_non_locals node in
  let death = Hashtbl.create 23 in
  let sort  = ref sort in
  begin
    while (!sort <> [])
    do
      let head = List.hd !sort in
      let dead =
	begin
	  IdentDepGraph.iter_succ (IdentDepGraph.remove_edge g head) g head;
	  remove_local_roots non_locals g
	end in
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

(* Replaces [v] by [v'] in set [s] *)
let replace_in_set s v v' =
  if ISet.mem v s then ISet.add v' (ISet.remove v s) else s

(* Replaces [v] by [v'] in death table [death] *)
let replace_in_death_table death v v' =
 Hashtbl.iter (fun k dead -> Hashtbl.add death k (replace_in_set dead v v')) death

let find_compatible_local node var dead =
  let typ = (Corelang.node_var var node).var_type in
  try
    Some ((List.find (fun v -> ISet.mem v.var_id dead && Typing.eq_ground typ v.var_type) node.node_locals).var_id)
  with Not_found -> None

let reuse_policy node sort death =
  let dead = ref ISet.empty in
  let policy = Hashtbl.create 23 in
  let sort = ref sort in
  while !sort <> []
  do
    let head = List.hd !sort in
    dead := ISet.union (Hashtbl.find death head) !dead;
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
