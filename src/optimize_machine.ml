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

open LustreSpec 
open Corelang
open Machine_code 

let rec eliminate elim instr =
  let e_expr = eliminate_expr elim in
  match instr with  
  | MLocalAssign (i,v) -> MLocalAssign (i, e_expr v)
  | MStateAssign (i,v) -> MStateAssign (i, e_expr v)
  | MReset i           -> instr
  | MStep (il, i, vl)  -> MStep(il, i, List.map e_expr vl)
  | MBranch (g,hl)     -> 
    MBranch
      (e_expr g, 
       (List.map 
	  (fun (l, il) -> l, List.map (eliminate elim) il) 
	  hl
       )
      )
    
and eliminate_expr elim expr =
  match expr with
  | LocalVar v -> if List.mem_assoc v elim then List.assoc v elim else expr
  | Fun (id, vl) -> Fun (id, List.map (eliminate_expr elim) vl)
  | Array(vl) -> Array(List.map (eliminate_expr elim) vl)
  | Access(v1, v2) -> Access(eliminate_expr elim v1, eliminate_expr elim v2)
  | Power(v1, v2) -> Access(eliminate_expr elim v1, eliminate_expr elim v2)
  | Cst _ | StateVar _ -> expr

(* see if elim has to take in account the provided instr:
   if so, upodate elim and return the remove flag,
   otherwise, the expression should be kept and elim is left untouched *)
let update_elim outputs elim instr =
(*  Format.eprintf "SHOULD WE STORE THE EXPRESSION IN INSTR %a TO ELIMINATE IT@." pp_instr instr;*)
	  
  let apply elim v new_e = 
    (v, new_e)::List.map (fun (v, e) -> v, eliminate_expr [v, new_e] e) elim 
  in
  match instr with
  (* Simple cases*)
  | MLocalAssign (v, (Cst _ as e)) 
  | MLocalAssign (v, (LocalVar _ as e)) 
  | MLocalAssign (v, (StateVar _ as e)) -> 
    if not (List.mem v outputs) then  true, apply elim v e else false, elim
  (* When optimization >= 3, we also inline any basic operator call. 
     All those are returning a single ouput *)
  | MStep([v], id, vl) when
      Basic_library.is_internal_fun id
      && !Options.optimization >= 3
      -> 	  assert false 
(*    true, apply elim v (Fun(id, vl))*)

    
  | MLocalAssign (v, ((Fun (id, il)) as e)) when 
      not (List.mem v outputs) 
      && Basic_library.is_internal_fun id (* this will avoid inlining ite *)
      && !Options.optimization >= 3 
	-> (
(*	  Format.eprintf "WE STORE THE EXPRESSION DEFINING %s TO ELIMINATE IT@." v.var_id; *)
	  true, apply elim v e
	)
  | _ -> 
    (* default case, we keep the instruction and do not modify elim *)
    false, elim
  

(** We iterate in the order, recording simple local assigns in an accumulator
    1. each expression is rewritten according to the accumulator
    2. local assigns then rewrite occurrences of the lhs in the computed accumulator
*)
let optimize_minstrs outputs instrs = 
  let rev_instrs, eliminate = 
    List.fold_left (fun (rinstrs, elim) instr ->
      (* each subexpression in instr that could be rewritten by the elim set is
	 rewritten *)
      let instr = eliminate elim instr in
      (* if instr is a simple local assign, then (a) elim is simplified with it (b) it
	 is stored as the elim set *)
      let remove, elim = update_elim outputs elim instr in
      (if remove then rinstrs else instr::rinstrs), elim
    ) ([],[]) instrs 
  in
  let eliminated_vars = List.map fst eliminate in
  eliminated_vars, List.rev rev_instrs

(** Perform optimization on machine code:
    - iterate through step instructions and remove simple local assigns
    
*)
let optimize_machine machine =
  let eliminated_vars, new_instrs = optimize_minstrs machine.mstep.step_outputs machine.mstep.step_instrs in
  let new_locals = 
    List.filter (fun v -> not (List.mem v eliminated_vars)) machine.mstep.step_locals 
  in
  {
    machine with
      mstep = { 
	machine.mstep with 
	  step_locals = new_locals;
	  step_instrs = new_instrs
      }
  }
    


let optimize_machines machines =
  List.map optimize_machine machines

(* variable substitution for optimizing purposes *)

(* checks whether an [instr] is skip and can be removed from program *)
let rec instr_is_skip instr =
  match instr with
  | MLocalAssign (i, LocalVar v) when i = v -> true
  | MStateAssign (i, StateVar v) when i = v -> true
  | MBranch (g, hl) -> List.for_all (fun (_, il) -> instrs_are_skip il) hl
  | _               -> false
and instrs_are_skip instrs =
  List.for_all instr_is_skip instrs

let instr_cons instr cont =
 if instr_is_skip instr then cont else instr::cont

let rec instr_remove_skip instr cont =
  match instr with
  | MLocalAssign (i, LocalVar v) when i = v -> cont
  | MStateAssign (i, StateVar v) when i = v -> cont
  | MBranch (g, hl) -> MBranch (g, List.map (fun (h, il) -> (h, instrs_remove_skip il [])) hl) :: cont
  | _               -> instr::cont

and instrs_remove_skip instrs cont =
  List.fold_right instr_remove_skip instrs cont

let rec value_replace_var fvar value =
  match value with
  | Cst c -> value
  | LocalVar v -> LocalVar (fvar v)
  | StateVar v -> value
  | Fun (id, args) -> Fun (id, List.map (value_replace_var fvar) args) 
  | Array vl -> Array (List.map (value_replace_var fvar) vl)
  | Access (t, i) -> Access(value_replace_var fvar t, i)
  | Power (v, n) -> Power(value_replace_var fvar v, n)

let rec instr_replace_var fvar instr cont =
  match instr with
  | MLocalAssign (i, v) -> instr_cons (MLocalAssign (fvar i, value_replace_var fvar v)) cont
  | MStateAssign (i, v) -> instr_cons (MStateAssign (i, value_replace_var fvar v)) cont
  | MReset i            -> instr_cons instr cont
  | MStep (il, i, vl)   -> instr_cons (MStep (List.map fvar il, i, List.map (value_replace_var fvar) vl)) cont
  | MBranch (g, hl)     -> instr_cons (MBranch (value_replace_var fvar g, List.map (fun (h, il) -> (h, instrs_replace_var fvar il [])) hl)) cont

and instrs_replace_var fvar instrs cont =
  List.fold_right (instr_replace_var fvar) instrs cont

let step_replace_var fvar step =
  (* Some outputs may have been replaced by locals.
     We then need to rename those outputs
     without changing their clocks, etc *)
  let outputs' =
    List.map (fun o -> { o with var_id = (fvar o).var_id }) step.step_outputs in
  let locals'  =
    List.fold_left (fun res l ->
      let l' = fvar l in
      if List.exists (fun o -> o.var_id = l'.var_id) outputs'
      then res
      else Utils.add_cons l' res)
      [] step.step_locals in
  { step with
    step_checks = List.map (fun (l, v) -> (l, value_replace_var fvar v)) step.step_checks;
    step_outputs = outputs';
    step_locals = locals';
    step_instrs = instrs_replace_var fvar step.step_instrs [];
}

let rec machine_replace_variables fvar m =
  { m with
    mstep = step_replace_var fvar m.mstep
  }

let machine_reuse_variables m reuse =
  let fvar v =
    try
      Hashtbl.find reuse v.var_id
    with Not_found -> v in
  machine_replace_variables fvar m

let machines_reuse_variables prog node_schs =
  List.map 
    (fun m -> 
      machine_reuse_variables m (Utils.IMap.find m.mname.node_id node_schs).Scheduling.reuse_table
    ) prog


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
