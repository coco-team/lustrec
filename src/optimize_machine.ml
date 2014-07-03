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


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
