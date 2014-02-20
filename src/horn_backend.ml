open Format
open LustreSpec
open Corelang
open Machine_code


let pp_type fmt t =
  match (Types.repr t).Types.tdesc with
  | Types.Tbool           -> Format.fprintf fmt "Bool"
  | Types.Tint            -> Format.fprintf fmt "Int"
  | Types.Tclock _
  | Types.Tarray _
  | Types.Tstatic _
  | Types.Tconst _
  | Types.Tarrow _
  | _                     -> Format.eprintf "internal error: pp_type %a@." Types.print_ty t; assert false
    

let pp_decl_var fmt id = 
  Format.fprintf fmt "(declare_var %s %a)"
    id.var_id
    pp_type id.var_type

let pp_var fmt id = Format.pp_print_string fmt id.var_id

let pp_instr machine_name fmt i =
  Format.fprintf fmt "(xxx)"

let rename f = List.map (fun v -> {v with var_id = f v.var_id } )
let rename_current machine_name = rename (fun n -> machine_name ^ "." ^ n ^ "_c")
let rename_next machine_name = rename (fun n -> machine_name ^ "." ^ n ^ "_x")
let rename_machine machine_name = rename (fun n -> machine_name ^ "." ^ n)

let machine_vars m = 
  (rename_current m.mname.node_id m.mstatic)@
    (rename_next m.mname.node_id m.mstatic)@
    (rename_machine m.mname.node_id m.mstep.step_inputs)@
    (rename_machine m.mname.node_id m.mstep.step_outputs)
    
(* Print the machine m: 
   two functions: m_init and m_step
   - m_init is a predicate over m memories
   - m_step is a predicate over old_memories, inputs, new_memories, outputs
   We first declare all variables then the two /rules/.
*)
let print_machine fmt m = 
 if m.mname.node_id = arrow_id then () else ( (* We don't print arrow function *)
   Format.fprintf fmt "; %s@." m.mname.node_id;
   (* Printing variables *)
   Utils.fprintf_list ~sep:"@." pp_decl_var fmt 
     ((machine_vars m)@(rename_machine m.mname.node_id m.mstep.step_locals));

   (* Declaring predicate *)
   Format.fprintf fmt "(declare-rel %s_init (%a))@."
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_type) (List.map (fun v -> v.var_type) m.mstatic);
   
   Format.fprintf fmt "(declare-rel %s_step (%a))@."
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_type) (List.map (fun v -> v.var_type) (machine_vars m));

   Format.fprintf fmt "(rule (=> (and %a) (%s_init %a)))"
     (Utils.fprintf_list ~sep:"@." (pp_instr m.mname.node_id)) m.minit
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_var) (rename_machine m.mname.node_id m.mstatic);

   Format.fprintf fmt "(rule (=> (and %a) (%s_step %a)))"
     (Utils.fprintf_list ~sep:"@." (pp_instr m.mname.node_id)) m.mstep.step_instrs
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_var) (machine_vars m);
   
()
  )

let main_print fmt = ()

let translate fmt basename prog machines =
  List.iter (print_machine fmt) machines;
  main_print fmt 


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
