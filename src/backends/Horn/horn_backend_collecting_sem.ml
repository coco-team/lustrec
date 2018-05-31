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

(* The compilation presented here was first defined in Garoche, Gurfinkel,
   Kahsai, HCSV'14.

   This is a modified version that handle reset
*)

open Format
open Lustre_types
open Corelang
open Machine_code_types

open Horn_backend_common
open Horn_backend_printers

let collecting_semantics machines fmt node machine =
  fprintf fmt "; Collecting semantics for node %s@.@." node;
  (* We print the types of the main node "memory tree" TODO: add the output *)
  let main_output =
    rename_machine_list machine.mname.node_id machine.mstep.step_outputs
  in
  let main_output_dummy =
    rename_machine_list ("dummy" ^ machine.mname.node_id) machine.mstep.step_outputs
  in
  let main_memory_next =
    (rename_next_list (* machine.mname.node_id *) (full_memory_vars machines machine)) @
      main_output
  in
  let main_memory_current =
    (rename_current_list (* machine.mname.node_id *) (full_memory_vars machines machine)) @
      main_output_dummy
  in

  fprintf fmt "(declare-rel MAIN (%a))@."
    (Utils.fprintf_list ~sep:" " pp_type)
    (List.map (fun v -> v.var_type) main_memory_next);

  
  (* Init case *)
  let _ = 
    (* Special case when the main node is stateless *)
    if is_stateless machine then (
      let step_name = pp_machine_stateless_name in
      fprintf fmt "; Initial set: One Step(m,x)  -- Stateless node! @.";
      fprintf fmt "(declare-rel INIT_STATE ())@.";
      fprintf fmt "(rule INIT_STATE)@.";
      fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>";
      fprintf fmt "INIT_STATE@ ";
      fprintf fmt "(@[<v 0>%a %a@])"
	step_name node
	(Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (step_vars_m_x machines machine);    
      fprintf fmt "@]@ )@ ";
      fprintf fmt "(MAIN %a)@]@.))@.@."
	(Utils.fprintf_list ~sep:" " (pp_horn_var machine)) main_memory_next ;
    )
    else (
      let reset_name, step_name =
	pp_machine_reset_name, pp_machine_step_name
      in
      fprintf fmt "; Initial set: Reset(c,m) + One Step(m,x) @.";
      fprintf fmt "(declare-rel INIT_STATE ())@.";
      fprintf fmt "(rule INIT_STATE)@.";
      fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>";
      fprintf fmt "INIT_STATE@ ";
      fprintf fmt "(@[<v 0>%a %a@])@ "
	reset_name node
	(Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (reset_vars machines machine);
      fprintf fmt "(@[<v 0>%a %a@])"
	step_name node
	(Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (step_vars_m_x machines machine);
      
      fprintf fmt "@]@ )@ ";
      fprintf fmt "(MAIN %a)@]@.))@.@."
	(Utils.fprintf_list ~sep:" " (pp_horn_var machine)) main_memory_next ;
    )
  in

  let step_name = 
    if is_stateless machine then 
      pp_machine_stateless_name
    else
      pp_machine_step_name
  in
  
  fprintf fmt "; Inductive def@.";
  (Utils.fprintf_list ~sep:" " (fun fmt v -> fprintf fmt "%a@." pp_decl_var v)) fmt main_output_dummy;
  fprintf fmt
    "@[<v 2>(rule (=> @ (and @[<v 0>(MAIN %a)@ (@[<v 0>%a %a@])@]@ )@ (MAIN %a)@]@.))@.@."
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) main_memory_current
    step_name node
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (step_vars machines machine)
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) main_memory_next


let check_prop machines fmt node machine =
  let main_output =
    rename_machine_list machine.mname.node_id machine.mstep.step_outputs
  in
  let main_memory_next =
    (rename_next_list (full_memory_vars machines machine)) @ main_output
  in
  fprintf fmt "; Property def@.";
  fprintf fmt "(declare-rel ERR ())@.";
  fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>(not %a)@ (MAIN %a)@])@ ERR))@."
    (pp_conj (pp_horn_var machine)) main_output
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) main_memory_next
    ;
   if !Options.horn_query then fprintf fmt "(query ERR)@."


let cex_computation machines fmt node machine =
  fprintf fmt "; CounterExample computation for node %s@.@." node;
    (* We print the types of the cex node "memory tree" TODO: add the output *)
  let cex_input =
    rename_machine_list machine.mname.node_id machine.mstep.step_inputs
  in
  let cex_input_dummy =
    rename_machine_list ("dummy" ^ machine.mname.node_id) machine.mstep.step_inputs
  in
  let cex_output =
    rename_machine_list machine.mname.node_id machine.mstep.step_outputs
  in
  let cex_output_dummy =
    rename_machine_list ("dummy" ^ machine.mname.node_id) machine.mstep.step_outputs
  in
  let cex_memory_next =
    cex_input @ (rename_next_list (full_memory_vars machines machine)) @ cex_output
  in
  let cex_memory_current =
    cex_input_dummy @ (rename_current_list (full_memory_vars machines machine)) @ cex_output_dummy
  in

    (* Special case when the cex node is stateless *)
  let reset_name, step_name =
    if is_stateless machine then
      pp_machine_stateless_name, pp_machine_stateless_name
    else
      pp_machine_reset_name, pp_machine_step_name
  in

  fprintf fmt "(declare-rel CEX (Int %a))@.@."
    (Utils.fprintf_list ~sep:" " pp_type)
    (List.map (fun v -> v.var_type) cex_memory_next);

  fprintf fmt "; Initial set: Reset(c,m) + One Step(m,x) @.";
  fprintf fmt "(declare-rel INIT_STATE_CEX ())@.";
  fprintf fmt "(rule INIT_STATE_CEX)@.";
  fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>";
  fprintf fmt "INIT_STATE_CEX@ ";
  fprintf fmt "(@[<v 0>%a %a@])@ "
    reset_name node
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (reset_vars machines machine);
  fprintf fmt "(@[<v 0>%a %a@])"
    step_name node
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (step_vars_m_x machines machine);
  
  fprintf fmt "@]@ )@ ";
  fprintf fmt "(CEX 0 %a)@]@.))@.@."
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) cex_memory_next ;

  fprintf fmt "; Inductive def@.";
    (* Declare dummy inputs. Outputs should have been declared previously with collecting sem *)
  (Utils.fprintf_list ~sep:" " (fun fmt v -> fprintf fmt "%a@." pp_decl_var v)) fmt cex_input_dummy;
  fprintf fmt "(declare-var cexcpt Int)@.";
  fprintf fmt
    "@[<v 2>(rule (=> @ (and @[<v 0>(CEX cexcpt %a)@ (@[<v 0>%a %a@])@]@ )@ (CEX (+ 1 cexcpt) %a)@]@.))@.@."
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) cex_memory_current
    step_name node
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) (step_vars machines machine)
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) cex_memory_next

let get_cex machines fmt node machine =
    let cex_input =
     rename_machine_list machine.mname.node_id machine.mstep.step_inputs
    in
    let cex_output =
     rename_machine_list machine.mname.node_id machine.mstep.step_outputs
    in
  let cex_memory_next =
    cex_input @ (rename_next_list (full_memory_vars machines machine)) @ cex_output
  in
  fprintf fmt "; Property def@.";
  fprintf fmt "(declare-rel CEXTRACE ())@.";
  fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>(not %a)@ (CEX cexcpt %a)@])@ CEXTRACE))@."
    (pp_conj (pp_horn_var machine)) cex_output
    (Utils.fprintf_list ~sep:" " (pp_horn_var machine)) cex_memory_next
    ;
  fprintf fmt "(query CEXTRACE)@."

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
