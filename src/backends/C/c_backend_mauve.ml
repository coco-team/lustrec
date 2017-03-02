open LustreSpec
open Corelang
open Machine_code
open Format
open C_backend_common
open Utils

(* module type MODIFIERS_MAINSRC =
sig
end

module EmptyMod =
struct
end

module Mauve = functor (Mod: MODIFIERS_MAINSRC) -> 
struct
end
 *)
(********************************************************************************************)
(*                         Main related functions                                           *)
(********************************************************************************************)

let mauve_default_value v =
  let v_name = v.var_id in
  let v_type = (Types.repr v.var_type).Types.tdesc in
  match v_type with
  | Types.Tbool -> "false"
  | Types.Tint  -> "0"
  | Types.Treal -> "0.0"
  | _ -> assert false

let shell_name node = node ^ "Shell"
let core_name  node = node ^ "Core"
let fsm_name   node = node ^ "FSM"

let print_mauve_header fmt mauve_machine basename prog machines _ (*dependencies*) =
  fprintf fmt "#include \"mauve/runtime.hpp\"@.";
  print_import_alloc_prototype fmt (Dep (true, basename, [], true (* assuming it is stateful*) ));
  pp_print_newline fmt ();
  pp_print_newline fmt ()


let print_mauve_shell fmt mauve_machine basename prog machines _ (*dependencies*) =
  let node_name = mauve_machine.mname.node_id in
  
  fprintf fmt "/*@.";
  fprintf fmt " *          SHELL@.";
  fprintf fmt " */@.";

  fprintf fmt "struct %s: public Shell {@." (shell_name node_name);

  (* in ports *)
  fprintf fmt "\t// InputPorts@.";
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc (Types.repr v.var_type).Types.tdesc in
      let default = mauve_default_value v in
      fprintf fmt "\tReadPort<%s> port_%s = mk_readPort<%s>(\"%s\", %s);@." v_type v_name v_type v_name default;
    ) mauve_machine.mstep.step_inputs;
  (* out ports *)
  fprintf fmt "\t// OutputPorts@.";
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc (Types.repr v.var_type).Types.tdesc in
      fprintf fmt "\tWritePort<%s> port_%s = mk_writePort<%s>(\"%s\");@." v_type v_name v_type v_name;
    ) mauve_machine.mstep.step_outputs;

  fprintf fmt "};@.";

  pp_print_newline fmt ()

let print_mauve_step fmt node_name mauve_machine =
  fprintf fmt "\t\t%s_step(" node_name;
  List.iter
    (fun v ->
      let v_name = v.var_id in
      fprintf fmt "%s, " v_name;
    ) mauve_machine.mstep.step_inputs;
  List.iter
    (fun v ->
      let v_name = v.var_id in
      fprintf fmt "&%s, " v_name;
    ) mauve_machine.mstep.step_outputs;
  fprintf fmt "node";
  fprintf fmt ");@."

let print_mauve_core fmt mauve_machine basename prog machines _ (*dependencies*) =
  let node_name = mauve_machine.mname.node_id in

  fprintf fmt "/*@.";
  fprintf fmt " *          CORE@.";
  fprintf fmt " */@.";

  fprintf fmt "struct %s: public Core<%s> {@." (core_name node_name) (shell_name node_name);

  (* Attribute *)
  fprintf fmt "\tstruct %s_mem * node;@." node_name;
  pp_print_newline fmt ();
  (* Update *)
  fprintf fmt "\tvoid update() {@.";
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc (Types.repr v.var_type).Types.tdesc in
      fprintf fmt "\t\t%s %s = port_%s.read();@." v_type v_name v_name;
    ) mauve_machine.mstep.step_inputs;
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc (Types.repr v.var_type).Types.tdesc in
      fprintf fmt "\t\t%s %s;@." v_type v_name;
    ) mauve_machine.mstep.step_outputs;
  print_mauve_step fmt node_name mauve_machine;
  List.iter
    (fun v ->
      let v_name = v.var_id in
      fprintf fmt "\t\tport_%s.write(%s);@." v_name v_name;
    ) mauve_machine.mstep.step_outputs;
  fprintf fmt "\t}@.";
  pp_print_newline fmt ();
  (* Configure *)
  fprintf fmt "\tbool configure_hook() override {@.";
  fprintf fmt "\t\tnode = %s_alloc();@." node_name;
  fprintf fmt "\t\t%s_reset(node);@." node_name;
  fprintf fmt "\t\treturn true;@.";
  fprintf fmt "\t}@.";
  pp_print_newline fmt ();
  (* Cleanup *)
  fprintf fmt "\tvoid cleanup_hook() override {@.";
  fprintf fmt "\t\t%s_reset(node);@." node_name;
  fprintf fmt "\t\tfree(node);@.";
  fprintf fmt "\t}@.";
  fprintf fmt "};@.";
  pp_print_newline fmt ()


let print_mauve_fsm fmt mauve_machine basename prog machines _ (*dependencies*) =
  let node_name = mauve_machine.mname.node_id in

  fprintf fmt "/*@.";
  fprintf fmt " *          FSM@.";
  fprintf fmt " */@.";

  fprintf fmt "struct %s: public FiniteStateMachine<%s, %s> {@." (fsm_name node_name) (shell_name node_name) (core_name node_name);

  (* Attribute *)
  fprintf fmt "\tExecState<%s>    & update  = mk_execution      (\"Update\" , &%s::update);@." (core_name node_name) (core_name node_name);
  fprintf fmt "\tSynchroState<%s> & synchro = mk_synchronization(\"Synchro\", ms_to_ns(100));@." (core_name node_name);
  pp_print_newline fmt ();
  (* Configure *)
  fprintf fmt "\tbool configure_hook() override {@.";
  fprintf fmt "\t\tset_initial(update);@.";
  fprintf fmt "\t\tset_next(update, synchro);@.";
  fprintf fmt "\t\tset_next(synchro, update);@.";
  fprintf fmt "\t\treturn true;@.";
  fprintf fmt "\t}@.";
  pp_print_newline fmt ();
  (* Cleanup *)
  fprintf fmt "\tvoid cleanup_hook() override {@.";
  fprintf fmt "\t}@.";
  fprintf fmt "};@.";
  pp_print_newline fmt ()

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
