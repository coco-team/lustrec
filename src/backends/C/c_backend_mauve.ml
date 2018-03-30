open Lustre_types
open Machine_code_types
open Corelang
open Machine_code
open Format
open C_backend_common
open Utils
open Printers

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

let shell_name node = node ^ "Shell"
let core_name  node = node ^ "Core"
let fsm_name   node = node ^ "FSM"

(* -------------------------------------------------- *)
(*                       Hearder                      *)
(* -------------------------------------------------- *)

let print_mauve_header fmt mauve_machine basename prog machines _ (*dependencies*) =
  fprintf fmt "#include \"mauve/runtime.hpp\"@.";
  print_import_alloc_prototype fmt (Dep (true, basename, [], true (* assuming it is stateful*) ));
  pp_print_newline fmt ();
  pp_print_newline fmt ()

(* -------------------------------------------------- *)
(*                       Shell                        *)
(* -------------------------------------------------- *)

let mauve_default_value v =
  (* let v_name = v.var_id in *)

  if Types.is_bool_type v.var_type then "false"
  else if Types.is_int_type v.var_type then "0"
  else if Types.is_real_type v.var_type then "0.0"
  else assert false

let print_mauve_default fmt mauve_machine v = 
  let v_name: string = v.var_id in
  let found = ref false in
  let annotations: expr_annot list = mauve_machine.mname.node_annot in
  List.iter
    (fun (al: expr_annot) ->
        List.iter
          (fun ((sl, e): string list * eexpr) -> if not !found then match sl with 
          | ["mauve"; "default"; name] ->
            if v_name = name then begin (pp_expr fmt e.eexpr_qfexpr); found := true; end
          | _ -> ();
          ) al.annots;
    ) annotations;
  if not !found then fprintf fmt "%s" (mauve_default_value v)


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
      let v_type = pp_c_basic_type_desc v.var_type in
      fprintf fmt "\tReadPort<%s> & port_%s = mk_readPort<%s>(\"%s\", " v_type v_name v_type v_name;
      print_mauve_default fmt mauve_machine v;
      fprintf fmt ");@.";
    ) mauve_machine.mstep.step_inputs;
  (* out ports *)
  fprintf fmt "\t// OutputPorts@.";
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc v.var_type in
      fprintf fmt "\tWritePort<%s> & port_%s = mk_writePort<%s>(\"%s\");@." v_type v_name v_type v_name;
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

(* -------------------------------------------------- *)
(*                       Core                      *)
(* -------------------------------------------------- *)

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
      let v_type = pp_c_basic_type_desc v.var_type in
      fprintf fmt "\t\t%s %s = port_%s.read();@." v_type v_name v_name;
    ) mauve_machine.mstep.step_inputs;
  List.iter
    (fun v ->
      let v_name = v.var_id in
      let v_type = pp_c_basic_type_desc v.var_type in
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
  fprintf fmt "\t\t%s_dealloc(node);@." node_name;
  fprintf fmt "\t}@.";
  fprintf fmt "};@.";
  pp_print_newline fmt ()

(* -------------------------------------------------- *)
(*                       FSM                          *)
(* -------------------------------------------------- *)

let print_period_conversion fmt expr = (
  match expr.expr_desc with
    | Expr_tuple [p; u] -> (
       match u.expr_desc with 
       | Expr_ident "s"   -> fprintf fmt "sec_to_ns("; (pp_expr fmt p); fprintf fmt ")"
       | Expr_ident "ssec"-> fprintf fmt "sec_to_ns("; (pp_expr fmt p); fprintf fmt ")"
       | Expr_ident "ms"  -> fprintf fmt "ms_to_ns(" ; (pp_expr fmt p); fprintf fmt ")"
       | Expr_ident "ns"  -> pp_expr fmt p
       | _    -> assert false
      )
    | _ -> assert false
  )

let print_mauve_period fmt mauve_machine = 
  let found = ref false in
  let annotations: expr_annot list = mauve_machine.mname.node_annot in
  List.iter
    (fun (al: expr_annot) ->
        List.iter
          (fun ((sl, e): string list * eexpr) -> if not !found then match sl with 
           | ["mauve"; "period" ] -> (print_period_conversion fmt e.eexpr_qfexpr); found := true;
           | _ -> ();
          ) al.annots;
    ) annotations;
  if not !found then fprintf fmt "0"


let print_mauve_fsm fmt mauve_machine basename prog machines _ (*dependencies*) =
  let node_name = mauve_machine.mname.node_id in

  fprintf fmt "/*@.";
  fprintf fmt " *          FSM@.";
  fprintf fmt " */@.";

  fprintf fmt "struct %s: public FiniteStateMachine<%s, %s> {@." (fsm_name node_name) (shell_name node_name) (core_name node_name);

  (* Attribute *)
  fprintf fmt "\tExecState<%s>    & update  = mk_execution      (\"Update\" , &%s::update);@." (core_name node_name) (core_name node_name);
  fprintf fmt "\tSynchroState<%s> & synchro = mk_synchronization(\"Synchro\", " (core_name node_name);
  print_mauve_period fmt mauve_machine;
  fprintf fmt ");@.";
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
