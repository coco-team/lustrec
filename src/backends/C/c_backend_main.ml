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

open Lustre_types
open Machine_code_types
open Corelang
open Machine_code_common
open Format
open C_backend_common
open Utils

module type MODIFIERS_MAINSRC =
sig
end

module EmptyMod =
struct
end

module Main = functor (Mod: MODIFIERS_MAINSRC) -> 
struct

(********************************************************************************************)
(*                         Main related functions                                           *)
(********************************************************************************************)


let print_put_outputs fmt m = 
  let po fmt (id, o', o) =
    let suff = string_of_int id in
    print_put_var fmt suff o'.var_id o.var_type o.var_id
  in
  List.iteri2 (fun idx v' v -> fprintf fmt "@ %a;" po ((idx+1), v', v)) m.mname.node_outputs m.mstep.step_outputs

let print_main_inout_declaration basename fmt m =
  let mname = m.mname.node_id in
  (* TODO: find a proper way to shorthen long names. This causes segfault in the binary when trying to fprintf in them *)
  let mname = if String.length mname > 50 then string_of_int (Hashtbl.hash mname) else mname in
  fprintf fmt "/* Declaration of inputs/outputs variables */@ ";
  List.iteri 
    (fun idx v ->
      fprintf fmt "%a;@ " (pp_c_type v.var_id) v.var_type;
      fprintf fmt "FILE *f_in%i;@ " (idx+1); (* we start from 1: in1, in2, ... *)
      fprintf fmt "f_in%i = fopen(\"%s_%s_simu.in%i\", \"w\");@ " (idx+1) basename mname (idx+1);
    ) m.mstep.step_inputs;
  List.iteri 
    (fun idx v ->
      fprintf fmt "%a;@ " (pp_c_type v.var_id) v.var_type;
      fprintf fmt "FILE *f_out%i;@ " (idx+1); (* we start from 1: in1, in2, ... *)
      fprintf fmt "f_out%i = fopen(\"%s_%s_simu.out%i\", \"w\");@ " (idx+1) basename mname (idx+1);
    ) m.mstep.step_outputs


  
let print_main_memory_allocation mname main_mem fmt m =
  if not (fst (get_stateless_status m)) then
  begin  
    fprintf fmt "@ /* Main memory allocation */@ ";
    if (!Options.static_mem && !Options.main_node <> "")
    then (fprintf fmt "%a(static,main_mem);@ " pp_machine_static_alloc_name mname)
    else (fprintf fmt "%a *main_mem = %a();@ " pp_machine_memtype_name mname pp_machine_alloc_name mname);
    fprintf fmt "@ /* Initialize the main memory */@ ";
    fprintf fmt "%a(%s);@ " pp_machine_reset_name mname main_mem;
  end

let print_global_initialize fmt basename =
  let mNAME = file_to_module_name basename in
  fprintf fmt "@ /* Initialize global constants */@ %a();@ "
    pp_global_init_name mNAME

let print_global_clear fmt basename =
  let mNAME = file_to_module_name basename in
  fprintf fmt "@ /* Clear global constants */@ %a();@ "
    pp_global_clear_name mNAME

let print_main_initialize mname main_mem fmt m =
  if not (fst (get_stateless_status m))
  then
    fprintf fmt "@ /* Initialize inputs, outputs and memories */@ %a%t%a%t%a(%s);@ "
      (Utils.fprintf_list ~sep:"@ " (pp_initialize m main_mem (pp_c_var_read m))) m.mstep.step_inputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_initialize m main_mem (pp_c_var_read m))) m.mstep.step_outputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      pp_machine_init_name mname
      main_mem
  else
    fprintf fmt "@ /* Initialize inputs and outputs */@ %a%t%a@ "
      (Utils.fprintf_list ~sep:"@ " (pp_initialize m main_mem (pp_c_var_read m))) m.mstep.step_inputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_initialize m main_mem (pp_c_var_read m))) m.mstep.step_outputs

let print_main_clear mname main_mem fmt m =
  if not (fst (get_stateless_status m))
  then
    fprintf fmt "@ /* Clear inputs, outputs and memories */@ %a%t%a%t%a(%s);@ "
      (Utils.fprintf_list ~sep:"@ " (pp_clear m main_mem (pp_c_var_read m))) m.mstep.step_inputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_clear m main_mem (pp_c_var_read m))) m.mstep.step_outputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      pp_machine_clear_name mname
      main_mem
  else
    fprintf fmt "@ /* Clear inputs and outputs */@ %a%t%a@ "
      (Utils.fprintf_list ~sep:"@ " (pp_clear m main_mem (pp_c_var_read m))) m.mstep.step_inputs
      (Utils.pp_newline_if_non_empty m.mstep.step_inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_clear m main_mem (pp_c_var_read m))) m.mstep.step_outputs

let print_main_loop mname main_mem fmt m =
  let input_values =
    List.map (fun v -> mk_val (Var v) v.var_type)
      m.mstep.step_inputs in
  begin
    fprintf fmt "@ ISATTY = isatty(0);@ ";
    fprintf fmt "@ /* Infinite loop */@ ";
    fprintf fmt "@[<v 2>while(1){@ ";
    fprintf fmt  "fflush(stdout);@ ";
    List.iteri (fun idx _ -> fprintf fmt "fflush(f_in%i);@ " (idx+1)) m.mstep.step_inputs;
    List.iteri (fun idx _ -> fprintf fmt "fflush(f_out%i);@ " (idx+1)) m.mstep.step_outputs;
    fprintf fmt "%a@ %t%a"
      print_get_inputs m
      (fun fmt -> pp_main_call mname main_mem fmt m input_values m.mstep.step_outputs)
      print_put_outputs m
  end

let print_main_code fmt basename m =
  let mname = m.mname.node_id in
  let main_mem =
    if (!Options.static_mem && !Options.main_node <> "")
    then "&main_mem"
    else "main_mem" in
  fprintf fmt "@[<v 2>int main (int argc, char *argv[]) {@ ";
  print_main_inout_declaration basename fmt m;
  Plugins.c_backend_main_loop_body_prefix basename mname fmt ();
  print_main_memory_allocation mname main_mem fmt m;
  if !Options.mpfr then
    begin
      print_global_initialize fmt basename;
      print_main_initialize mname main_mem fmt m;
    end;
  print_main_loop mname main_mem fmt m;

  Plugins.c_backend_main_loop_body_suffix fmt ();
  fprintf fmt "@]@ }@ @ ";
  if !Options.mpfr then
    begin
      print_main_clear mname main_mem fmt m;
      print_global_clear fmt basename;
    end;
  fprintf fmt "@ return 1;";
  fprintf fmt "@]@ }@."       

let print_main_header fmt =
  fprintf fmt (if !Options.cpp then "#include <stdio.h>@.#include <unistd.h>@.#include \"%s/io_frontend.hpp\"@." else "#include <stdio.h>@.#include <unistd.h>@.#include \"%s/io_frontend.h\"@.")
    (Options_management.core_dependency "io_frontend")

let print_main_c main_fmt main_machine basename prog machines _ (*dependencies*) =
  print_main_header main_fmt;
  fprintf main_fmt "#include <stdlib.h>@.#include <assert.h>@.";
  print_import_alloc_prototype main_fmt (Dep (true, basename, [], true (* assuming it is stateful*) ));
  pp_print_newline main_fmt ();

  (* Print the svn version number and the supported C standard (C90 or C99) *)
  print_version main_fmt;
  print_main_code main_fmt basename main_machine
end  

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
