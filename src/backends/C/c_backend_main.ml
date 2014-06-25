open LustreSpec
open Corelang
open Machine_code
open Format
open C_backend_common

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

let print_get_input fmt v =
  match v.var_type.Types.tdesc with
    | Types.Tint -> fprintf fmt "_get_int(\"%s\")" v.var_id
    | Types.Tbool -> fprintf fmt "_get_bool(\"%s\")" v.var_id
    | Types.Treal -> fprintf fmt "_get_double(\"%s\")" v.var_id
    | _ -> assert false

let print_put_outputs fmt ol = 
  let po fmt o =
    match o.var_type.Types.tdesc with
    | Types.Tint -> fprintf fmt "_put_int(\"%s\", %s)" o.var_id o.var_id
    | Types.Tbool -> fprintf fmt "_put_bool(\"%s\", %s)" o.var_id o.var_id
    | Types.Treal -> fprintf fmt "_put_double(\"%s\", %s)" o.var_id o.var_id
    | _ -> assert false
  in
  List.iter (fprintf fmt "@ %a;" po) ol

let print_main_fun machines m fmt =
  let mname = m.mname.node_id in
  let main_mem =
    if (!Options.static_mem && !Options.main_node <> "")
    then "&main_mem"
    else "main_mem" in
  fprintf fmt "@[<v 2>int main (int argc, char *argv[]) {@ ";
  fprintf fmt "/* Declaration of inputs/outputs variables */@ ";
  List.iter 
    (fun v -> fprintf fmt "%a = %a;@ " (pp_c_type v.var_id) v.var_type pp_c_initialize v.var_type
    ) m.mstep.step_inputs;
  List.iter 
    (fun v -> fprintf fmt "%a = %a;@ " (pp_c_type v.var_id) v.var_type pp_c_initialize v.var_type
    ) m.mstep.step_outputs;
  fprintf fmt "@ /* Main memory allocation */@ ";
  if (!Options.static_mem && !Options.main_node <> "")
  then (fprintf fmt "%a(static,main_mem);@ " pp_machine_static_alloc_name mname)
  else (fprintf fmt "%a *main_mem = %a();@ " pp_machine_memtype_name mname pp_machine_alloc_name mname);
  fprintf fmt "@ /* Initialize the main memory */@ ";
  fprintf fmt "%a(%s);@ " pp_machine_reset_name mname main_mem;
  fprintf fmt "@ ISATTY = isatty(0);@ ";
  fprintf fmt "@ /* Infinite loop */@ ";
  fprintf fmt "@[<v 2>while(1){@ ";
  fprintf fmt  "fflush(stdout);@ ";
  List.iter 
    (fun v -> fprintf fmt "%s = %a;@ "
      v.var_id
      print_get_input v
    ) m.mstep.step_inputs;
  (match m.mstep.step_outputs with
    (* | [] -> ( *)
    (*   fprintf fmt "%a(%a%t%s);@ "  *)
    (* 	pp_machine_step_name mname *)
    (* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs *)
    (* 	(pp_final_char_if_non_empty ", " m.mstep.step_inputs) *)
    (* 	main_mem *)
    (* ) *)
    (* | [o] -> ( *)
    (*   fprintf fmt "%s = %a(%a%t%a, %s);%a" *)
    (* 	o.var_id *)
    (* 	pp_machine_step_name mname *)
    (* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs *)
    (* 	(pp_final_char_if_non_empty ", " m.mstep.step_inputs) *)
    (* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> fprintf fmt "&%s" v.var_id)) m.mstep.step_outputs *)
    (* 	main_mem *)
    (* 	print_put_outputs [o]) *)
    | _ -> (
      fprintf fmt "%a(%a%t%a, %s);%a"
	pp_machine_step_name mname
	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs
	(Utils.pp_final_char_if_non_empty ", " m.mstep.step_inputs)
	(Utils.fprintf_list ~sep:", " (fun fmt v -> fprintf fmt "&%s" v.var_id)) m.mstep.step_outputs
	main_mem
	print_put_outputs m.mstep.step_outputs)
  );
  fprintf fmt "@]@ }@ ";
  fprintf fmt "return 1;";
  fprintf fmt "@]@ }@."       

let print_main_header fmt =
  fprintf fmt "#include <stdio.h>@.#include <unistd.h>@.#include \"%s/include/lustrec/io_frontend.h\"@." Version.prefix


let print_main_c main_fmt main_machine basename prog machines dependencies =
  print_main_header main_fmt;
  fprintf main_fmt "#include <stdlib.h>@.#include <assert.h>@.#include \"%s\"@.@." (basename^".h");
  (* Print the svn version number and the supported C standard (C90 or C99) *)
  print_version main_fmt;
  print_main_fun machines main_machine main_fmt
end  

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
