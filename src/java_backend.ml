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

(* This module is used for the lustre to Java compiler *)

open Format
open Utils
open LustreSpec
open Corelang
open Machine_code


(********************************************************************************************)
(*                     Basic      Printing functions                                        *)
(********************************************************************************************)

let pp_final_char_if_non_empty c l =
  (fun fmt -> match l with [] -> () | _ -> fprintf fmt "%s" c)

let pp_newline_if_non_empty l =
  (fun fmt -> match l with [] -> () | _ -> fprintf fmt "@,")

let pp_dimension fmt d =
  Printers.pp_expr fmt (expr_of_dimension d)

let pp_type fmt t = 
  match (Types.repr t).Types.tdesc with
    | Types.Tbool -> pp_print_string fmt "boolean" 
    | Types.Treal -> pp_print_string fmt "double" 
    | _ -> Types.print_ty fmt t

let pp_var fmt id = fprintf fmt "%a %s" pp_type id.var_type id.var_id

let pp_tag fmt t =
 pp_print_string fmt t

let rec pp_const fmt c =
  match c with
    | Const_int i -> pp_print_int fmt i
    | Const_real r -> pp_print_string fmt r
    | Const_float r -> pp_print_float fmt r
    | Const_tag t -> pp_tag fmt t
    | Const_array ca -> Format.fprintf fmt "{%a}" (Utils.fprintf_list ~sep:"," pp_const) ca

let rec pp_val m fmt v =
  match v with
    | Cst c -> pp_const fmt c
    | LocalVar v ->
      if List.exists (fun o -> o.var_id = v) m.mstep.step_outputs then
	fprintf fmt "*%s" v
      else
	pp_print_string fmt v
    | StateVar v -> fprintf fmt "%s" v
    | Fun (n, vl) -> if Basic_library.is_internal_fun n then
	Basic_library.pp_java n (pp_val m) fmt vl
      else
	fprintf fmt "%s (%a)" n (Utils.fprintf_list ~sep:", " (pp_val m)) vl

let pp_add_val m fmt i =
  if List.exists (fun o -> o.var_id = i) m.mstep.step_outputs
  then
    fprintf fmt "%s" i
  else
    fprintf fmt "&%s" i

(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)
let get_output_of_machine machines i =
  try 
    let m = List.find (fun m -> m.mname.node_id = i) machines in
    m.mstep.step_outputs
  with Not_found -> assert false

let rec pp_machine_instr m machines instance_out_list fmt instr =
  match instr with
    | MReset i -> (
      match List.assoc i m.minstances with
	| "_arrow" -> fprintf fmt "%s = true;" i
	| _ -> fprintf fmt "%s.reset();" i
    )
    | MLocalAssign (i,v) -> (
      fprintf fmt "%s = %a;" 
	i (pp_val m) v
    )
    | MStateAssign (i,v) ->
      fprintf fmt "%s = %a;" i (pp_val m) v
    | MStep ([i0], i, vl) when Basic_library.is_internal_fun i ->
      fprintf fmt "%s = %a;" i0 (Basic_library.pp_java i (pp_val m)) vl    
    | MStep ([i0], i, [init; step]) when ((List.assoc i m.minstances) = "_arrow") -> (
      fprintf fmt "@[<v 2>if (%s) {@,%s = false;@,%s = %a;@]@,@[<v 2>} else {@,%s = %a;@]@,};@,"
	    i i i0 (pp_val m) init i0 (pp_val m) step
    )
    | MStep (il, i, vl) -> (
      let out = 
	try
	  List.assoc i instance_out_list 
	with Not_found -> (eprintf "impossible to find instance %s in the list@.@?" i; 
			   assert false) 
      in 
	  fprintf fmt "%s = %s.step (%a);@,"
    	    out i
     	    (Utils.fprintf_list ~sep:", " (pp_val m)) vl;
	  Utils.fprintf_list ~sep:"@," 
	    (fun fmt (o, oname) -> fprintf fmt "%s = %s.%s;" o out oname) fmt 
	    (List.map2 
	       (fun x y -> x, y.var_id) 
	       il 
	       (get_output_of_machine machines (List.assoc i m.minstances))
	    ) 
    ) 
      	
    | MBranch (g,hl) ->
      Format.fprintf fmt "@[<v 2>switch(%a) {@,%a@,}@]"
	(pp_val m) g
	(Utils.fprintf_list ~sep:"@," (pp_machine_branch m machines instance_out_list)) hl

and pp_machine_branch m machines instance_out_list fmt (t, h) =
  Format.fprintf fmt "@[<v 2>case %a:@,%a@,break;@]" pp_tag t (Utils.fprintf_list ~sep:"@," (pp_machine_instr m machines instance_out_list)) h

(********************************************************************************************)
(*                         Java file Printing functions                                        *)
(********************************************************************************************)

let get_class_name n = match n with "_arrow" -> "boolean" | _ -> String.capitalize n

let pp_local_fields visibility = 
  fprintf_list ~sep:"@," (fun fmt v -> fprintf fmt "%s %a;" visibility pp_var v) 

let pp_local_field_instances = 
  fprintf_list ~sep:"@," 
    (fun fmt (node_inst, node_type) -> fprintf fmt "protected %s %s;" 
      (get_class_name node_type) 
      node_inst
    ) 

let pp_output_constructor fmt outputs =
  fprintf fmt "@[<v 2>public Output(%a) {@,%a@]@,}"
    (fprintf_list ~sep:"; " pp_var) outputs
    (fprintf_list ~sep:"@," (fun fmt v -> fprintf fmt "this.%s = %s;" v.var_id v.var_id)) outputs

let pp_output_class fmt step = 
  fprintf fmt "@[<v 2>public class Output {@,%a@,@,%a@]@,}@,"
    (pp_local_fields "public") step.step_outputs
    pp_output_constructor step.step_outputs

let pp_constructor fmt (name, instances) = 
  fprintf fmt "@[<v 2>public %s () {@,%a@]@,}@,"
    (String.capitalize name)
    (
      fprintf_list ~sep:"@," 
	(fun fmt (node_inst, node_type) -> 
	  match node_type with
	      "_arrow" -> fprintf fmt "%s = true;" node_inst
	    | _ -> fprintf fmt "%s = new %s();" node_inst (get_class_name node_type) 
	) 
    ) 
    instances

let pp_reset machines fmt m = 
  fprintf fmt "@[<v 2>public void reset () {@,%a@]@,}@,"
    (fprintf_list ~sep:"@," (pp_machine_instr m machines [])) m.minit

let pp_step machines fmt m : unit = 
  let out_assoc_list = 
    List.map (fun (node_inst, _) -> node_inst, "out_" ^ node_inst) m.minstances
  in
  fprintf fmt 
    "@[<v 2>public Output step (%a) {@,%a%t@,%a%a%t@,%a@,%t@]@,}@,"
    (Utils.fprintf_list ~sep:",@ " pp_var) m.mstep.step_inputs
    (* locals *)
    (Utils.fprintf_list ~sep:";@," pp_var) m.mstep.step_locals
    (pp_final_char_if_non_empty ";" m.mstep.step_locals) 
    (* declare out variables of subnode instances + out of this node *)
    (fprintf_list ~sep:"" 
       (fun fmt (ninst, ntype) -> fprintf fmt "%s.Output out_%s;@," (get_class_name ntype) ninst )) 
    (List.filter (fun (_,ntyp) -> not (ntyp = "_arrow")) m.minstances)
    (fprintf_list ~sep:";@," pp_var) m.mstep.step_outputs
    (pp_final_char_if_non_empty ";" m.mstep.step_outputs) 
    (* instructions *)
    (fprintf_list ~sep:"@," (pp_machine_instr m machines out_assoc_list)) m.mstep.step_instrs     
    (* create out object and return it *)
    (fun fmt -> fprintf fmt "return new Output(%a);" 
      (fprintf_list ~sep:"," (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_outputs 
    )
  


let print_machine machines fmt m =
  if m.mname.node_id = "_arrow" then () else ( (* We don't print arrow function *)
    fprintf fmt "@[<v 2>class %s {@,%a%t%a%t%t%a@,%a@,%a@,%a@]@,}@.@.@."
      (String.capitalize m.mname.node_id) (* class name *)
      (pp_local_fields "protected") m.mmemory                      (* fields *)
      (pp_newline_if_non_empty m.mmemory)            
      pp_local_field_instances m.minstances          (* object fields *)
      (pp_newline_if_non_empty m.minstances)         
      (pp_newline_if_non_empty m.minstances)         
      pp_output_class m.mstep                        (* class for output of step method *)
      pp_constructor (m.mname.node_id, m.minstances) (* constructor to instanciate object fields *)
      (pp_reset machines) m                               (* reset method *)
      (pp_step machines) m             (* step method *)

  )

(********************************************************************************************)
(*                         Main related functions                                           *)
(********************************************************************************************)

(* let print_get_input fmt v = *)
(*   match v.var_type.Types.tdesc with *)
(*     | Types.Tint -> fprintf fmt "_get_int(\"%s\")" v.var_id *)
(*     | Types.Tbool -> fprintf fmt "_get_bool(\"%s\")" v.var_id *)
(*     | Types.Treal -> fprintf fmt "_get_double(\"%s\")" v.var_id *)
(*     | _ -> assert false *)

(* let print_put_outputs fmt ol =  *)
(*   let po fmt o = *)
(*     match o.var_type.Types.tdesc with *)
(*     | Types.Tint -> fprintf fmt "_put_int(\"%s\", %s)" o.var_id o.var_id *)
(*     | Types.Tbool -> fprintf fmt "_put_bool(\"%s\", %s)" o.var_id o.var_id *)
(*     | Types.Treal -> fprintf fmt "_put_double(\"%s\", %s)" o.var_id o.var_id *)
(*     | _ -> assert false *)
(*   in *)
(*   List.iter (fprintf fmt "@ %a;" po) ol *)

let read_input fmt typ = match typ.Types.tdesc with
  | Types.Treal -> fprintf fmt "StdIn.readDouble()"
  | Types.Tint ->  fprintf fmt "StdIn.readInt()"
  | Types.Tbool ->  fprintf fmt "StdIn.readBoolean()"
  | _ -> assert false

let print_main_fun basename machines m fmt =
  let m_class = String.capitalize m.mname.node_id in
  fprintf fmt "@[<v 2>class %s {@,@,@[<v 2>%s {@,%t@,%t@]@,}@,@]@,}@."
    (String.capitalize basename)
    "public static void main (String[] args)"
    (fun fmt -> fprintf fmt "%s main_node = new %s();"  m_class m_class)
    (fun fmt -> fprintf fmt "@[<v 2>while (true) {@,%a@,%t@,%a@]@,}@,"  
      (fprintf_list ~sep:"@," 
	 (fun fmt v -> fprintf fmt "System.out.println(\"%s?\");@,%a = %a;" 
	   v.var_id pp_var v read_input v.var_type))
      m.mstep.step_inputs
      (fun fmt -> fprintf fmt "%s.Output out = main_node.step(%a);" 
	m_class  
	(fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs
      )
      (fprintf_list ~sep:"@," (fun fmt v -> fprintf fmt "System.out.println(\"%s = \" + out.%s);" v.var_id v.var_id))
      m.mstep.step_outputs
    )
    
    
(* let print_main_fun machines m fmt = *)
(*   let mname = m.mname.node_id in *)
(*   let main_mem = *)
(*     if (!Options.static_mem && !Options.main_node <> "") *)
(*     then "&main_mem" *)
(*     else "main_mem" in *)
(*   fprintf fmt "@[<v 2>int main (int argc, char *argv[]) {@ "; *)
(*   fprintf fmt "/* Declaration of inputs/outputs variables */@ "; *)
(*   List.iter  *)
(*     (fun v -> fprintf fmt "%a %s = %a;@ " pp_c_type v.var_type v.var_id pp_c_initialize v.var_type *)
(*     ) m.mstep.step_inputs; *)
(*   List.iter  *)
(*     (fun v -> fprintf fmt "%a %s = %a;@ " pp_c_type v.var_type v.var_id pp_c_initialize v.var_type *)
(*     ) m.mstep.step_outputs; *)
(*   fprintf fmt "@ /* Main memory allocation */@ "; *)
(*   if (!Options.static_mem && !Options.main_node <> "") *)
(*   then (fprintf fmt "%a(main_mem);@ " pp_machine_static_alloc_name mname) *)
(*   else (fprintf fmt "%a *main_mem = %a();@ " pp_machine_memtype_name mname pp_machine_alloc_name mname); *)
(*   fprintf fmt "@ /* Initialize the main memory */@ "; *)
(*   fprintf fmt "%a(%s);@ " pp_machine_reset_name mname main_mem; *)
(*   fprintf fmt "@ ISATTY = isatty(0);@ "; *)
(*   fprintf fmt "@ /* Infinite loop */@ "; *)
(*   fprintf fmt "@[<v 2>while(1){@ "; *)
(*   fprintf fmt  "fflush(stdout);@ "; *)
(*   List.iter  *)
(*     (fun v -> fprintf fmt "%s = %a;@ " *)
(*       v.var_id *)
(*       print_get_input v *)
(*     ) m.mstep.step_inputs; *)
(*   (match m.mstep.step_outputs with *)
(*     | [] -> ( *)
(*       fprintf fmt "%a(%a%t%s);@ "  *)
(* 	pp_machine_step_name mname *)
(* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs *)
(* 	(pp_final_char_if_non_empty ", " m.mstep.step_inputs) *)
(* 	main_mem *)
(*     ) *)
(*     | [o] -> ( *)
(*       fprintf fmt "%s = %a(%a%t%a, %s);%a" *)
(* 	o.var_id *)
(* 	pp_machine_step_name mname *)
(* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs *)
(* 	(pp_final_char_if_non_empty ", " m.mstep.step_inputs) *)
(* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> fprintf fmt "&%s" v.var_id)) m.mstep.step_outputs *)
(* 	main_mem *)
(* 	print_put_outputs [o]) *)
(*     | _ -> ( *)
(*       fprintf fmt "%a(%a%t%a, %s);%a" *)
(* 	pp_machine_step_name mname *)
(* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> pp_print_string fmt v.var_id)) m.mstep.step_inputs *)
(* 	(pp_final_char_if_non_empty ", " m.mstep.step_inputs) *)
(* 	(Utils.fprintf_list ~sep:", " (fun fmt v -> fprintf fmt "&%s" v.var_id)) m.mstep.step_outputs *)
(* 	main_mem *)
(* 	print_put_outputs m.mstep.step_outputs) *)
(*   ); *)
(*   fprintf fmt "@]@ }@ "; *)
(*   fprintf fmt "return 1;"; *)
(*   fprintf fmt "@]@ }@."        *)

(* let print_main_header fmt = *)
(*   fprintf fmt "#include <stdio.h>@.#include <unistd.h>@.#include \"io_frontend.h\"@." *)
  
      
(********************************************************************************************)
(*                         Translation function                                             *)
(********************************************************************************************)

let translate_to_java source_fmt basename prog machines =

  
  (* If a main node is identified, generate a main function for it *)
  let main_print =
    match !Options.main_node with
      | "" -> (fun _ -> ())
      | main_node -> (
  	let main_node_opt =
  	  List.fold_left
  	    (fun res m ->
  	      match res with
  		| Some _ -> res
  		| None -> if m.mname.node_id = main_node then Some m else None)
  	    None machines
	in
	match main_node_opt with
  	  | None -> eprintf "Unable to find a main node named %s@.@?" main_node; (fun _ -> ())
  	  | Some m -> print_main_fun basename machines m
      )
  in
  
  (* Print nodes one by one (in the previous order) *)
  List.iter ((print_machine machines) source_fmt) machines;
  main_print source_fmt 




(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
