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
open LustreSpec
open Corelang
open Machine_code

open Horn_backend_common
open Horn_backend_printers
open Horn_backend_collecting_sem

(*
TODO:
- gerer les traces. Ca merde pour l'instant dans le calcul des memoires sur les arrows
- gerer le reset --- DONE
- reconstruire les rechable states DONE
- reintroduire le cex/traces ... DONE
- traiter les types enum et les branchements sur ces types enum (en particulier les traitements des resets qui ont lieu dans certaines branches et pas dans d'autres )
*)

(************************************************
=======
(* Used to print boolean constants *)
let pp_horn_tag fmt t =
  pp_print_string fmt (if t = tag_true then "true" else if t = tag_false then "false" else t)

(* Prints a constant value *)
let rec pp_horn_const fmt c =
  match c with
    | Const_int i    -> pp_print_int fmt i
    | Const_real (c,e,s)   -> assert false (* TODO rational pp_print_string fmt r *)
    (* | Const_float r  -> pp_print_float fmt r *)
    | Const_tag t    -> pp_horn_tag fmt t
    | _              -> assert false

(* Prints a value expression [v], with internal function calls only.
   [pp_var] is a printer for variables (typically [pp_c_var_read]),
   but an offset suffix may be added for array variables
*)
let rec pp_horn_val ?(is_lhs=false) self pp_var fmt v =
  match v.value_desc with
    | Cst c         -> pp_horn_const fmt c
    | Array _
    | Access _ -> assert false (* no arrays *)
    | Power (v, n)  -> assert false
    | LocalVar v    -> pp_var fmt (rename_machine self v)
    | StateVar v    ->
      if Types.is_array_type v.var_type
      then assert false
      else pp_var fmt (rename_machine self ((if is_lhs then rename_next else rename_current) (* self *) v))
    | Fun (n, vl)   -> Format.fprintf fmt "%a" (Basic_library.pp_horn n (pp_horn_val self pp_var)) vl

(* Prints a [value] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self pp_value fmt value =
 match value.value_desc with
 | Fun (n, vl)  ->
   Basic_library.pp_horn n (pp_value_suffix self pp_value) fmt vl
 |  _            ->
   pp_horn_val self pp_value fmt value

(* type_directed assignment: array vs. statically sized type
   - [var_type]: type of variable to be assigned
   - [var_name]: name of variable to be assigned
   - [value]: assigned value
   - [pp_var]: printer for variables
*)
let pp_assign m self pp_var fmt var_type var_name value =
  fprintf fmt "(= %a %a)" (pp_horn_val ~is_lhs:true self pp_var) var_name (pp_value_suffix self pp_var) value

let pp_instance_call
    machines ?(init=false) m self fmt i (inputs: value_t list) (outputs: var_decl list) =
  try (* stateful node instance *)
    begin
      let (n,_) = List.assoc i m.minstances in
      match node_name n, inputs, outputs with
      | "_arrow", [i1; i2], [o] -> begin
        if init then
          pp_assign
   	    m
   	    self
   	    (pp_horn_var m)
	    fmt
   	    o.var_type (mk_val (LocalVar o) o.var_type) i1
        else
          pp_assign
   	    m self (pp_horn_var m) fmt
   	    o.var_type (mk_val (LocalVar o) o.var_type) i2
	    
      end
      | name, _, _ ->
	begin
	  let target_machine = List.find (fun m  -> m.mname.node_id = name) machines in
	  if init then
	    Format.fprintf fmt "(%a %a%t%a%t%a)"
	      pp_machine_init_name (node_name n)
	      (* inputs *)
	      (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m)))
	      inputs
	      (Utils.pp_final_char_if_non_empty " " inputs)
	      (* outputs *)
	      (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) 
	      (List.map (fun v -> mk_val (LocalVar v) v.var_type) outputs)
	      (Utils.pp_final_char_if_non_empty " " outputs)
	      (* memories (next) *)
	      (Utils.fprintf_list ~sep:" " pp_var) (
  		rename_machine_list
		  (concat m.mname.node_id i)
		  (rename_next_list (full_memory_vars machines target_machine)
		  )
	       )
	  else
	    Format.fprintf fmt "(%a %a%t%a%t%a)"
	      pp_machine_step_name (node_name n)
	      (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) inputs
	      (Utils.pp_final_char_if_non_empty " " inputs) 
	      (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) 
	      (List.map (fun v -> mk_val (LocalVar v) v.var_type) outputs)
	      (Utils.pp_final_char_if_non_empty " " outputs)
	      (Utils.fprintf_list ~sep:" " pp_var) (
		(rename_machine_list
		   (concat m.mname.node_id i)
		   (rename_current_list (full_memory_vars machines target_machine))
		) @
		  (rename_machine_list
		     (concat m.mname.node_id i)
		     (rename_next_list (full_memory_vars machines target_machine))
		  )
	       )

	end
    end
    with Not_found -> ( (* stateless node instance *)
      let (n,_) = List.assoc i m.mcalls in
      Format.fprintf fmt "(%s %a%t%a)"
	(node_name n)
	(Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m)))
	inputs
	(Utils.pp_final_char_if_non_empty " " inputs) 
	(Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) 
	(List.map (fun v -> mk_val (LocalVar v) v.var_type) outputs)
    )

let pp_machine_init (m: machine_t) self fmt inst =
  let (node, static) = List.assoc inst m.minstances in
  fprintf fmt "(%a %a%t%s->%s)"
    pp_machine_init_name (node_name node)
    (Utils.fprintf_list ~sep:" " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty " " static)
    self inst

(* TODO *)
let rec pp_conditional machines ?(init=false)  (m: machine_t) self fmt c tl el =
  fprintf fmt "@[<v 2>if (%a) {%t%a@]@,@[<v 2>} else {%t%a@]@,}"
    (pp_horn_val self (pp_horn_var m)) c
    (Utils.pp_newline_if_non_empty tl)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) tl
    (Utils.pp_newline_if_non_empty el)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) el

and pp_machine_instr machines ?(init=false) (m: machine_t) self fmt instr =
  match instr with
  | MReset i ->
    pp_machine_init m self fmt i
  | MLocalAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (mk_val (LocalVar i) i.var_type) v
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (mk_val (StateVar i) i.var_type) v
  | MStep ([i0], i, vl) when Basic_library.is_value_internal_fun (mk_val (Fun (i, vl)) i0.var_type)  -> 
    assert false (* This should not happen anymore *)
  | MStep (il, i, vl) ->
    pp_instance_call machines ~init:init m self fmt i vl il
  | MBranch (g,hl) ->
    if hl <> [] && let t = fst (List.hd hl) in t = tag_true || t = tag_false
    then (* boolean case, needs special treatment in C because truth value is not unique *)
      (* may disappear if we optimize code by replacing last branch test with default *)
      let tl = try List.assoc tag_true  hl with Not_found -> [] in
      let el = try List.assoc tag_false hl with Not_found -> [] in
      pp_conditional machines ~init:init m self fmt g tl el
    else assert false (* enum type case *)
  | MComment _ -> ()


(**************************************************************)

let is_stateless m = m.minstances = [] && m.mmemory = []

(* Print the machine m:
   two functions: m_init and m_step
   - m_init is a predicate over m memories
   - m_step is a predicate over old_memories, inputs, new_memories, outputs
   We first declare all variables then the two /rules/.
*)
let print_machine machines fmt m =
  let pp_instr init = pp_machine_instr machines ~init:init m in
  if m.mname.node_id = arrow_id then
    (* We don't print arrow function *)
    ()
  else
    begin
      Format.fprintf fmt "; %s@." m.mname.node_id;

   (* Printing variables *)
   Utils.fprintf_list ~sep:"@." pp_decl_var fmt
     ((step_vars machines m)@
	 (rename_machine_list m.mname.node_id m.mstep.step_locals));
   Format.pp_print_newline fmt ();



   if is_stateless m then
     begin
       (* Declaring single predicate *)
       Format.fprintf fmt "(declare-rel %a (%a))@."
	 pp_machine_stateless_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_type)
	 (List.map (fun v -> v.var_type) (stateless_vars machines m));

       (* Rule for single predicate *)
       Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
	 (pp_conj (pp_instr
		     true (* In this case, the boolean init can be set to true or false.
			     The node is stateless. *)
		     m.mname.node_id)
	 )
	 m.mstep.step_instrs
	 pp_machine_stateless_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_var) (stateless_vars machines m);
     end
   else
     begin
       (* Declaring predicate *)
       Format.fprintf fmt "(declare-rel %a (%a))@."
	 pp_machine_init_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_type)
	 (List.map (fun v -> v.var_type) (init_vars machines m));

       Format.fprintf fmt "(declare-rel %a (%a))@."
	 pp_machine_step_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_type)
	 (List.map (fun v -> v.var_type) (step_vars machines m));

       Format.pp_print_newline fmt ();

      (* Adding assertions *)
       (match m.mstep.step_asserts with
       | [] ->
          begin
            (* Rule for init *)
            Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
	                   (pp_conj (pp_instr true m.mname.node_id)) m.mstep.step_instrs
	                   pp_machine_init_name m.mname.node_id
	                   (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m);
            (* Rule for step*)
            Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
                           (pp_conj (pp_instr false m.mname.node_id)) m.mstep.step_instrs
                           pp_machine_step_name m.mname.node_id
                           (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m);
          end
       | assertsl ->
          begin
	    let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id pp_var in
            (* print_string pp_val; *)
            let instrs_concat = m.mstep.step_instrs in
            Format.fprintf fmt "; with Assertions @.";
            (*Rule for init*)
            Format.fprintf fmt "@[<v 2>(rule (=> @ (and @ %a@. %a)(%a %a)@]@.))@.@."
                           (pp_conj (pp_instr true m.mname.node_id)) instrs_concat
                           (pp_conj pp_val) assertsl
                           pp_machine_init_name m.mname.node_id
                           (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m);
            (*Rule for step*)
            Format.fprintf fmt "@[<v 2>(rule (=> @ (and @ %a@. %a)(%a %a)@]@.))@.@."
                           (pp_conj (pp_instr false m.mname.node_id)) instrs_concat
                           (pp_conj pp_val) assertsl
                           pp_machine_step_name m.mname.node_id
                           (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m);
          end
       );
       
(*
       match m.mspec with
	 None -> () (* No node spec; we do nothing *)
       | Some {requires = []; ensures = [EnsuresExpr e]; behaviors = []} -> 
	 ( 
       (* For the moment, we only deal with simple case: single ensures, no other parameters *)
	   ()
	     
	 )
       | _ -> () (* Other cases give nothing *)
*)      
     end
    end



let collecting_semantics machines fmt node machine =
    Format.fprintf fmt "; Collecting semantics for node %s@.@." node;
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

    (* Special case when the main node is stateless *)
    let init_name, step_name =
      if is_stateless machine then
	pp_machine_stateless_name, pp_machine_stateless_name
      else
	pp_machine_init_name, pp_machine_step_name
    in

    Format.fprintf fmt "(declare-rel MAIN (%a))@."
      (Utils.fprintf_list ~sep:" " pp_type)
      (List.map (fun v -> v.var_type) main_memory_next);

    Format.fprintf fmt "; Initial set@.";
    Format.fprintf fmt "(declare-rel INIT_STATE ())@.";
    Format.fprintf fmt "(rule INIT_STATE)@.";
    Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>INIT_STATE@ (@[<v 0>%a %a@])@]@ )@ (MAIN %a)@]@.))@.@."
      init_name node
      (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines machine)
      (Utils.fprintf_list ~sep:" " pp_var) main_memory_next ;

    Format.fprintf fmt "; Inductive def@.";
    (Utils.fprintf_list ~sep:" " (fun fmt v -> Format.fprintf fmt "%a@." pp_decl_var v)) fmt main_output_dummy;
    Format.fprintf fmt
      "@[<v 2>(rule (=> @ (and @[<v 0>(MAIN %a)@ (@[<v 0>%a %a@])@]@ )@ (MAIN %a)@]@.))@.@."
      (Utils.fprintf_list ~sep:" " pp_var) main_memory_current
      step_name node
      (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines machine)
      (Utils.fprintf_list ~sep:" " pp_var) main_memory_next

let check_prop machines fmt node machine =
  let main_output =
    rename_machine_list machine.mname.node_id machine.mstep.step_outputs
  in
  let main_memory_next =
    (rename_next_list (full_memory_vars machines machine)) @ main_output
  in
  Format.fprintf fmt "; Property def@.";
  Format.fprintf fmt "(declare-rel ERR ())@.";
  Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>(not %a)@ (MAIN %a)@])@ ERR))@."
    (pp_conj pp_var) main_output
    (Utils.fprintf_list ~sep:" " pp_var) main_memory_next
    ;
  if !Options.horn_queries then
    Format.fprintf fmt "(query ERR)@."


let cex_computation machines fmt node machine =
    Format.fprintf fmt "; CounterExample computation for node %s@.@." node;
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
    let init_name, step_name =
      if is_stateless machine then
	pp_machine_stateless_name, pp_machine_stateless_name
      else
	pp_machine_init_name, pp_machine_step_name
    in

    Format.fprintf fmt "(declare-rel CEX (Int %a))@.@."
      (Utils.fprintf_list ~sep:" " pp_type)
      (List.map (fun v -> v.var_type) cex_memory_next);

    Format.fprintf fmt "; Initial set@.";
    Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>INIT_STATE@ (@[<v 0>%a %a@])@]@ )@ (CEX 0 %a)@]@.))@.@."
      init_name node
      (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines machine)
      (Utils.fprintf_list ~sep:" " pp_var) cex_memory_next ;

    Format.fprintf fmt "; Inductive def@.";
    (* Declare dummy inputs. Outputs should have been declared previously with collecting sem *)
    (Utils.fprintf_list ~sep:" " (fun fmt v -> Format.fprintf fmt "%a@." pp_decl_var v)) fmt cex_input_dummy;
    Format.fprintf fmt "(declare-var cexcpt Int)@.";
    Format.fprintf fmt
      "@[<v 2>(rule (=> @ (and @[<v 0>(CEX cexcpt %a)@ (@[<v 0>%a %a@])@]@ )@ (CEX (+ 1 cexcpt) %a)@]@.))@.@."
      (Utils.fprintf_list ~sep:" " pp_var) cex_memory_current
      step_name node
      (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines machine)
      (Utils.fprintf_list ~sep:" " pp_var) cex_memory_next

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
  Format.fprintf fmt "; Property def@.";
  Format.fprintf fmt "(declare-rel CEXTRACE ())@.";
  Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>(not %a)@ (CEX cexcpt %a)@])@ CEXTRACE))@."
    (pp_conj pp_var) cex_output
    (Utils.fprintf_list ~sep:" " pp_var) cex_memory_next
    ;
  Format.fprintf fmt "(query CEXTRACE)@."


************************************)
let main_print machines fmt =
if !Options.main_node <> "" then
  begin
    let node = !Options.main_node in
    let machine = get_machine machines node in
    collecting_semantics machines fmt node machine;
    check_prop machines fmt node machine;
    if !Options.horn_cex then(
      cex_computation machines fmt node machine;
      get_cex machines fmt node machine)
end

let print_type_definitions fmt =
  let cpt_type = ref 0 in
  Hashtbl.iter (fun typ decl ->
    match typ with
    | Tydec_const var ->
      (match decl.top_decl_desc with
      | TypeDef tdef -> (
	match tdef.tydef_desc with
	| Tydec_enum tl ->
	  incr cpt_type;
	  fprintf fmt "(declare-datatypes () ((%s %a)));@.@."
	    var
	    (Utils.fprintf_list ~sep:" " pp_print_string) tl
	| _ -> assert false
      )
      | _ -> assert false
      )
    | _        -> ()) type_table


let translate fmt basename prog machines =
  (* We print typedef *)
  print_type_definitions fmt;
  List.iter (print_machine machines fmt) (List.rev machines);
  main_print machines fmt

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
