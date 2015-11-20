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


(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)

let pp_horn_var m fmt id =
  if Types.is_array_type id.var_type
  then
    assert false (* no arrays in Horn output *)
  else
    fprintf fmt "%s" id.var_id

(* Used to print boolean constants *)
let pp_horn_tag fmt t =
  pp_print_string fmt (if t = tag_true then "true" else if t = tag_false then "false" else t)

(* Prints a constant value *)
let rec pp_horn_const fmt c =
  match c with
    | Const_int i    -> pp_print_int fmt i
    | Const_real r   -> pp_print_string fmt r
    | Const_float r  -> pp_print_float fmt r
    | Const_tag t    -> pp_horn_tag fmt t
    | _              -> assert false

(* Prints a value expression [v], with internal function calls only.
   [pp_var] is a printer for variables (typically [pp_c_var_read]),
   but an offset suffix may be added for array variables
*)
let rec pp_horn_val ?(is_lhs=false) self pp_var fmt v =
  match v with
    | Cst c         -> pp_horn_const fmt c
    | Array _
    | Access _ -> assert false (* no arrays *)
    | Power (v, n)  -> assert false
    | LocalVar v    -> pp_var fmt (rename_machine self v)
    | StateVar v    ->
      if Types.is_array_type v.var_type
      then assert false
      else pp_var fmt (rename_machine self ((if is_lhs then rename_next else rename_current) (* self *) v))
    | Fun (n, vl)   -> fprintf fmt "%a" (Basic_library.pp_horn n (pp_horn_val self pp_var)) vl

(* Prints a [value] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self pp_value fmt value =
 match value with
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
  fprintf fmt "(= %a %a)" 
    (pp_horn_val ~is_lhs:true self pp_var) var_name
    (pp_value_suffix self pp_var) value
    

(* In case of no reset call, we define mid_mem = current_mem *)
let pp_no_reset machines m target_machine i fmt =
  let m_list = 
    rename_machine_list
      (concat m.mname.node_id i)
      (rename_mid_list (full_memory_vars machines target_machine))
  in
  let c_list =
    rename_machine_list
      (concat m.mname.node_id i)
      (rename_current_list (full_memory_vars machines target_machine))
  in
  match c_list, m_list with
  | [chd], [mhd] ->
    fprintf fmt "(= %a %a)"
      (pp_horn_var m) mhd
      (pp_horn_var m) chd
  
  | _ -> (
    fprintf fmt "@[<v 0>(and @[<v 0>";
    List.iter2 (fun mhd chd -> 
      fprintf fmt "(= %a %a)@ "
      (pp_horn_var m) mhd
      (pp_horn_var m) chd
    )
      m_list
      c_list      ;
    fprintf fmt ")@]@ @]"
  )

let pp_instance_reset machines m self fmt i =
  let (n,_) = List.assoc i m.minstances in
  let target_machine = List.find (fun m  -> m.mname.node_id = (node_name n)) machines in
  
  fprintf fmt "(%a @[<v 0>%a)@]"
    pp_machine_reset_name (node_name n)
    (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) 
    (
      (rename_machine_list
	 (concat m.mname.node_id i)
	 (rename_current_list (full_memory_vars machines target_machine))
      ) 
      @
	(rename_machine_list
	   (concat m.mname.node_id i)
	   (rename_mid_list (full_memory_vars machines target_machine))
	)
    )

let pp_instance_call machines reset_instances m self fmt i inputs outputs =
  try (* stateful node instance *)
    begin
      let (n,_) = List.assoc i m.minstances in
      let target_machine = List.find (fun m  -> m.mname.node_id = node_name n) machines in
      (* Checking whether this specific instances has been reset yet *)
      if not (List.mem i reset_instances) then
	(* If not, declare mem_m = mem_c *)
	pp_no_reset machines m target_machine i fmt;
      
      let mems = full_memory_vars machines target_machine in
      let rename_mems f = rename_machine_list (concat m.mname.node_id i) (f mems) in
      let mid_mems = rename_mems rename_mid_list in
      let next_mems = rename_mems rename_next_list in

      match node_name n, inputs, outputs, mid_mems, next_mems with
      | "_arrow", [i1; i2], [o], [mem_m], [mem_x] -> begin
	fprintf fmt "@[<v 5>(and ";
	fprintf fmt "(= %a (ite %a %a %a))"
	  (pp_horn_val ~is_lhs:true self (pp_horn_var m)) (LocalVar o) (* output var *)
	  (pp_horn_var m) mem_m 
	  (pp_horn_val self (pp_horn_var m)) i1
	  (pp_horn_val self (pp_horn_var m)) i2
	;
	fprintf fmt "@ ";
	fprintf fmt "(= %a false)" (pp_horn_var m) mem_x;
	fprintf fmt ")@]"
      end

      | node_name_n -> begin
	fprintf fmt "(%a @[<v 0>%a%t%a%t%a)@]"
	  pp_machine_step_name (node_name n)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m))) inputs
	  (Utils.pp_final_char_if_non_empty "@ " inputs)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
	  (List.map (fun v -> LocalVar v) outputs)
	  (Utils.pp_final_char_if_non_empty "@ " outputs)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (mid_mems@next_mems)
	
      end
    end
  with Not_found -> ( (* stateless node instance *)
    let (n,_) = List.assoc i m.mcalls in
    fprintf fmt "(%s @[<v 0>%a%t%a)@]"
      (node_name n)
      (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
      inputs
      (Utils.pp_final_char_if_non_empty "@ " inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
      (List.map (fun v -> LocalVar v) outputs)
  )


(* let rec pp_bool_conditional machines ?(init=false)  (m: machine_t) self fmt c tl el = *)
(*   fprintf fmt "@[<v 2>if (%a) {%t%a@]@,@[<v 2>} else {%t%a@]@,}" *)
(*     (pp_horn_val self (pp_horn_var m)) c *)
(*     (Utils.pp_newline_if_non_empty tl) *)
(*     (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) tl *)
(*     (Utils.pp_newline_if_non_empty el) *)
(*     (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) el *)

(* and pp_enum_conditional machines ?(init=false)  (m: machine_t) self fmt g hl = *)
(* (\* TODO: check that the enum has all its constructor defined: Xavier how have you handled that, could we have partial definition? *\) *)
(*   match hl with *)
(*   | [] -> assert false *)
(*   | [el] -> Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self) fmt el *)
(*   | hd::tl -> *)
(*   fprintf fmt "@[<v 2>if (= %a %a) {%t%a@]@,@[<v 2>} else {@.(%a)xxxx@]@,}" *)
(*     (pp_horn_val self (pp_horn_var m)) c *)
(*     TODOg *)
(*     (Utils.pp_newline_if_non_empty tl) *)
(*     (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) hd *)
(*     pp_print_newline fmt; *)
    
    

let rec pp_machine_instr machines reset_instances (m: machine_t) self fmt instr =
  match instr with
  | MReset i -> (* we assign middle_mem with reset: reset(mem_m) *)
    pp_instance_reset machines m self fmt i;
    i::reset_instances
  | MLocalAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (LocalVar i) v;
    reset_instances
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (StateVar i) v;
    reset_instances
  | MStep ([i0], i, vl) when Basic_library.is_internal_fun i  ->
    assert false (* This should not happen anymore *)
  | MStep (il, i, vl) ->
      (* if reset instance, just print the call over mem_m , otherwise declare mem_m =
	 mem_c and print the call to mem_m *)
    pp_instance_call machines reset_instances m self fmt i vl il;
    reset_instances (* Since this instance call will only happen once, we
		       don't have to update reset_instances *)
  | MBranch (g,hl) -> (* should not be produced yet. Later, we will have to
			 compare the reset_instances of each branch and
			 introduced the mem_m = mem_c for branches to do not
			 address it while other did. Am I clear ? *)
    assert false
      
  (* if hl <> [] && let t = fst (List.hd hl) in t = tag_true || t = tag_false *)
  (* then (\* boolean case, needs special treatment in C because truth value is not unique *\) *)
  (*   (\* may disappear if we optimize code by replacing last branch test with default *\) *)
  (*   let tl = try List.assoc tag_true  hl with Not_found -> [] in *)
  (*   let el = try List.assoc tag_false hl with Not_found -> [] in *)
  (*   pp_bool_conditional machines ~init:init m self fmt g tl el *)
  (* else (\* enum type case *\) *)

  (*   pp_enum_conditional machines ~init:init m self fmt g hl  *)
and pp_machine_instrs machines reset_instances m fmt instrs = 
  let ppi rs fmt i = pp_machine_instr machines rs m m.mname.node_id fmt i in
  match instrs with
  | [] -> assert false
  | [x] -> ppi reset_instances fmt x 
  | _::_ ->
    fprintf fmt "(and @[<v 0>";
    let rs = List.fold_left (fun rs i -> 
      let rs = ppi rs fmt i in
      fprintf fmt "@ ";
      rs
    )
      reset_instances instrs 
    in
    fprintf fmt "@]@ )";
    rs

let pp_machine_reset machines fmt m =
  let locals = local_memory_vars machines m in
  fprintf fmt "@[<v 5>(and @ ";

  (* print "x_m = x_c" for each local memory *)
  (Utils.fprintf_list ~sep:"@ " (fun fmt v -> 
    fprintf fmt "(= %a %a)"
      (pp_horn_var m) (rename_mid v)
      (pp_horn_var m) (rename_current v)
   )) fmt locals;
  fprintf fmt "@ ";

  (* print "child_reset ( associated vars _ {c,m} )" for each subnode.
     Special treatment for _arrow: _first = true
  *)
  (Utils.fprintf_list ~sep:"@ " (fun fmt (id, (n, _)) ->
    let name = node_name n in
    if name = "_arrow" then ( 
      fprintf fmt "(= %s._arrow._first_m true)"
	(concat m.mname.node_id id)  
    ) else (
      let machine_n = get_machine machines name in 
      fprintf fmt "(%s_reset @[<hov 0>%a@])" 
	name
	(Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) 
	(rename_machine_list (concat m.mname.node_id id) (reset_vars machines machine_n))
    )
   )) fmt m.minstances;

  fprintf fmt "@]@ )"



(**************************************************************)

let is_stateless m = m.minstances = [] && m.mmemory = []

(* Print the machine m:
   two functions: m_init and m_step
   - m_init is a predicate over m memories
   - m_step is a predicate over old_memories, inputs, new_memories, outputs
   We first declare all variables then the two /rules/.
*)
let print_machine machines fmt m =
  if m.mname.node_id = arrow_id then
    (* We don't print arrow function *)
    ()
  else
    begin
      fprintf fmt "; %s@." m.mname.node_id;
      
      (* Printing variables *)
      Utils.fprintf_list ~sep:"@." pp_decl_var fmt
	(
	  (inout_vars machines m)@
	    (rename_current_list (full_memory_vars machines m)) @
	    (rename_mid_list (full_memory_vars machines m)) @
	    (rename_next_list (full_memory_vars machines m)) @
	    (rename_machine_list m.mname.node_id m.mstep.step_locals)
	);
      pp_print_newline fmt ();

      if is_stateless m then
	begin
	  (* Declaring single predicate *)
	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_stateless_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (inout_vars machines m));

	  (* Rule for single predicate *)
	  fprintf fmt "@[<v 2>(rule (=> @ ";
	  ignore (pp_machine_instrs machines ([] (* No reset info for stateless nodes *) )  m fmt m.mstep.step_instrs);
	  fprintf fmt "@ (%a %a)@]@.))@.@."
	    pp_machine_stateless_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (inout_vars machines m);
	end
      else
	begin
	  (* Declaring predicate *)
	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_reset_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (reset_vars machines m));

	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_step_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (step_vars machines m));

	  pp_print_newline fmt ();

	  (* Rule for reset *)
	  fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a @[<v 0>%a)@]@]@.))@.@."
	    (pp_machine_reset machines) m 
	    pp_machine_reset_name m.mname.node_id
	    (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (reset_vars machines m);

          match m.mstep.step_asserts with
	  | [] ->
	    begin

	      (* Rule for step*)
	      fprintf fmt "@[<v 2>(rule (=> @ ";
	      ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	      fprintf fmt "@ (%a @[<v 0>%a)@]@]@.))@.@."
		pp_machine_step_name m.mname.node_id
		(Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (step_vars machines m);
	    end
	  | assertsl -> 
	    begin
	      let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id (pp_horn_var m) in
	      (* print_string pp_val; *)
	      fprintf fmt "; with Assertions @.";
	      
	      (*Rule for step*)
	      fprintf fmt "@[<v 2>(rule (=> @ (and @ ";
	      ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	      fprintf fmt "@. %a)(%a @[<v 0>%a)@]@]@.))@.@." (pp_conj pp_val) assertsl
		pp_machine_step_name m.mname.node_id
		(Utils.fprintf_list ~sep:" " (pp_horn_var m)) (step_vars machines m);
	    end
	      
	      
	end
    end


(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
