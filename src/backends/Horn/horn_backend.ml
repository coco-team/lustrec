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

       (* Rule for init *)
       Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
	 (pp_conj (pp_instr true m.mname.node_id)) m.mstep.step_instrs
	 pp_machine_init_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m);

       (* (\* Rule for step *\) *)
       (* Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@." *)
       (*   (pp_conj (pp_instr false m.mname.node_id)) m.mstep.step_instrs *)
       (*   pp_machine_step_name m.mname.node_id *)
       (*   (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m); *)


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
	    (* Format.fprintf fmt " @[<v 2>%a@]@ @.@.@." *)
            (*                 (pp_conj pp_val) assertsl; *)

          end
       );


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


let translate fmt basename prog machines =
  List.iter (print_machine machines fmt) (List.rev machines);
  main_print machines fmt


let traces_file fmt basename prog machines =
  Format.fprintf fmt
    "; Horn code traceability generated by %s@.; SVN version number %s@.@."
    (Filename.basename Sys.executable_name)
    Version.number;

  (* We extract the annotation dealing with traceability *)
  let machines_traces = List.map (fun m ->
    let traces : (ident * expr) list=
      let all_annots = List.flatten (List.map (fun ann -> ann.annots) m.mannot) in
      let filtered =
	List.filter (fun (kwds, _) -> kwds = ["traceability"]) all_annots
      in
      let content = List.map snd filtered in
      (* Elements are supposed to be a pair (tuple): variable, expression *)
      List.map (fun ee ->
	match ee.eexpr_quantifiers, ee.eexpr_qfexpr.expr_desc with
	| [], Expr_tuple [v;e] -> (
	  match v.expr_desc with
	  | Expr_ident vid -> vid, e
	  | _ -> assert false )
	| _ -> assert false)
	content
    in

    m, traces

  ) machines
  in

  (* Compute memories associated to each machine *)
  let compute_mems m =
    let rec aux fst prefix m =
      (List.map (fun mem -> (prefix, mem)) m.mmemory) @
	List.fold_left (fun accu (id, (n, _)) ->
	  let name = node_name n in
	  if name = "_arrow" then accu else
	    let machine_n = get_machine machines name in
	    ( aux false ((id,machine_n)::prefix) machine_n )
	    @ accu
	) [] m.minstances
    in
    aux true [] m
  in

  List.iter (fun m ->
    Format.fprintf fmt "; Node %s@." m.mname.node_id;

    let memories_old =
      List.map (fun (p, v) ->
	let machine = match p with | [] -> m | (_,m')::_ -> m' in
	let traces = List.assoc machine machines_traces in
	if List.mem_assoc v.var_id traces then (
	  (* We take the expression associated to variable v in the trace info *)
	  (* Format.eprintf "Found variable %a in traces: %a@."  pp_var v Printers.pp_expr (List.assoc v.var_id traces); *)
	  p, List.assoc v.var_id traces
      )
	else (
	  (* We keep the variable as is: we create an expression v *)
	  (* Format.eprintf "Unable to found variable %a in traces (%a)@."  pp_var v (Utils.fprintf_list ~sep:", " Format.pp_print_string) (List.map fst traces); *)
	  p, mkexpr Location.dummy_loc (Expr_ident v.var_id)
	)

      ) (compute_mems m)
    in
    let memories_next = (* We remove the topest pre in each expression *)
      List.map
      	(fun (prefix, ee) ->
      	  match ee.expr_desc with
      	  | Expr_pre e -> prefix, e
      	  | _ -> Format.eprintf
      	    "Mem Failure: (prefix: %a, eexpr: %a)@.@?"
      	    (Utils.fprintf_list ~sep:","
      	       (fun fmt (id,n) -> fprintf fmt "(%s,%s)" id n.mname.node_id ))
      	    (List.rev prefix)
      	    Printers.pp_expr ee;
      	    assert false)
	memories_old
    in

    let pp_prefix_rev fmt prefix =
      Utils.fprintf_list ~sep:"." (fun fmt (id,n) -> fprintf fmt "(%s,%s)" id n.mname.node_id) fmt (List.rev prefix)
    in

    Format.fprintf fmt "; Init predicate@.";

    Format.fprintf fmt "; horn encoding@.";
    Format.fprintf fmt "(%a %a)@."
      pp_machine_init_name m.mname.node_id
      (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m);

    Format.fprintf fmt "; original expressions@.";
    Format.fprintf fmt "(%a %a%t%a)@."
      pp_machine_init_name m.mname.node_id
      (Utils.fprintf_list ~sep:" " pp_var) (m.mstep.step_inputs@m.mstep.step_outputs)
      (fun fmt -> match memories_next with [] -> () | _ -> fprintf fmt " ")
      (Utils.fprintf_list ~sep:" " (fun fmt (prefix, ee) -> fprintf fmt "%a(%a)" pp_prefix_rev prefix Printers.pp_expr ee)) memories_next;

    Format.pp_print_newline fmt ();
    Format.fprintf fmt "; Step predicate@.";

    Format.fprintf fmt "; horn encoding@.";
    Format.fprintf fmt "(%a %a)@."
      pp_machine_step_name m.mname.node_id
      (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m);
    Format.fprintf fmt "; original expressions@.";
    Format.fprintf fmt "(%a %a%t%a)@."
      pp_machine_step_name m.mname.node_id
      (Utils.fprintf_list ~sep:" " pp_var) (m.mstep.step_inputs@m.mstep.step_outputs)
      (fun fmt -> match memories_old with [] -> () | _ -> fprintf fmt " ")
      (Utils.fprintf_list ~sep:" " (fun fmt (prefix,ee) -> fprintf fmt "%a(%a)" pp_prefix_rev prefix Printers.pp_expr ee)) (memories_old@memories_next);
    Format.pp_print_newline fmt ();
  ) (List.rev machines);


(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
