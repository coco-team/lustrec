open Format
open LustreSpec
open Corelang
open Machine_code


let pp_machine_init_name fmt id = fprintf fmt "%s_init" id
let pp_machine_step_name fmt id = fprintf fmt "%s_step" id
let pp_machine_stateless_name fmt id = fprintf fmt "%s" id

let pp_type fmt t =
  match (Types.repr t).Types.tdesc with
  | Types.Tbool           -> Format.fprintf fmt "Bool"
  | Types.Tint            -> Format.fprintf fmt "Int"
  | Types.Treal           -> Format.fprintf fmt "Real"
  | Types.Tclock _
  | Types.Tarray _
  | Types.Tstatic _
  | Types.Tconst _
  | Types.Tarrow _
  | _                     -> Format.eprintf "internal error: pp_type %a@."
                             Types.print_ty t; assert false

let pp_decl_var fmt id =
  Format.fprintf fmt "(declare-var %s %a)"
    id.var_id
    pp_type id.var_type

let pp_var fmt id = Format.pp_print_string fmt id.var_id


let pp_conj pp fmt l =
  match l with
    [] -> assert false
  | [x] -> pp fmt x
  | _ -> fprintf fmt "(and @[<v 0>%a@]@ )" (Utils.fprintf_list ~sep:" " pp) l



let concat prefix x = if prefix = "" then x else prefix ^ "." ^ x
let rename f = (fun v -> {v with var_id = f v.var_id } )
let rename_machine p = rename (fun n -> concat p n)
let rename_machine_list p = List.map (rename_machine p)

let rename_current =  rename (fun n -> n ^ "_c")
let rename_current_list = List.map rename_current
let rename_next = rename (fun n -> n ^ "_x")
let rename_next_list = List.map rename_next


let get_machine machines node_name =
  List.find (fun m  -> m.mname.node_id = node_name) machines

let full_memory_vars machines machine =
  let rec aux fst prefix m =
    (rename_machine_list (if fst then prefix else concat prefix m.mname.node_id) m.mmemory) @
      List.fold_left (fun accu (id, (n, _)) ->
	let name = node_name n in
	if name = "_arrow" then accu else
	  let machine_n = get_machine machines name in
	( aux false (concat prefix (if fst then id else concat m.mname.node_id id)) machine_n ) @ accu
      ) [] (m.minstances)
  in
  aux true machine.mname.node_id machine

let stateless_vars machines m =
  (rename_machine_list m.mname.node_id m.mstep.step_inputs)@
    (rename_machine_list m.mname.node_id m.mstep.step_outputs)

let step_vars machines m =
  (stateless_vars machines m)@
    (rename_current_list (full_memory_vars machines m)) @
    (rename_next_list (full_memory_vars machines m))

let init_vars machines m =
  (stateless_vars machines m) @ (rename_next_list (full_memory_vars machines m))

(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)

let pp_horn_var m fmt id =
  if Types.is_array_type id.var_type
  then
    assert false (* no arrays in Horn output *)
  else
    Format.fprintf fmt "%s" id.var_id


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
    | Fun (n, vl)   -> Format.fprintf fmt "%a" (Basic_library.pp_horn n (pp_horn_val self pp_var)) vl

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
   	    o.var_type (LocalVar o) i1
        else
          pp_assign
   	    m self (pp_horn_var m) fmt
   	    o.var_type (LocalVar o) i2

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
	      (List.map (fun v -> LocalVar v) outputs)
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
	      (List.map (fun v -> LocalVar v) outputs)
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
	(List.map (fun v -> LocalVar v) outputs)
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
      i.var_type (LocalVar i) v
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (StateVar i) v
  | MStep ([i0], i, vl) when Basic_library.is_internal_fun i  ->
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

       (* Rule for init *)
       Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
	 (pp_conj (pp_instr true m.mname.node_id)) m.mstep.step_instrs
	 pp_machine_init_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m);

       (* Rule for step *)
       Format.fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a %a)@]@.))@.@."
	 (pp_conj (pp_instr false m.mname.node_id)) m.mstep.step_instrs
	 pp_machine_step_name m.mname.node_id
	 (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m);

       (* Adding assertions *)
       (match m.mstep.step_asserts with
       | [] -> ()
       | assertsl -> begin
	 let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id pp_var in

	 Format.fprintf fmt "; Asserts@.";
	 Format.fprintf fmt "(assert @[<v 2>%a@]@ )@.@.@."
	   (pp_conj pp_val) assertsl;

	 (** TEME: the following code is the one we described. But it generates a segfault in z3
	 Format.fprintf fmt "; Asserts for init@.";
	 Format.fprintf fmt "@[<v 2>(assert (=> @ (and @[<v 0>%a@]@ (%a %a))@ %a@]@.))@.@.@."
	   (Utils.fprintf_list ~sep:"@ " (pp_instr true m.mname.node_id)) m.mstep.step_instrs
	   pp_machine_init_name m.mname.node_id
	   (Utils.fprintf_list ~sep:" " pp_var) (init_vars machines m)
	   (pp_conj pp_val) assertsl;

	 Format.fprintf fmt "; Asserts for step@.";
	 Format.fprintf fmt "@[<v 2>(assert (=> @ (and @[<v 0>%a@]@ (%a %a))@ %a@]@.))@.@."
	   (Utils.fprintf_list ~sep:"@ " (pp_instr false m.mname.node_id)) m.mstep.step_instrs

	   pp_machine_step_name m.mname.node_id
	   (Utils.fprintf_list ~sep:" " pp_var) (step_vars machines m)
	   (pp_conj pp_val) assertsl
      	 *)
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
	List.filter (fun (kwds, _) -> kwds = ["horn_backend";"trace"]) all_annots
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
	if List.mem_assoc v.var_id traces then
	  (* We take the expression associated to variable v in the trace info *)
	  p, List.assoc v.var_id traces
	else
	  (* We keep the variable as is: we create an expression v *)
	  p, mkexpr Location.dummy_loc (Expr_ident v.var_id)

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
(* compile-command:"make -C .." *)
(* End: *)
