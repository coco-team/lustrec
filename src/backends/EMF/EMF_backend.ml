(* EMF backend *)
(* This backup is initially motivated by the need to express Spacer computed
   invariants as Matlab Simulink executable evidences.

   Therefore the input language is more restricted. We do not expect fancy
   datastructure, complex function calls, etc.

   In case of error, use -node foo -inline to eliminate function calls that are
   not seriously handled yet.
   

   In terms of algorithm, the process was initially based on printing normalized
   code. We now rely on machine code printing. The old code is available in old
   commits (eg in dd71e482a9d0).

   
   A (normalized) node becomes a JSON struct
   node foo (in1, in2: int) returns (out1, out2: int);
   var x : int;
   let
   x = bar(in1, in2); -- a stateful node
   out1 = x;
   out2 = in2;
   tel

   Since foo contains a stateful node, it is stateful itself. Its prototype is 
   extended with a reset input. When the node is reset, each of its "pre" expression
   is reset as well as all calls to stateful node it contains. 

   will produce the following JSON struct:
   "foo": {"kind":  "stateful",
   inputs:  [{name: "in1", type: "int"}, 
   {name: "in2", type: "int"}, 
   ],
   outputs: [{name: "out1", type: "int"}, {name: "out2", type: "int"}],
   locals:  [{name: "x", type: "int"}],
   instrs:  {
   def_x: { lhs: ["x"], 
   rhs: {type: "statefulcall", name: "bar", 
   args: [in1, in2], reset: [ni4_reset] } 
   }
   
   def_out1: { lhs: "out1", rhs: "x" } ,
   def_out2: { lhs: "out2", rhs: "in2"}
   }
   }

   Basically we have the following different definitions
   1. local assign of a variable to another one:
   def_out1: { kind: "local_assign", lhs: "out1", rhs: "x" },

   2. pre construct over a variable (this is a state assign):
   def_pre_x: { kind: "pre", lhs: "pre_x", rhs: "x" },

   3. arrow constructs, while there is not specific input, it could be reset 
   by a specific signal. We register it as a fresh rhs var:
   def_arrow: { kind: "arrow", name: "ni4", lhs: "is_init", rhs: "reset_ni4"}

   2. call to a stateless function, typically an operator
   def_x: { kind: "statelesscall", lhs: ["x"], 
   name: "bar", rhs: [in1, in2]} 
   
   or in the operator version 
   def_x: { kind: "operator", lhs: ["x"], 
   name: "+", rhs: [in1, in2]} 
   

   In Simulink this should introduce a subsystem in the first case or a 
   regular block in the second with card(lhs) outputs and card{args} inputs.

   3. call to a stateful node. It is similar to the stateless above, 
   with the addition of the reset argument
   { def_x: { kind: "statefulcall", lhs: ["x"], 
   name: "bar", rhs: [in1, in2], reset: [ni4_reset] } 
   }
   
   In lustrec compilation phases, a unique id is associated to this specific
   instance of stateful node "bar", here ni4. 
   Instruction such as reset(ni4) or noreset(ni4) may -- or not -- reset this 
   specific node. This corresponds to "every c" suffix of a node call in lustre.

   In Simulink this should introduce a subsystem that has this extra reset input.  
   The reset should be defined as an "OR" over (1) the input reset of the parent 
   node, __reset in the present example and (2) any occurence of reset(ni4) in 
   the instructions.

   4. branching construct: (guard expr, (tag, instr list) list)
   "merge_XX": { type: "branch", guard: "var_guard", 
   inputs:   ["varx", "vary"],
   outputs:  ["vark", "varz"],
   branches: {"tag1": {liste_of_definitions (1-4)}, ...}
   }
   

   In Simulink, this should become one IF block to produce enable ports 
   "var_guard == tag1", "var_guard == tag2", .... as well as one action 
   block per branch: each of these action block shall  

*)

open Lustre_types
open Machine_code_types
open Machine_code_common
open Format 
open EMF_common
exception Unhandled of string

module ISet = Utils.ISet
let fprintf_list = Utils.fprintf_list
  


(**********************************************)
(*   Utility functions: arrow and lustre expr *)
(**********************************************)

(* detect whether the instruction i represents an ARROW, ie an arrow with true
   -> false *)
let is_arrow_fun m i =
  match Corelang.get_instr_desc i with
  | MStep ([var], i, vl) ->
     (
       try
	 let name = (get_node_def i m).node_id in
	 match name, vl with
	 | "_arrow", [v1; v2] -> (
	   match v1.value_desc, v2.value_desc with
	   | Cst c1, Cst c2 ->
	      if c1 = Corelang.const_of_bool true && c2 = Corelang.const_of_bool false then
		true
	      else
		assert false (* only handle true -> false *)
	   | _ -> assert false
	 )
	    
	 | _ -> false
       with
       | Not_found -> false (* Not declared (should have been detected now, or
			       imported node) *)
     )
  | _ -> false
     
     
     
let is_resetable_fun lustre_eq =
  (* We extract the clock if it exist from the original lustre equation *)
  match lustre_eq with
  | Some eq -> (
    match eq.eq_rhs.expr_desc with
    | Expr_appl(_,_,reset) -> (
      match reset with None -> false | Some _ -> true
    )
    | _ ->  assert false
  )
  | None -> assert false (* should have been assigned to an original lustre equation *)

(**********************************************)
(*   Printing machine code as EMF             *)
(**********************************************)

     

let branch_cpt = ref 0
let get_instr_id fmt i =
  match Corelang.get_instr_desc i with
  | MLocalAssign(lhs,_) | MStateAssign (lhs, _) -> pp_var_name fmt lhs
  | MReset i | MNoReset i -> fprintf fmt "%s" (reset_name i)
  | MBranch (g, _) -> incr branch_cpt; fprintf fmt "branch_%i" !branch_cpt
  | MStep (outs, id, _) ->
     print_protect fmt 
       (fun fmt -> fprintf fmt "%a_%s" (fprintf_list ~sep:"_" pp_var_name) outs id)
  | _ -> () (* No name *)

let rec branch_block_vars m il =
  List.fold_left
    (fun (accu_all_def, accu_def, accu_read) i ->
      let all_defined_vars, common_def_vars, read_vars = branch_instr_vars m i in
      ISet.union accu_all_def all_defined_vars,
      ISet.union accu_def common_def_vars,
      VSet.union accu_read read_vars)
    (ISet.empty, ISet.empty, VSet.empty) il
and branch_instr_vars m i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (var,expr) 
  | MStateAssign (var,expr) -> ISet.singleton var.var_id, ISet.singleton var.var_id, get_expr_vars expr
  | MStep (vars, f, args)  ->
     let is_stateful = List.mem_assoc f m.minstances in 
     let lhs = ISet.of_list (List.map (fun v -> v.var_id) vars) in
     let args_vars =
       List.fold_left (fun accu v -> VSet.union accu (get_expr_vars v)) VSet.empty args in
     
     lhs, lhs,
     (
       if is_stateful && is_resetable_fun i.lustre_eq then
	 let reset_var =
	   let loc = Location.dummy_loc in
	   Corelang.mkvar_decl loc
	     (reset_name f,
	      Corelang.mktyp loc Tydec_bool, Corelang.mkclock loc Ckdec_any,
	      false,
	      None,
	      None) 
	 in
	 VSet.add reset_var args_vars
       else
	 args_vars
     )
  | MBranch (g,(_,hd_il)::tl)     -> (* We focus on variables defined in all branches *)
     let read_guard = get_expr_vars g in
     let all_def_vars_hd, def_vars_hd, read_vars_hd = branch_block_vars m hd_il in
     let all_def_vars, def_vars, read_vars =
       List.fold_left
	 (fun (all_def_vars, def_vars, read_vars) (_, il) ->
	   (* We accumulate reads but intersect writes *)
	   let all_writes_il, writes_il, reads_il = branch_block_vars m il in
	   ISet.union all_def_vars all_writes_il,
	   ISet.inter def_vars writes_il,
	   VSet.union read_vars reads_il
	 )
	 (all_def_vars_hd, def_vars_hd, read_vars_hd)
	 tl
     in
     all_def_vars, def_vars, VSet.union read_guard read_vars
  | MBranch _ -> assert false (* branch instruction should admit at least one case *)
  | MReset ni           
  | MNoReset ni ->
     let write = ISet.singleton (reset_name ni) in
     write, write, VSet.empty
  | MComment _ -> assert false (* not  available for EMF output *)
     
(* A kind of super join_guards: all MBranch are postponed and sorted by
   guards so they can be easier merged *)
let merge_branches instrs =
  let instrs, branches =
    List.fold_right (fun i (il, branches) ->
      match Corelang.get_instr_desc i with
	MBranch _ -> il, i::branches
      | _ -> i::il, branches
    ) instrs ([],[])
  in
  let sorting_branches b1 b2 =
    match Corelang.get_instr_desc b1, Corelang.get_instr_desc b2 with
    | MBranch(g1, hl1), MBranch(g2, hl) ->
       compare g1 g2
    | _ -> assert false
  in
  let sorted_branches = List.sort sorting_branches branches in
  instrs @ (join_guards_list sorted_branches)
    
let rec pp_emf_instr m fmt i =
  let pp_content fmt i =
    match Corelang.get_instr_desc i with
    | MLocalAssign(lhs, expr)
      -> (
	(match expr.value_desc with
	| Fun (fun_id, vl) -> (
	  (* Thanks to normalization, vl shall only contain constant or
	     local/state vars but not calls to other functions *)
	  fprintf fmt "\"kind\": \"operator\",@ ";
	  fprintf fmt "\"lhs\": \"%a\",@ " pp_var_name lhs;
	  fprintf fmt "\"name\": \"%s\",@ \"args\": [@[%a@]]"
	    fun_id
	    pp_emf_cst_or_var_list vl
	)	 
	| Array _ | Access _ | Power _ -> assert false (* No array expression allowed yet *)
	| Cst _ 
	| LocalVar _
	| StateVar _ -> (
	  fprintf fmt "\"kind\": \"local_assign\",@ \"lhs\": \"%a\",@ \"rhs\": %a"
	    pp_var_name lhs
	    pp_emf_cst_or_var expr
	))    )

    | MStateAssign(lhs, expr) (* a Pre construct Shall only be defined by a
				 variable or a constant, no function anymore! *)
      -> (
	fprintf fmt "\"kind\": \"pre\",@ \"lhs\": \"%a\",@ \"rhs\": %a"
	  pp_var_name lhs
	  pp_emf_cst_or_var expr
      )
       
    | MReset id           
      -> (
	fprintf fmt "\"kind\": \"reset\",@ \"lhs\": \"%s\",@ \"rhs\": \"true\""
	  (reset_name id)
      )
    | MNoReset id           
      -> (
	fprintf fmt "\"kind\": \"reset\",@ \"lhs\": \"%s\",@ \"rhs\": \"false\""
	  (reset_name id)
      )
       
    | MBranch (g, hl) -> (
      let all_outputs, outputs, inputs = branch_instr_vars m i in
      (* Format.eprintf "Mbranch %a@.vars: all_out: %a, out:%a, in:%a@.@." *)
      (* 	Machine_code.pp_instr i *)
      (* 	(fprintf_list ~sep:", " pp_var_string) (ISet.elements all_outputs) *)
      (* 	(fprintf_list ~sep:", " pp_var_string) (ISet.elements outputs) *)
      (* 	pp_emf_vars_decl *)
      (* 	(VSet.elements inputs) *)

      (* ; *)
      let inputs = VSet.filter (fun v -> not (ISet.mem v.var_id all_outputs)) inputs in
      (* Format.eprintf "Filtering in: %a@.@." *)
      (* 	pp_emf_vars_decl *)
      (* 	(VSet.elements inputs) *)

      (* ; *)
      fprintf fmt "\"kind\": \"branch\",@ ";
      fprintf fmt "\"guard\": %a,@ " pp_emf_cst_or_var g; (* it has to be a variable or a constant *)
      fprintf fmt "\"outputs\": [%a],@ " (fprintf_list ~sep:", " pp_var_string) (ISet.elements outputs);
      fprintf fmt "\"inputs\": [%a],@ " pp_emf_vars_decl
	(* (let guard_inputs = get_expr_vars g in
	   VSet.elements (VSet.diff inputs guard_inputs)) -- previous version to
	   remove guard's variable from inputs *)
	(VSet.elements inputs)
      ;
      fprintf fmt "@[<v 2>\"branches\": {@ @[<v 0>%a@]@]@ }"
	(fprintf_list ~sep:",@ "
	   (fun fmt (tag, instrs_tag) ->
	     let branch_all_lhs, _, branch_inputs = branch_block_vars m instrs_tag in
	     let branch_inputs = VSet.filter (fun v -> not (ISet.mem v.var_id branch_all_lhs)) branch_inputs in
	     fprintf fmt "@[<v 2>\"%a\": {@ " print_protect (fun fmt -> Format.pp_print_string fmt tag);
	     fprintf fmt "\"guard_value\": \"%a\",@ " pp_tag_id tag; 
	     fprintf fmt "\"inputs\": [%a],@ " pp_emf_vars_decl (VSet.elements branch_inputs); 
	     fprintf fmt "@[<v 2>\"instrs\": {@ ";
	     (pp_emf_instrs m) fmt instrs_tag;
	     fprintf fmt "@]@ }";
	     fprintf fmt "@]@ }"
	   )
	)
	hl
    )

    | MStep ([var], f, _) when is_arrow_fun m i -> (* Arrow case *) (
      fprintf fmt "\"kind\": \"arrow\",@ \"name\": \"%s\",@ \"lhs\": \"%a\",@ \"rhs\": \"%s\""
	f
	pp_var_name var
	(reset_name f)
    )

    | MStep (outputs, f, inputs) when not (is_imported_node f m) -> (
      let node_f = get_node_def f m in
      let is_stateful = List.mem_assoc f m.minstances in 
      fprintf fmt "\"kind\": \"%s\",@ \"name\": \"%a\",@ \"id\": \"%s\",@ "
	(if is_stateful then "statefulcall" else "statelesscall")
	print_protect (fun fmt -> pp_print_string fmt (node_f.node_id)) 
	f;
      fprintf fmt "\"lhs\": [@[%a@]],@ \"args\": [@[%a@]]"
	(fprintf_list ~sep:",@ " (fun fmt v -> fprintf fmt "\"%a\"" pp_var_name v)) outputs
	pp_emf_cst_or_var_list inputs;
      if is_stateful then
	fprintf fmt ",@ \"reset\": { \"name\": \"%s\", \"resetable\": \"%b\"}"
	  (reset_name f)
	  (is_resetable_fun i.lustre_eq)	  
      else fprintf fmt "@ "
    )

    | MStep(outputs, f, inputs ) -> (* This is an imported node *)
       EMF_library_calls.pp_call fmt m f outputs inputs
	 
    | MComment _ 
      -> Format.eprintf "unhandled comment in EMF@.@?"; assert false
  (* not  available for EMF output *)
  in
    fprintf fmt "@[ @[<v 2>\"%a\": {@ " get_instr_id i;
    fprintf fmt "%a" pp_content i;
    fprintf fmt "@]@]@ }"
and pp_emf_instrs m fmt instrs = fprintf_list ~sep:",@ " (pp_emf_instr m) fmt instrs

let pp_emf_annot cpt fmt (key, ee) =
  let _ =
    fprintf fmt "\"ann%i\": { @[<hov 0>\"key\": [%a],@ \"eexpr\": %a@] }"
      !cpt
      (fprintf_list ~sep:"," (fun fmt s -> fprintf fmt "\"%s\"" s)) key
      pp_emf_eexpr ee
  in
  incr cpt
  
let pp_emf_annots cpt fmt annots = fprintf_list ~sep:",@ " (pp_emf_annot cpt) fmt annots.annots
let pp_emf_annots_list cpt fmt annots_list = fprintf_list ~sep:",@ " (pp_emf_annots cpt) fmt annots_list
let pp_machine fmt m =
  let instrs = (*merge_branches*) m.mstep.step_instrs in
  try
    fprintf fmt "@[<v 2>\"%a\": {@ "
       print_protect (fun fmt -> pp_print_string fmt m.mname.node_id);
    fprintf fmt "\"kind\": %t,@ "
      (fun fmt -> if not ( snd (get_stateless_status m) )
	then fprintf fmt "\"stateful\""
	else fprintf fmt "\"stateless\"");
    fprintf fmt "\"inputs\": [%a],@ "
      pp_emf_vars_decl m.mstep.step_inputs;
    fprintf fmt "\"outputs\": [%a],@ "
      pp_emf_vars_decl m.mstep.step_outputs;
    fprintf fmt "\"locals\": [%a],@ "
      pp_emf_vars_decl m.mstep.step_locals;
    fprintf fmt "\"mems\": [%a],@ "
      pp_emf_vars_decl m.mmemory;
    
    fprintf fmt "\"original_name\": \"%s\",@ " m.mname.node_id;
    fprintf fmt "\"instrs\": {@[<v 0> %a@]@ },@ "
      (pp_emf_instrs m) instrs;
    fprintf fmt "\"annots\": {@[<v 0> %a@]@ }" (pp_emf_annots_list (ref 0)) m.mannot;
    fprintf fmt "@]@ }"
  with Unhandled msg -> (
    eprintf "[Error] @[<v 0>EMF backend@ Issues while translating node %s@ "
      m.mname.node_id;
    eprintf "%s@ " msg;
    eprintf "node skipped - no output generated@ @]@."
  )

(****************************************************)
(* Main function: iterates over node and print them *)
(****************************************************)
let pp_meta fmt basename =
  fprintf fmt "\"meta\": @[<v 0>{@ ";
  Utils.fprintf_list ~sep:",@ "
    (fun fmt (k, v) -> fprintf fmt "\"%s\": \"%s\"" k v)
    fmt
    ["compiler-name", (Filename.basename Sys.executable_name);
     "compiler-version", Version.number;
     "command", (String.concat " " (Array.to_list Sys.argv));
     "source_file", basename
    ]
  ;
  fprintf fmt "@ @]},@ "
    
let translate fmt basename prog machines =
   (* record_types prog; *)
  fprintf fmt "@[<v 0>{@ ";
  pp_meta fmt basename;
  fprintf fmt "\"nodes\": @[<v 0>{@ ";
  (* Previous alternative: mapping normalized lustre to EMF: 
     fprintf_list ~sep:",@ " pp_decl fmt prog; *)
  fprintf_list ~sep:",@ " pp_machine fmt (List.rev machines);
  fprintf fmt "}@]@ }";
  fprintf fmt "@]@ }"

(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
