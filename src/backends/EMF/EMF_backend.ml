(* EMF backend *)
(* This backup is initially motivated by the need to express Spacer computed
   invariants as Matlab Simulink executable evidences.

   Therefore the input language is more restricted. We do not expect fancy
   datastructure, complex function calls, etc.

   In case of error, use -node foo -inline to eliminate function calls that are
   not seriously handled yet.
   

   In terms of algorithm, the process was initially based on printing normalized
   code. We now rely on machine code printing. The old code is preserved for
   reference.
*)

open LustreSpec
open Machine_code
open Format
open Utils

exception Unhandled of string
    

(* Basic printing functions *)
    
let pp_var_string fmt v = fprintf fmt "\"%s\"" v
let pp_var_name fmt v = fprintf fmt "\"%a\"" Printers.pp_var_name v
let pp_node_args = fprintf_list ~sep:", " pp_var_name

    
(* Matlab starting counting from 1.
   simple function to extract the element id in the list. Starts from 1. *)
let rec get_idx x l =
  match l with
  | hd::tl -> if hd = x then 1 else 1+(get_idx x tl)
  | [] -> assert false

(**********************************************)
(* Old stuff: printing normalized code as EMF *)     
(**********************************************)

(*
let pp_expr vars fmt expr =
  let rec pp_expr fmt expr =
    match expr.expr_desc with
    | Expr_const c -> Printers.pp_const fmt c
    | Expr_ident id ->
       if List.mem id vars then
	 Format.fprintf fmt "u%i" (get_idx id vars)
       else
	 assert false (* impossible to find element id in var list *)
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "if %a; y=(%a); else y=(%a); end" pp_expr c pp_expr t pp_expr e
    | Expr_arrow (e1, e2) ->(
      match e1.expr_desc, e2.expr_desc with
      | Expr_const c1, Expr_const c2 -> if c1 = Corelang.const_of_bool true && c2 = Corelang.const_of_bool false then fprintf fmt "STEP" else assert false (* only handle true -> false *)
      | _ -> assert false (* only handle true -> false *)
    )
    | Expr_fby (e1, e2) -> assert false (* not covered yet *)
    | Expr_pre e -> fprintf fmt "UNITDELAY"
    | Expr_when (e, id, l) -> assert false (* clocked based expressions are not handled yet *)
    | Expr_merge (id, hl) -> assert false (* clocked based expressions are not handled yet *)
    | Expr_appl (id, e, r) -> pp_app fmt id e r

  and pp_tuple fmt el =
    fprintf_list ~sep:"," pp_expr fmt el

  and pp_app fmt id e r =
    match r with
    | None -> pp_call fmt id e
    | Some c -> assert false (* clocked based expressions are not handled yet *)

  and pp_call fmt id e =
    match id, e.expr_desc with
    | "+", Expr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
    | "uminus", _ -> fprintf fmt "(- %a)" pp_expr e
    | "-", Expr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
    | "*", Expr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
    | "/", Expr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_expr e1 pp_expr e2
    | "mod", Expr_tuple([e1;e2]) -> fprintf fmt "mod (%a, %a)" pp_expr e1 pp_expr e2
    | "&&", Expr_tuple([e1;e2]) -> fprintf fmt "(%a & %a)" pp_expr e1 pp_expr e2
    | "||", Expr_tuple([e1;e2]) -> fprintf fmt "(%a | %a)" pp_expr e1 pp_expr e2
    | "xor", Expr_tuple([e1;e2]) -> fprintf fmt "xor (%a, %a)" pp_expr e1 pp_expr e2
    | "impl", Expr_tuple([e1;e2]) -> fprintf fmt "((~%a) | %a)" pp_expr e1 pp_expr e2
    | "<", Expr_tuple([e1;e2]) -> fprintf fmt "(%a < %a)" pp_expr e1 pp_expr e2
    | "<=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a <= %a)" pp_expr e1 pp_expr e2
    | ">", Expr_tuple([e1;e2]) -> fprintf fmt "(%a > %a)" pp_expr e1 pp_expr e2
    | ">=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a >= %a)" pp_expr e1 pp_expr e2
    | "!=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a ~= %a)" pp_expr e1 pp_expr e2
    | "=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a == %a)" pp_expr e1 pp_expr e2
    | "not", _ -> fprintf fmt "(~%a)" pp_expr e
    | _, Expr_tuple _ -> fprintf fmt "%s %a" id pp_expr e
    | _ -> fprintf fmt "%s (%a)" id pp_expr e

  in
  pp_expr fmt expr

let pp_stmt fmt stmt =
  match stmt with
  | Eq eq -> (
    match eq.eq_lhs with
      [var] -> (
     (* first, we extract the expression and associated variables *)
	let vars = Utils.ISet.elements (Corelang.get_expr_vars eq.eq_rhs) in

	fprintf fmt "\"%s\": @[<v 2>{ \"expr\": \"%a\",@ \"vars\": [%a] @]}"
	  var
	  (pp_expr vars) eq.eq_rhs (* todo_pp_expr expr *)
	  (fprintf_list ~sep:", " pp_var_string) vars
      )
    | _ -> assert false (* should not happen for input of EMF backend (cocospec generated nodes *)
  )
  | _ -> assert false (* should not happen with EMF backend *)

let pp_node fmt nd =
  fprintf fmt "@[<v 2>\"%s\": {@ \"inputs\": [%a],@ \"outputs\": [%a],@ "
    nd.node_id
    pp_node_args nd.node_inputs
    pp_node_args nd.node_outputs;
  fprintf fmt "\"exprs\": {@[<v 1> %a@]@ }"
    (fprintf_list ~sep:",@ " pp_stmt ) nd.node_stmts;
  fprintf fmt "@]@ }"

let pp_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "%a@ " pp_node nd
  | ImportedNode _
  | Const _
  | Open _
  | TypeDef _ -> eprintf "should not happen in EMF backend"
*)


(**********************************************)
(*   Printing machine code as EMF             *)
(**********************************************)

(* detect whether the instruction i represents an ARROW, ie an arrow with true
   -> false *)
let is_arrow_fun m i =
  match Corelang.get_instr_desc i with
  | MStep ([var], i, vl)  -> (
    let name = try (Machine_code.get_node_def i m).node_id with Not_found -> Format.eprintf "Impossible to find node %s@.@?" i; raise Not_found in
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
  )
  | _ -> false

let pp_original_lustre_expression m fmt i =
  match Corelang.get_instr_desc i with
  | MLocalAssign _ | MStateAssign _ 
  | MBranch _
    -> ( match i.lustre_eq with None -> () | Some e -> Printers.pp_node_eq fmt e) 
  | MStep _ when is_arrow_fun m i -> () (* we print nothing, this is a STEP *)
  | MStep _ -> (match i.lustre_eq with None -> () | Some eq -> Printers.pp_node_eq fmt eq)
  | _ -> ()


(* Print machine code values as matlab expressions. Variable identifiers are
   replaced by uX where X is the index of the variables in the list vars of input
   variables. *)
let rec pp_matlab_val vars fmt v =
  match v.value_desc with
  | Cst c -> Printers.pp_const fmt c
  | LocalVar v
  | StateVar v ->
     let id = v.var_id in
     if List.mem id vars then
       Format.fprintf fmt "u%i" (get_idx id vars)
     else
       let _ = Format.eprintf "Error: looking for var %s in %a@.@?" id (Utils.fprintf_list ~sep:"," Format.pp_print_string) vars in assert false (* impossible to find element id in var list *)
  | Fun (n, vl) -> pp_fun vars n fmt vl
  | _ -> assert false (* not available in EMF backend *)
and pp_fun vars id fmt vl =
  (* eprintf "print %s with %i args@.@?" id (List.length vl);*)
  match id, vl with
    | "+", [v1;v2] -> fprintf fmt "(%a + %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "uminus", [v] -> fprintf fmt "(- %a)" (pp_matlab_val vars) v
    | "-", [v1;v2] -> fprintf fmt "(%a - %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "*",[v1;v2] -> fprintf fmt "(%a * %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "/", [v1;v2] -> fprintf fmt "(%a / %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "mod", [v1;v2] -> fprintf fmt "mod (%a, %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "&&", [v1;v2] -> fprintf fmt "(%a & %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "||", [v1; v2] -> fprintf fmt "(%a | %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "xor", [v1; v2] -> fprintf fmt "xor (%a, %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "impl", [v1; v2] -> fprintf fmt "((~%a) | %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "<", [v1; v2] -> fprintf fmt "(%a < %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "<=", [v1; v2] -> fprintf fmt "(%a <= %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | ">", [v1; v2] -> fprintf fmt "(%a > %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | ">=", [v1; v2] -> fprintf fmt "(%a >= %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "!=", [v1; v2] -> fprintf fmt "(%a != %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "=", [v1; v2] -> fprintf fmt "(%a = %a)" (pp_matlab_val vars) v1 (pp_matlab_val vars) v2
    | "not", [v] -> fprintf fmt "(~%a)" (pp_matlab_val vars) v
    | _ -> fprintf fmt "%s (%a)" id  (Utils.fprintf_list ~sep:", " (pp_matlab_val vars)) vl 

     

(* pp_basic_instr prints regular instruction. These do not contain MStep which
   should have been already filtered out. Another restriction which is supposed
   to be enforced is that branching statement contain a single instruction (in
   practice it has to be an assign) *)
let pp_matlab_basic_instr m vars fmt i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (var,v) 
  | MStateAssign (var,v) -> fprintf fmt "y = %a" (pp_matlab_val vars) v
  | MReset _           
    -> Format.eprintf "unhandled reset in EMF@.@?"; assert false
  | MNoReset _
    -> Format.eprintf "unhandled noreset in EMF@.@?"; assert false
  | MBranch _ (* branching instructions already handled *)
    -> Format.eprintf "unhandled branch statement in EMF (should have been filtered out before)@.@?";
      assert false
  | MStep _ (* function calls already handled, including STEP *)
    -> Format.eprintf "unhandled function call in EMF (should have been filtered out before)@.@?";
      assert false
  | MComment _ 
    -> Format.eprintf "unhandled comment in EMF@.@?"; assert false
      (* not  available for EMF output *)



let rec get_instr_lhs_var i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (var,_) 
  | MStateAssign (var,_) 
  | MStep ([var], _, _)  ->
     (* The only MStep instructions that filtered here
	should be arrows, ie. single var *)
     var
  | MBranch (_,(_,case1)::_)     ->
     get_instrs_var case1 (* assuming all cases define the same variables *)
  | MStep (f,name,a) -> Format.eprintf "step %s@.@?" name; assert false (* no other MStep here *)
  | MBranch _ -> assert false (* branch instruction should admit at least one case *)
  | MReset _           
  | MNoReset _
  | MComment _ -> assert false (* not  available for EMF output *)
and get_instrs_var il =
  match il with
  | i::_ -> get_instr_lhs_var i (* looking for the first instr *)
  | _ -> assert false

  
let rec  get_val_vars v =
  match v.value_desc with
  | Cst c -> Utils.ISet.empty
  | LocalVar v
  | StateVar v -> Utils.ISet.singleton v.var_id
  | Fun (n, vl) -> List.fold_left (fun res v -> Utils.ISet.union (get_val_vars v) res) Utils.ISet.empty vl
  | _ -> assert false (* not available in EMF backend *)

let rec get_instr_rhs_vars i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (_,v)  
  | MStateAssign (_,v) -> get_val_vars v
  | MStep (_, _, vl)  -> List.fold_left (fun res v -> Utils.ISet.union res (get_val_vars v)) Utils.ISet.empty vl 
  | MBranch (c,[(_,[case1]);(_,[case2])])     ->
     Utils.ISet.union
       (get_val_vars c)
       (
	 Utils.ISet.union
	   (get_instr_rhs_vars case1)
	   (get_instr_rhs_vars case2)
       )
  | MBranch (g, branches) ->
     List.fold_left
       (fun accu (_, il) -> Utils.ISet.union accu (get_instrs_vars il))
       (get_val_vars g)
       branches
  | MReset id           
  | MNoReset id -> Utils.ISet.singleton id
  | MComment _ -> Utils.ISet.empty
and get_instrs_vars il =
  List.fold_left (fun res i -> Utils.ISet.union res (get_instr_rhs_vars i))
    Utils.ISet.empty
    il


     
let rec pp_emf_instr m fmt i =
  (* Either it is a Step function non arrow, then we have a dedicated treatment,
     or it has to be a single variable assigment *)
  let arguments_vars = Utils.ISet.elements (get_instr_rhs_vars i) in	
  
  match Corelang.get_instr_desc i with
  (* Regular node call either a statuful node or a functional one *)
  | MStep (outputs, f, inputs) when not (is_arrow_fun m i) -> (
    fprintf fmt "\"CALL\": @[<v 2>{ \"node\": \"%s\",@ \"inputs\": [%a],@ \"vars\": [%a]@ \"lhs\": [%a],@ \"original_lustre_expr\": [%a]@]}"
      ((Machine_code.get_node_def f m).node_id) (* Node name *)
      (Utils.fprintf_list ~sep:", " (fun fmt _val -> fprintf fmt "\"%a\"" (pp_matlab_val arguments_vars) _val)) inputs                  (* inputs *)
      (fprintf_list ~sep:", " pp_var_string) arguments_vars
      (fprintf_list ~sep:", " (fun fmt v -> pp_var_string fmt v.var_id)) outputs  (* outputs *)
      (pp_original_lustre_expression m) i         (* original lustre expr *)
  )
  | MStep _ -> (* Arrow case *) (
    let var = get_instr_lhs_var i in
    fprintf fmt "\"STEP\": @[<v 2>{ \"lhs\": \"%s\",@ \"vars\": [%a] @ \"original_lustre_expr\": [%a]@]}"
      var.var_id
      (fprintf_list ~sep:", " pp_var_string) arguments_vars
      (pp_original_lustre_expression m) i
  )
  | MBranch (g,[(tag1,[case1]);(tag2,[case2])]) when tag1 = Corelang.tag_true || tag2 = Corelang.tag_true  ->
     (* Thanks to normalization with join_guards = false, branches shall contain
	a single expression *)
     let var = get_instr_lhs_var i in
     let then_case, else_case =
       if tag1 = Corelang.tag_true then
	 case1, case2
       else
	 case2, case1
     in
     fprintf fmt "\"ITE\": @[<v 2>{ \"lhs\": \"%s\",@ \"guard\": \"%a\",@ \"then_expr\": \"%a\",@ \"else_expr\": \"%a\",@ \"vars\": [%a],@ \"original_lustre_expr\": [%a]@]}"
       var.var_id
       (pp_matlab_val arguments_vars) g
       (pp_matlab_basic_instr m arguments_vars) then_case
       (pp_matlab_basic_instr m arguments_vars) else_case
       (fprintf_list ~sep:", " pp_var_string) arguments_vars
       (pp_original_lustre_expression m) i

  | MBranch (g, [single_tag, single_branch]) ->
     (* First case: it corresponds to a clocked expression: a MBranch with a
	single case. It shall become a subsystem with an enable port that depends on g = single_tag *)
     (* Thanks to normalization with join_guards = false, branches shall contain
	a single expression TODO REMOVE COMMENT THIS IS NOT TRUE *)
     let var = get_instr_lhs_var i in
     fprintf fmt "\"ENABLEDSUB\": @[<v 2>{ \"lhs\": \"%s\",@ \"enable_cond\": \"%a = %s\",@ \"subsystem\": {%a },@ \"vars\": [%a],@ \"original_lustre_expr\": [%a]@]}"
       var.var_id
       (pp_matlab_val arguments_vars) g
       single_tag
       (fprintf_list ~sep:",@ " (pp_emf_instr m)) single_branch
       (fprintf_list ~sep:", " pp_var_string) arguments_vars
       (pp_original_lustre_expression m) i
       
  | MBranch (g, hl) ->
     (* Thanks to normalization with join_guards = false, branches shall contain
	a single expression *)
     fprintf fmt "\"BRANCH\": @[<v 2>{ \"guard\": \"%a\",@ \"branches\": [@[<v 0>%a@]],@ \"vars\": [%a],@ \"original_lustre_expr\": [%a]@]}"
       (pp_matlab_val arguments_vars) g
       (fprintf_list ~sep:",@ "
	  (fun fmt (tag, (is_tag: instr_t list)) ->
	    fprintf fmt "\"%s\": [%a]"
	      tag
	      (fprintf_list ~sep:",@ " (fun fmt i_tag -> match Corelang.get_instr_desc i_tag with
		  | MLocalAssign (var,v) 
		  | MStateAssign (var,v) ->
		     fprintf fmt "{lhs= \"%s\", rhs= \"%a\"]" var.var_id (pp_matlab_val arguments_vars) v
		  | _ -> Format.eprintf "unhandled instr: %a@." Machine_code.pp_instr i_tag; assert false
	      )) is_tag
	  )) hl 
       (fprintf_list ~sep:", " pp_var_string) arguments_vars
       (pp_original_lustre_expression m) i
       
       
       
  | _ ->
     (* Other expressions, including "pre" *)
     ( 
       (* first, we extract the expression and associated variables *)
       let var = get_instr_lhs_var i in
       fprintf fmt "\"EXPR\": @[<v 2>{ \"lhs\": \"%s\",@ \"expr\": \"%a\",@ \"vars\": [%a] @ \"original_lustre_expr\": [%a]@]}"
	 var.var_id
	 (fun fmt i -> match Corelang.get_instr_desc i with
	 | MStep _ -> fprintf fmt "STEP"
	 | _ -> pp_matlab_basic_instr m arguments_vars fmt i) i
	 (fprintf_list ~sep:", " pp_var_string) arguments_vars
	 (pp_original_lustre_expression m) i
     )
    
(* A (normalized) node becomes a JSON struct
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
   "foo": {inputs:  [{name: "in1", type: "int"}, 
                     {name: "in2", type: "int"}, 
                     {name: "__reset", type: "reset"}
                    ],
           outputs: [{name: "out1", type: "int"}, {name: "out2", type: "int"}],
           locals:  [{name: "x", type: "int"}],
           instrs:  [
                    { def_x: { lhs: ["x"], 
                               rhs: {type: "statefulcall", name: "bar", 
                                     args: [in1, in2], reset: [ni4_reset] } 
                             }
                    }
                    { def_out1: { lhs: "out1", rhs: "x" } },
                    { def_out2: { lhs: "out2", rhs: "in2" }}
                    ]
           }

Basically we have three different definitions
1. classical assign of a variable to another one:
   { def_out1: { lhs: "out1", rhs: "x" } },
2. call to a stateless function, typically an operator
   { def_x: { lhs: ["x"], 
     rhs: {type: "statelesscall", name: "bar", args: [in1, in2]} 
   }
  or in the operator version 
   { def_x: { lhs: ["x"], 
     rhs: {type: "operator", name: "+", args: [in1, in2]} 
   }

  In Simulink this should introduce a subsystem in the first case or a 
  regular block in the second with card(lhs) outputs and card{args} inputs.

3. call to a stateful node. It is similar to the stateless above, 
   with the addition of the reset argument
    { def_x: { lhs: ["x"], 
               rhs: {type: "statefulcall", name: "bar", 
                     args: [in1, in2], reset: [ni4_reset] } 
             }
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
   { "merge_XX": { type: "branch", guard: "var_guard", 
                   inputs:   ["varx", "vary"],
                   outputs:  ["vark", "varz"],
                   branches: ["tag1": [liste_of_definitions (1-4)], ...] 
                 }
   }

  In Simulink, this should become one IF block to produce enable ports "var_guard == tag1", "var_guard == tag2", .... as well as one action block per branch: each of these action block shall  
*)     
let pp_machine fmt m =
  try
    fprintf fmt "@[<v 2>\"%s\": {@ \"inputs\": [%a],@ \"outputs\": [%a],@ "
      m.mname.node_id
      pp_node_args m.mstep.step_inputs
      pp_node_args m.mstep.step_outputs;
    fprintf fmt "\"exprs\": {@[<v 1> %a@]@ }"
      (fprintf_list ~sep:",@ " (pp_emf_instr m)) m.mstep.step_instrs;
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
  fprintf fmt "@[<v 0>{@ ";
  pp_meta fmt basename;
  fprintf fmt "\"nodes\": @[<v 0>{@ ";
  (* Previous alternative: mapping normalized lustre to EMF: 
     fprintf_list ~sep:",@ " pp_decl fmt prog; *)
  fprintf_list ~sep:",@ " pp_machine fmt machines;
  fprintf fmt "@ @]}";
  fprintf fmt "@ @]}"

(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
