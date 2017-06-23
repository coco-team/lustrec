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

let rec pp_val vars fmt v =
  match v.value_desc with
  | Cst c -> Printers.pp_const fmt c
  | LocalVar v
  | StateVar v ->
     let id = v.var_id in
     if List.mem id vars then
       Format.fprintf fmt "u%i" (get_idx id vars)
     else
       assert false (* impossible to find element id in var list *)
  | Fun (n, vl) -> pp_fun vars n fmt vl
  | _ -> assert false (* not available in EMF backend *)
and pp_fun vars id fmt vl =
  eprintf "print %s with %i args@.@?" id (List.length vl);
  match id, vl with
    | "+", [v1;v2] -> fprintf fmt "(%a + %a)" (pp_val vars) v1 (pp_val vars) v2
    | "uminus", [v] -> fprintf fmt "(- %a)" (pp_val vars) v
    | "-", [v1;v2] -> fprintf fmt "(%a - %a)" (pp_val vars) v1 (pp_val vars) v2
    | "*",[v1;v2] -> fprintf fmt "(%a * %a)" (pp_val vars) v1 (pp_val vars) v2
    | "/", [v1;v2] -> fprintf fmt "(%a / %a)" (pp_val vars) v1 (pp_val vars) v2
    | "mod", [v1;v2] -> fprintf fmt "mod (%a, %a)" (pp_val vars) v1 (pp_val vars) v2
    | "&&", [v1;v2] -> fprintf fmt "(%a & %a)" (pp_val vars) v1 (pp_val vars) v2
    | "||", [v1; v2] -> fprintf fmt "(%a | %a)" (pp_val vars) v1 (pp_val vars) v2
    | "xor", [v1; v2] -> fprintf fmt "xor (%a, %a)" (pp_val vars) v1 (pp_val vars) v2
    | "impl", [v1; v2] -> fprintf fmt "((~%a) | %a)" (pp_val vars) v1 (pp_val vars) v2
    | "<", [v1; v2] -> fprintf fmt "(%a < %a)" (pp_val vars) v1 (pp_val vars) v2
    | "<=", [v1; v2] -> fprintf fmt "(%a <= %a)" (pp_val vars) v1 (pp_val vars) v2
    | ">", [v1; v2] -> fprintf fmt "(%a > %a)" (pp_val vars) v1 (pp_val vars) v2
    | ">=", [v1; v2] -> fprintf fmt "(%a >= %a)" (pp_val vars) v1 (pp_val vars) v2
    | "!=", [v1; v2] -> fprintf fmt "(%a != %a)" (pp_val vars) v1 (pp_val vars) v2
    | "=", [v1; v2] -> fprintf fmt "(%a = %a)" (pp_val vars) v1 (pp_val vars) v2
    | "not", [v] -> fprintf fmt "(~%a)" (pp_val vars) v
    | _ -> fprintf fmt "%s (%a)" id  (Utils.fprintf_list ~sep:", " (pp_val vars)) vl 

     
(* detect whether the instruction i represents a STEP, ie an arrow with true -> false *)
let is_step_fun m i =
  match Corelang.get_instr_desc i with
  | MStep ([var], i, vl)  -> (
    let name = (Machine_code.get_node_def i m).node_id in
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

     
let rec pp_instr m vars fmt i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (var,v) 
  | MStateAssign (var,v) -> fprintf fmt "y = %a" (pp_val vars) v
  | MStep _ when is_step_fun m i  -> fprintf fmt "STEP" 
  | MBranch (g,[(tag1,case1);(tag2,case2)])     ->
     let then_case, else_case =
       if tag1 = Corelang.tag_true then
	 case1, case2
       else
	 case2, case1
     in
     fprintf fmt "if %a; %a; else %a; end"
       (pp_val vars) g
       (pp_instrs m vars) then_case
       (pp_instrs m vars) else_case
  | MStep _ (* no function calls handled yet *)
  | MBranch _ (* EMF backend only accept true/false ite *)
  | MReset _           
  | MNoReset _
  | MComment _ -> assert false (* not  available for EMF output *)
and pp_instrs m vars fmt il =
  fprintf fmt "@[<v 2>%a@]" (Utils.fprintf_list ~sep:"@," (pp_instr m vars)) il


let rec get_instr_var i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (var,_) 
  | MStateAssign (var,_) 
  | MStep ([var], _, _)  -> var 
  | MBranch (_,[(tag1,case1);(tag2,case2)])     ->
     get_instrs_var case1 (* assuming case1 and case2 define the same variable *)
  | MStep _ (* only single output for function call *)
  | MBranch _ (* EMF backend only accept true/false ite *)
  | MReset _           
  | MNoReset _
  | MComment _ -> assert false (* not  available for EMF output *)
and get_instrs_var il =
  match il with
  | i::_ -> get_instr_var i (* looking for the first instr *)
  | _ -> assert false

  
let rec  get_val_vars v =
  match v.value_desc with
  | Cst c -> Utils.ISet.empty
  | LocalVar v
  | StateVar v -> Utils.ISet.singleton v.var_id
  | Fun (n, vl) -> List.fold_left (fun res v -> Utils.ISet.union (get_val_vars v) res) Utils.ISet.empty vl
  | _ -> assert false (* not available in EMF backend *)

let rec get_instr_vars i =
  match Corelang.get_instr_desc i with
  | MLocalAssign (_,v)  
  | MStateAssign (_,v) -> get_val_vars v
  | MStep ([_], _, vl)  -> List.fold_left (fun res v -> Utils.ISet.union res (get_val_vars v)) Utils.ISet.empty vl 
  | MBranch (c,[(_,case1);(_,case2)])     ->
     Utils.ISet.union
       (get_val_vars c)
       (
	 Utils.ISet.union
	   (get_instrs_vars case1)
	   (get_instrs_vars case2)
       )
  | MStep _ (* only single output for function call *)
  | MBranch _ (* EMF backend only accept true/false ite *)
  | MReset _           
  | MNoReset _
  | MComment _ -> failwith "Error in compiling some constructs into EMF. Have you considered -node foo -inline options ?" (* not  available for EMF output *)
and get_instrs_vars il =
  List.fold_left (fun res i -> Utils.ISet.union res (get_instr_vars i))
    Utils.ISet.empty
    il


let pp_original_lustre_expression m fmt i =
  match Corelang.get_instr_desc i with
  | MLocalAssign _ | MStateAssign _ 
  | MBranch _
    -> ( match i.lustre_eq with None -> () | Some e -> Printers.pp_node_eq fmt e) 
  | MStep _ when is_step_fun m i -> () (* we print nothing, this is a STEP *)
  | MStep _ -> (match i.lustre_eq with None -> () | Some eq -> Printers.pp_node_eq fmt eq)
  | _ -> ()
    
let pp_instr_main m fmt i =
  (* first, we extract the expression and associated variables *)
  let var = get_instr_var i in
  let vars = Utils.ISet.elements (get_instr_vars i) in	
  fprintf fmt "\"%s\": @[<v 2>{ \"expr\": \"%a\",@ \"vars\": [%a] @ \"original_lustre_expr\": [%a]@]}"
    var.var_id
    (pp_instr m vars) i
    (fprintf_list ~sep:", " pp_var_string) vars
    (pp_original_lustre_expression m) i

    
     
let pp_machine fmt m =
  try
    fprintf fmt "@[<v 2>\"%s\": {@ \"inputs\": [%a],@ \"outputs\": [%a],@ "
      m.mname.node_id
      pp_node_args m.mstep.step_inputs
      pp_node_args m.mstep.step_outputs;
    fprintf fmt "\"exprs\": {@[<v 1> %a@]@ }"
      (fprintf_list ~sep:",@ " (pp_instr_main m)) m.mstep.step_instrs;
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
  (* fprintf_list ~sep:",@ " pp_decl fmt prog; *)
  fprintf_list ~sep:",@ " pp_machine fmt machines;
  fprintf fmt "@ @]}";
  fprintf fmt "@ @]}"

(* Local Variables: *)
(* compile-command: "make -C ../.." *)
(* End: *)
