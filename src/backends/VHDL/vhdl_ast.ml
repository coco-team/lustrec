(* source: Synario VHDL Reference Manual - March 1997 *)

(************************************************************************************)		   
(*                       Types                                                      *)
(************************************************************************************)		   
let base_types = ["integer"; "character"; "bit"; "real"; "natural"; "positive"; "std_logic"; "std_logic_vector" ]

type vhdl_type_t =
  | Base of string
  | Range of string option * int * int
  | Bit_vector of int * int			  
  | Array of int * int * vhdl_type_t 
  | Enumerated of string list
  
let rec pp_vhdl_type fmt t =
  match t with
  | Base s -> Format.fprintf fmt "%s" s 
  | Bit_vector (n,m) -> Format.fprintf fmt "bit_vector(%i downto %i)" n m
  | Range(base, n, m) -> Format.fprintf fmt "%trange %i to %i" (fun fmt -> match base with Some s -> Format.fprintf fmt "%s " s | None -> ()) n m
  | Array (n, m, base) -> Format.fprintf fmt "array (%i to %i) of %a" n m pp_vhdl_type base
  | Enumerated sl -> Format.fprintf fmt "(%a)" (Utils.fprintf_list ~sep:", " Format.pp_print_string) sl



(************************************************************************************)		   
(*                     Constants                                                    *)
(************************************************************************************)		   

(* Std_logic values :
    'U': uninitialized. This signal hasn't been set yet.
    'X': unknown. Impossible to determine this value/result.
    '0': logic 0
    '1': logic 1
    'Z': High Impedance
    'W': Weak signal, can't tell if it should be 0 or 1.
    'L': Weak signal that should probably go to 0
    'H': Weak signal that should probably go to 1
    '-': Don't care. *)			       
let std_logic_cst = ["U"; "X"; "0"; "1"; "Z"; "W"; "L"; "H"; "-" ]

(* TODO: do we need more constructors ? *)
type cst_val_t = CstInt of int | CstStdLogic of string | CstBV of string * string

let pp_cst_val fmt c =
  match c with
  | CstInt i -> Format.fprintf fmt "%i" i
  | CstStdLogic s -> if List.mem s std_logic_cst then Format.fprintf fmt "%s" s else assert false
  | CstBV (pref,suff) -> Format.fprintf fmt "%s\"%s\"" pref suff

(************************************************************************************)		   
(*                     Declarations                                                 *)
(************************************************************************************)		   


(* TODO ? Shall we merge definition / declaration ? Do they appear at the same
place or at different ones ? *)
type vhdl_definition_t =
  | Type of {name : string ; definition: vhdl_type_t}
  | Subtype of {name : string ; definition: vhdl_type_t}
					
let pp_vhdl_definition fmt def =
  match def with
  | Type s -> Format.fprintf fmt "type %s is %a;" s.name pp_vhdl_type s.definition
  | Subtype s -> Format.fprintf fmt "subtype %s is %a;" s.name pp_vhdl_type s.definition
		      
type vhdl_declaration_t =
  | VarDecl of { name : string; typ : vhdl_type_t; init_val : cst_val_t option }
  | CstDecl of { name : string; typ : vhdl_type_t; init_val : cst_val_t  }
  | SigDecl of { name : string; typ : vhdl_type_t; init_val : cst_val_t option }

let pp_vhdl_declaration fmt decl =
  match decl with
  | VarDecl v -> Format.fprintf
		   fmt
		   "variable %s : %a%t;"
		   v.name
		   pp_vhdl_type v.typ
		   (fun fmt -> match v.init_val with Some initv -> Format.fprintf fmt " := %a" pp_cst_val initv | _ -> ())
  | CstDecl v -> Format.fprintf
		   fmt
		   "constant %s : %a := %a;"
		   v.name
		   pp_vhdl_type v.typ
		   pp_cst_val v.init_val
  | SigDecl v -> Format.fprintf
		   fmt
		   "signal %s : %a%t;"
		   v.name
		   pp_vhdl_type v.typ
		   (fun fmt -> match v.init_val with Some initv -> Format.fprintf fmt " := %a" pp_cst_val initv | _ -> ())


(************************************************************************************)		   
(*            Attributes for types, arrays, signals and strings                     *)
(************************************************************************************)		   

type 'basetype vhdl_type_attributes_t =
  | TAttNoArg of { id: string }
  | TAttIntArg of { id: string; arg: int }
  | TAttValArg of { id: string; arg: 'basetype }
  | TAttStringArg of { id: string; arg: string }

let typ_att_noarg = ["base"; "left"; "right"; "high"; "low"]
let typ_att_intarg = ["pos"; "val"; "succ"; "pred"; "leftof"; "rightof"]
let typ_att_valarg = ["image"]
let typ_att_stringarg = ["value"]
  
let pp_type_attribute pp_val fmt tatt =
  match tatt with
  | TAttNoArg a -> Format.fprintf fmt "'%s" a.id
  | TAttIntArg a -> Format.fprintf fmt "'%s(%i)" a.id a.arg
  | TAttValArg a -> Format.fprintf fmt "'%s(%a)" a.id pp_val a.arg
  | TAttStringArg a -> Format.fprintf fmt "'%s(%s)" a.id a.arg

type vhdl_array_attributes_t = AAttInt of { id: string; arg: int; } | AAttAscending
let pp_array_attribute fmt aatt =
  match aatt with
  | AAttInt a -> Format.fprintf fmt "'%s(%i)" a.id a.arg
  | AAttAscending -> Format.fprintf fmt "'ascending"
let array_att_intarg = ["left"; "right"; "high"; "low"; "range"; "reverse_range"; "length"]  

type vhdl_signal_attributes_t = SigAtt of string
let pp_signal_attribute fmt sa = match sa with
  | SigAtt s -> Format.fprintf fmt "'%s" s
let signal_att = [ "event"; "stable"; "last_value" ]

type vhdl_string_attributes_t = StringAtt of string
let pp_string_attribute fmt sa = match sa with
  | StringAtt s -> Format.fprintf fmt "'%s" s
let signal_att = [ "simple_name"; "path_name"; "instance_name" ]

(************************************************************************************)		   
(*                        Expressions  / Statements                                 *)
(************************************************************************************)		   

			      
(* TODO: call to functions? procedures? component instanciations ? *)

type suffix_selection_t = Idx of int | Range of int * int
let pp_suffix_selection fmt sel =
  match sel with
  | Idx n -> Format.fprintf fmt "(%i)" n
  | Range(n,m) -> Format.fprintf fmt "(%i downto %i)" n m
							
type vhdl_expr_t =
  | Cst of cst_val_t 
  | Var of string (* a signal or a variable *)
  | Sig of { name: string; att: vhdl_signal_attributes_t option }
  | SuffixMod of { expr : vhdl_expr_t; selection : suffix_selection_t }
  | Op of { id: string; args: vhdl_expr_t list } 
					     
let rec pp_vhdl_expr fmt e =
  match e with
  | Cst c ->  pp_cst_val fmt c
  | Var s -> Format.fprintf fmt "%s" s
  | Sig s -> Format.fprintf
	       fmt
	       "%s%t"
	       s.name
	       (fun fmt -> match s.att with None -> () | Some att -> pp_signal_attribute fmt att)
  | SuffixMod s ->
     Format.fprintf fmt "%a %a"
		    pp_vhdl_expr s.expr
		    pp_suffix_selection s.selection
  | Op op -> (
    match op.args with
    | [] -> assert false
    | [ e1; e2] -> Format.fprintf fmt "@[<hov 3>%a %s %a@]" pp_vhdl_expr e1 op.id pp_vhdl_expr e2
    | _ -> assert false (* all ops are binary up to now *)
    (* | _ -> Format.fprintf fmt "@[<hov 3>%s (%a)@]" op.id (Utils.fprintf_list ~sep:",@ " pp_vhdl_expr) op.args *)
  )

(* Available operators in the standard library. There are some restrictions on
types. See reference doc. *)
let arith_funs = ["+";"-";"*";"/";"mod"; "rem";"abs";"**"]
let bool_funs  = ["and"; "or"; "nand"; "nor"; "xor"; "not"]
let rel_funs   = ["<";">";"<=";">=";"/=";"="]

			  
type vhdl_if_case_t = {
    if_cond: vhdl_expr_t;
    if_block: vhdl_sequential_stmt_t list;
  }	   
 and vhdl_sequential_stmt_t = 
   | VarAssign of { lhs: string; rhs: vhdl_expr_t }
   | SigSeqAssign of { lhs: string; rhs: vhdl_expr_t }
   | If of { if_cases: vhdl_if_case_t list;
	    default: (vhdl_sequential_stmt_t list) option; }
   | Case of { guard: vhdl_expr_t; branches: vhdl_case_item_t list }
and vhdl_case_item_t = {
    when_cond: vhdl_expr_t;
    when_stmt: vhdl_sequential_stmt_t;
  }

					    
		 
let rec pp_vhdl_sequential_stmt fmt stmt =
  match stmt with
  | VarAssign va -> Format.fprintf fmt "%s := %a;" va.lhs pp_vhdl_expr va.rhs
  | SigSeqAssign va -> Format.fprintf fmt "%s <= %a;" va.lhs pp_vhdl_expr va.rhs
  | If ifva -> (
     List.iteri (fun idx ifcase ->
		 if idx = 0 then
		   Format.fprintf fmt "@[<v 3>if"
		 else
		   Format.fprintf fmt "@ @[<v 3>elsif";
		 Format.fprintf fmt " %a then@ %a@]"
				pp_vhdl_expr ifcase.if_cond
				pp_vhdl_sequential_stmts ifcase.if_block
		) ifva.if_cases;
     let _ =
       match ifva.default with
       | None -> ()
       | Some bl -> Format.fprintf fmt "@ @[<v 3>else@ %a@]" pp_vhdl_sequential_stmts bl
     in
     Format.fprintf fmt "@ end if;"
  )
  | Case caseva -> (
    Format.fprintf fmt "@[<v 3>case %a is@ %a@]@ end case;"
		   pp_vhdl_expr caseva.guard
		   (Utils.fprintf_list ~sep:"@ " pp_vhdl_case) caseva.branches
  )

     
and pp_vhdl_sequential_stmts fmt l  = Utils.fprintf_list ~sep:"@ " pp_vhdl_sequential_stmt fmt l
and pp_vhdl_case fmt case =
  Format.fprintf fmt "when %a => %a"
		 pp_vhdl_expr case.when_cond
		 pp_vhdl_sequential_stmt case.when_stmt
						  
type signal_condition_t =
  {                            
    expr: vhdl_expr_t;              (* when expression *)
    else_case: vhdl_expr_t option;  (* optional else case expression. 
                                             If None, could be a latch  *)
  }

type signal_selection_t =
  {
    sel_lhs: string;
    expr : vhdl_expr_t;
    when_sel: vhdl_expr_t option;
  }

type conditional_signal_t =
  {
      lhs: string;                        (* assigned signal *)
      rhs: vhdl_expr_t;                   (* expression *)
      cond: signal_condition_t option     (* conditional signal statement *)
  }

type process_t =
  { id: string option; active_sigs: string list; body: vhdl_sequential_stmt_t list }

type selected_signal_t = { sel: vhdl_expr_t;  branches: signal_selection_t list }
			   
type vhdl_concurrent_stmt_t =
  | SigAssign of conditional_signal_t 
  | Process of process_t 
  | SelectedSig of selected_signal_t
  (*
type vhdl_statement_t =
  
  (* | DeclarationStmt of declaration_stmt_t *)
  | ConcurrentStmt of vhdl_concurrent_stmt_t
  | SequentialStmt of vhdl_sequential_stmt_t
   *)
		     
let pp_vhdl_concurrent_stmt fmt stmt =
  let pp_sig_cond fmt va = 
    Format.fprintf
      fmt
      "%s <= %a%t;"
      va.lhs
      pp_vhdl_expr va.rhs
      (fun fmt -> match va.cond with
		  | None -> ()
		  | Some cond ->
		     Format.fprintf
		       fmt
		       " when %a%t"
		       pp_vhdl_expr cond.expr
		       (fun fmt -> match cond.else_case with
				   | None -> ()
				   | Some else_case ->
				      Format.fprintf
					fmt
					" else %a"
					pp_vhdl_expr else_case
		       )
      )
  in
  let pp_process fmt p =
    Format.fprintf
      fmt
      "@[<v 0>%tprocess %a@ @[<v 3>begin@ %a@]@ end process;@]"
      (fun fmt -> match p.id with Some id -> Format.fprintf fmt "%s: " id| None -> ())
      (fun fmt asigs ->
       if asigs <> [] then
	 Format.fprintf
	   fmt
	   "(@[<hov 0>%a)@]"
	   (Utils.fprintf_list ~sep:",@ " Format.pp_print_string) asigs)
      p.active_sigs
      (Utils.fprintf_list ~sep:"@ " pp_vhdl_sequential_stmt) p.body
  in
  let pp_sig_sel fmt va =
    Format.fprintf fmt "@[<v 3>with %a select@ %a;@]"
		   pp_vhdl_expr va.sel
		   (Utils.fprintf_list
		      ~sep:"@ "
		      (fun fmt b ->
		       Format.fprintf
			 fmt
			 "%s <= %a when %t"
			 b.sel_lhs
			 pp_vhdl_expr b.expr
			 (fun fmt -> match b.when_sel with
				     | None -> Format.fprintf fmt "others"
				     | Some w -> pp_vhdl_expr fmt w
			 ))
		   ) va.branches  in
  match stmt with
  | SigAssign va -> pp_sig_cond fmt va       
  | Process p -> pp_process fmt p
  | SelectedSig va -> pp_sig_sel fmt va
 

  
       



(************************************************************************************)		   
(*                     Entities                                                     *)
(************************************************************************************)		   
			     
(* TODO? Seems to appear optionally in entities *)
type vhdl_generic_t = unit
let pp_vhdl_generic fmt g = ()

			      
type vhdl_port_kind_t = InPort | OutPort | InoutPort | BufferPort
let pp_vhdl_port_kind fmt p =
  match p with
  | InPort -> Format.fprintf fmt "in"
  | OutPort -> Format.fprintf fmt "in"
  | InoutPort -> Format.fprintf fmt "inout"
  | BufferPort -> Format.fprintf fmt "buffer"

		     
type vhdl_port_t =
  {
    name: string;
    kind: vhdl_port_kind_t;
    typ: vhdl_type_t;
  }

let pp_vhdl_port fmt p =
  Format.fprintf fmt "%s : %a %a"
		 p.name
		 pp_vhdl_port_kind p.kind
		 pp_vhdl_type p.typ
	 
			     
type vhdl_entity_t =
  {
    name: string;
    generics: vhdl_generic_t list;
    ports: vhdl_port_t list;
  }
let pp_vhdl_entity fmt e =
  Format.fprintf
    fmt
    "@[<v 3>entity %s is@ %t%t@]@ end %s;@ "
    e.name
    (fun fmt -> List.iter (fun g -> Format.fprintf fmt "generic %a;@ " pp_vhdl_generic g) e.generics)
    (fun fmt ->
     if e.ports = [] then () else
       Format.fprintf fmt "port (@[<hov 0>%a@]);" (Utils.fprintf_list ~sep:",@ " pp_vhdl_port) e.ports)
    e.name




(************************************************************************************)		   
(*                    Packages / Library loading                                    *)
(************************************************************************************)		   

				
				
(* Optional. Describes shared definitions *)
type vhdl_package_t =
  {
    name: string;
    shared_defs: vhdl_definition_t list;
  }

let pp_vhdl_package fmt p =
  Format.fprintf
    fmt
    "@[<v 3>package %s is@ %a@]@ end %s;@ "
    p.name
    (Utils.fprintf_list  ~sep:"@ " pp_vhdl_definition) p.shared_defs
    p.name

type vhdl_load_t = Library of string | Use of string list
let pp_vhdl_load fmt l =
  match l with
  | Library s -> Format.fprintf fmt "library %s;@ " s
  | Use sl -> Format.fprintf fmt "use %a;@ " (Utils.fprintf_list ~sep:"." Format.pp_print_string) sl


(************************************************************************************)		   
(*                        Architecture / VHDL Design                                *)
(************************************************************************************)		   
				       
				       
type vhdl_architecture_t =
  {
    name: string;
    entity: string;
    declarations: vhdl_declaration_t list;
    body: vhdl_concurrent_stmt_t list;
  }
    
let pp_vhdl_architecture fmt a =
  Format.fprintf
    fmt
    "@[<v 3>architecture %s of %s is@ %a@]@ @[<v 3>begin@ %a@]@ end %s;"
    a.name
    a.entity
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_declaration) a.declarations
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_concurrent_stmt) a.body
    a.name
    

(* TODO. Configuraiton is optional *)
type vhdl_configuration_t = unit
let pp_vhdl_configuration fmt c = ()



type vhdl_design_t =
  {
    packages: vhdl_package_t list;
    libraries: vhdl_load_t list;
    entities: vhdl_entity_t list;
    architectures: vhdl_architecture_t list;
    configuration: vhdl_configuration_t option;
  }

let pp_vhdl_design fmt d =
  Format.fprintf
    fmt
    "@[<v 0>%a%t%a%t%a%t%a%t@]"
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_package) d.packages
    (fun fmt -> if d.packages <> [] then Format.fprintf fmt "@ ")
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_load) d.libraries
    (fun fmt -> if d.libraries <> [] then Format.fprintf fmt "@ ")
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_entity) d.entities
    (fun fmt -> if d.entities <> [] then Format.fprintf fmt "@ ")
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_architecture) d.architectures
    (fun fmt -> if d.architectures <> [] then Format.fprintf fmt "@ ")
