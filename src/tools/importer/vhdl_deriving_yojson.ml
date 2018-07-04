let base_types = ["integer"; "character"; "bit"; "real"; "natural"; "positive"; "std_logic"; "std_logic_vector" ]

type vhdl_type_t =
  | Base of string
  | Range of string option * int * int [@name "RANGE_WITH_DIRECTION"]
  | Bit_vector of int * int
  | Array of int * int * vhdl_type_t
  | Enumerated of string list
  | Void
[@@deriving yojson];;
  
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
let literal_base = ["B"; "O"; "X"; "UB"; "UO"; "UX"; "SB"; "SO"; "SX"; "D"] (* Prefix of CstLiteral *)

(* TODO: do we need more constructors ? *)
type cst_val_t = 
    CstInt of int 
  | CstStdLogic of string
  | CstLiteral of string [@name "CST_LITERAL"]
[@@deriving yojson {strict = false}];;

type vhdl_subtype_indication_t =
  {
    name : string;
    definition: vhdl_type_t option [@default Some (Void)];
  }
[@@deriving yojson {strict = false}];;

(* TODO ? Shall we merge definition / declaration  *)
type vhdl_definition_t =
  | Type of {name : string ; definition: vhdl_type_t} [@name "TYPE_DECLARATION"]
  | Subtype of {name : string ; typ : vhdl_subtype_indication_t} [@name "SUBTYPE_DECLARATION"]
[@@deriving yojson {strict = false}];;
					
type vhdl_declaration_t =
  | VarDecl of { names : string list; typ : vhdl_subtype_indication_t; init_val : cst_val_t option [@default Some (CstInt (0))] } [@name "VARIABLE_DECLARATION"]
  | CstDecl of { names : string list; typ : vhdl_subtype_indication_t; init_val : cst_val_t  } [@name "CONSTANT_DECLARATION"]
  | SigDecl of { names : string list; typ : vhdl_subtype_indication_t; init_val : cst_val_t option [@default Some (CstInt (0))] } [@name "SIGNAL_DECLARATION"]
[@@deriving yojson {strict = false}];;

(************************************************************************************)		   
(*            Attributes for types, arrays, signals and strings                     *)
(************************************************************************************)		   

type 'basetype vhdl_type_attributes_t =
  | TAttNoArg of { id: string }
  | TAttIntArg of { id: string; arg: int }
  | TAttValArg of { id: string; arg: 'basetype }
  | TAttStringArg of { id: string; arg: string }
[@@deriving yojson {strict = false}];;

let typ_att_noarg = ["base"; "left"; "right"; "high"; "low"]
let typ_att_intarg = ["pos"; "val"; "succ"; "pred"; "leftof"; "rightof"]
let typ_att_valarg = ["image"]
let typ_att_stringarg = ["value"]
  
type vhdl_array_attributes_t = AAttInt of { id: string; arg: int; } | AAttAscending
[@@deriving yojson {strict = false}];;

let array_att_intarg = ["left"; "right"; "high"; "low"; "range"; "reverse_range"; "length"]  

type vhdl_signal_attributes_t = SigAtt of string
[@@deriving yojson {strict = false}];;

type vhdl_string_attributes_t = StringAtt of string
[@@deriving yojson {strict = false}];;

(************************************************************************************)		   
(*                        Expressions  / Statements                                 *)
(************************************************************************************)		   
type suffix_selection_t = Idx of int | Range of int * int
[@@deriving yojson {strict = false}];;

type vhdl_expr_t =
  | Call of vhdl_name_t [@name "CALL"]
  | Cst of cst_val_t [@name "CONSTANT_VALUE"]
  | Op of { id: string [@default ""]; args: vhdl_expr_t list [@default []]} [@name "EXPRESSION"]
  | IsNull [@name "IsNull"]
  | Time of { value: int; phy_unit: string [@default ""]}
  | Sig of { name: string; att: vhdl_signal_attributes_t option }
  | SuffixMod of { expr : vhdl_expr_t; selection : suffix_selection_t }
[@@deriving yojson {strict = false}]
and					     
vhdl_name_t =
  | Simple of string [@name "SIMPLE_NAME"]
  | Selected of vhdl_name_t list [@name "SELECTED_NAME"]
  | Index of { id: vhdl_name_t; exprs: vhdl_expr_t list } [@name "INDEXED_NAME"]
  | Slice of { id: vhdl_name_t; range: vhdl_type_t } [@name "SLICE_NAME"]
  | Attribute of { id: vhdl_name_t; designator: vhdl_name_t; expr: vhdl_expr_t [@default IsNull]} [@name "ATTRIBUTE_NAME"]
  | Function of { id: vhdl_name_t; assoc_list: vhdl_assoc_element_t list } [@name "FUNCTION_CALL"]
  | NoName
[@@deriving yojson {strict = false}]
and vhdl_assoc_element_t =
  {
    formal_name: vhdl_name_t option [@default Some NoName];
    formal_arg: vhdl_name_t option [@default Some NoName];
    actual_name: vhdl_name_t option [@default Some NoName];
    actual_designator: vhdl_name_t option [@default Some NoName];
    actual_expr: vhdl_expr_t option [@default Some IsNull];
  }
[@@deriving yojson {strict = false}];;

let arith_funs = ["+";"-";"*";"/";"mod"; "rem";"abs";"**";"&"]
let bool_funs  = ["and"; "or"; "nand"; "nor"; "xor"; "not"]
let rel_funs   = ["<";">";"<=";">=";"/=";"=";"?=";"?/=";"?<";"?<=";"?>";"?>=";"??"]
let shift_funs = ["sll";"srl";"sla";"sra";"rol";"ror"]

type vhdl_sequential_stmt_t = 
  | VarAssign of { lhs: vhdl_name_t; rhs: vhdl_expr_t }
  | SigSeqAssign of { label: string [@default ""]; lhs: vhdl_name_t; rhs: vhdl_expr_t list} [@name "SIGNAL_ASSIGNMENT_STATEMENT"]
  | If of { label: string [@default ""]; if_cases: vhdl_if_case_t list;
    default: vhdl_sequential_stmt_t list [@default []]; } [@name "IF_STATEMENT"]
  | Case of { guard: vhdl_expr_t; branches: vhdl_case_item_t list } [@name "CASE_STATEMENT_TREE"]
  | Exit of { label: string [@default ""]; loop_label: string option [@default Some ""]; condition: vhdl_expr_t option [@default Some IsNull]} [@name "EXIT_STATEMENT"]
  | Assert of { label: string [@default ""]; cond: vhdl_expr_t; report: vhdl_expr_t [@default IsNull]; severity: vhdl_expr_t [@default IsNull]} [@name "ASSERTION_STATEMENT"]
  | Wait [@name "WAIT_STATEMENT"]
  | Null of { label: string [@default ""]} [@name "NULL_STATEMENT"]
and vhdl_if_case_t = 
  {
    if_cond: vhdl_expr_t;
    if_block: vhdl_sequential_stmt_t list;
  }
and vhdl_case_item_t = 
  {
    when_cond: vhdl_expr_t list;
    when_stmt: vhdl_sequential_stmt_t list;
  }
[@@deriving yojson {strict = false}];;
				    
type signal_condition_t =
  {                            
    expr: vhdl_expr_t list;              (* when expression *)
    cond: vhdl_expr_t [@default IsNull];  (* optional else case expression. 
                                             If None, could be a latch  *)
  }
[@@deriving yojson {strict = false}];;

type signal_selection_t =
  {
    expr : vhdl_expr_t;
    when_sel: vhdl_expr_t list [@default []];
  }
[@@deriving yojson {strict = false}];;

type conditional_signal_t =
  {
    postponed: bool [@default false];
    label: string option [@default Some ""];
    lhs: vhdl_name_t;        (* assigned signal = target*)
    rhs: signal_condition_t list;                   (* expression *)
    cond: vhdl_expr_t [@default IsNull];
    delay: vhdl_expr_t [@default IsNull];
  }
[@@deriving yojson {strict = false}];;

type process_t =
  { 
    id: string option [@default Some ""];
    declarations: vhdl_declaration_t list option [@key "PROCESS_DECLARATIVE_PART"] [@default Some []];
    active_sigs: vhdl_name_t list [@default []];
    body: vhdl_sequential_stmt_t list [@key "PROCESS_STATEMENT_PART"] [@default []]
  }
[@@deriving yojson {strict = false}];;

type selected_signal_t = 
  { 
    postponed: bool [@default false];
    label: string option [@default Some ""];
    lhs: vhdl_name_t;      (* assigned signal = target *)
    sel: vhdl_expr_t;  
    branches: signal_selection_t list [@default []];
    delay: vhdl_expr_t option;
  }
[@@deriving yojson {strict = false}];;
			   
type vhdl_concurrent_stmt_t =
  | SigAssign of conditional_signal_t [@name "CONDITIONAL_SIGNAL_ASSIGNMENT"]
  | Process of process_t [@name "PROCESS_STATEMENT"]
  | SelectedSig of selected_signal_t [@name "SELECTED_SIGNAL_ASSIGNMENT"]
[@@deriving yojson {strict = false}];;
  (*
type vhdl_statement_t =
  
  (* | DeclarationStmt of declaration_stmt_t *)
  | ConcurrentStmt of vhdl_concurrent_stmt_t
  | SequentialStmt of vhdl_sequential_stmt_t
   *)
		     
(************************************************************************************)		   
(*                     Entities                                                     *)
(************************************************************************************)		   
			     
(* TODO? Seems to appear optionally in entities *)
type vhdl_generic_t = unit
[@@deriving yojson {strict = false}];;
			      
type vhdl_port_kind_t = 
    InPort     [@name "in"]
  | OutPort    [@name "out"]
  | InoutPort  [@name "inout"]
  | BufferPort [@name "buffer"]
[@@deriving yojson];;
	     
type vhdl_port_t =
  {
    names: string list [@default []];
    kind: vhdl_port_kind_t;
    typ : string;
(*    typ: vhdl_type_t; *)
  }
[@@deriving yojson {strict = false}];;

type vhdl_entity_t =
  {
    name: string [@default ""];
    generics: vhdl_generic_t list option [@key "GENERIC_CLAUSE"] [@default Some []];
    ports: vhdl_port_t list [@key "PORT_CLAUSE"] [@default []];
  }
[@@deriving yojson {strict = false}];;

(************************************************************************************)		   
(*                    Packages / Library loading                                    *)
(************************************************************************************)		   
				
(* Optional. Describes shared definitions *)
type vhdl_package_t =
  {
    name: string [@default ""];
    shared_defs: vhdl_definition_t list [@default []];
  }
[@@deriving yojson {strict = false}];;

type vhdl_load_t = 
    Library of string list [@name "LIBRARY_CLAUSE"] [@default ""]
  | Use of string list [@name "USE_CLAUSE"] [@default []]
[@@deriving yojson];;

(************************************************************************************)		   
(*                        Architecture / VHDL Design                                *)
(************************************************************************************)		   
				       
type vhdl_architecture_t =
  {
    name: string [@default ""];
    entity: string [@default ""];
    declarations: vhdl_declaration_t list option [@key "ARCHITECTURE_DECLARATIVE_PART"] [@default Some []];
    body: vhdl_concurrent_stmt_t list option [@key "ARCHITECTURE_STATEMENT_PART"] [@default Some []]; 
  }
[@@deriving yojson {strict = false}];;
    
(* TODO. Configuration is optional *)
type vhdl_configuration_t = unit
[@@deriving yojson {strict = false}];;

type vhdl_design_t =
  {
    packages: vhdl_package_t list [@key "PACKAGE_DECLARATION"] [@default []];
    libraries: vhdl_load_t list option [@key "CONTEXT_CLAUSE"] [@default Some []];
    entities: vhdl_entity_t list [@key "ENTITY_DECLARATION"] [@default []];
    architectures: vhdl_architecture_t list [@key "ARCHITECTURE_BODY"] [@default []];
    configuration: vhdl_configuration_t option [@key "CONFIGURATION_DECLARATION"] [@default Some ()];
  }
[@@deriving yojson {strict = false}];;

type vhdl_design_file_t =
  {
    design_unit: vhdl_design_t list [@key "DESIGN_UNIT"] [@default []];
  }
[@@deriving yojson {strict = false}];;

type vhdl_file_t = 
  {
    design_file: vhdl_design_file_t [@key "DESIGN_FILE"];
  }
[@@deriving yojson];;
