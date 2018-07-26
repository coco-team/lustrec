let base_types = ["integer"; "character"; "bit"; "real"; "natural"; "positive"; "std_logic"; "std_logic_vector" ]
 
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
type vhdl_cst_val_t = 
    CstInt of int 
  | CstStdLogic of string
  | CstLiteral of string [@name "CST_LITERAL"]
[@@deriving show { with_path = false }, yojson {strict = false}];;

(*
let pp_cst_val fmt c =
  match c with
  | CstInt i -> Format.fprintf fmt "%i" i
  | CstStdLogic s -> if List.mem s std_logic_cst then Format.fprintf fmt "%s" s else assert false
  | CstLiteral s -> Format.fprintf fmt "%s" s
*)

type vhdl_type_t =
  | Base of string
  | Range of string option * int * int
  | Bit_vector of int * int
  | Array of { indexes: vhdl_name_t list [@default []]; const: vhdl_constraint_t option [@default None]; definition: vhdl_subtype_indication_t } [@name "ARRAY_TYPE_DEFINITION"]
  | Record of vhdl_element_declaration_t list [@name "RECORD_TYPE_DEFINITION"]
  | Enumerated of vhdl_name_t list [@name "ENUMERATION_TYPE_DEFINITION"]
  | Void
and vhdl_element_declaration_t =
  { 
    names : vhdl_name_t list; 
    definition: vhdl_subtype_indication_t;
  }
and vhdl_subtype_indication_t =
  {
    name : vhdl_name_t [@default NoName];
    functionName : vhdl_name_t [@default NoName];
    const: vhdl_constraint_t [@default NoConstraint];
  }
and vhdl_discrete_range_t =
  | SubDiscreteRange of vhdl_subtype_indication_t [@name "SUB_DISCRETE_RANGE"]
  | NamedRange of vhdl_name_t [@name "NAMED_RANGE"]
  | DirectedRange of { direction: string; from: vhdl_expr_t; _to: vhdl_expr_t } [@name "RANGE_WITH_DIRECTION"]
and vhdl_constraint_t =
  | RefConstraint of { ref_name: vhdl_name_t; }
  | RangeConstraint of { range: vhdl_discrete_range_t } [@name "RANGE_CONSTRAINT"]
  | IndexConstraint of { ranges: vhdl_discrete_range_t list; } [@name "INDEX_CONSTRAINT"]
  | ArrayConstraint of { ranges: vhdl_discrete_range_t list; sub: vhdl_constraint_t } [@name "ARRAY_CONSTRAINT"]
  | RecordConstraint
  | NoConstraint
and vhdl_definition_t =
  | Type of {name : vhdl_name_t ; definition: vhdl_type_t} [@name "TYPE_DECLARATION"]
  | Subtype of {name : vhdl_name_t ; typ : vhdl_subtype_indication_t} [@name "SUBTYPE_DECLARATION"]
and vhdl_expr_t =
  | Call of vhdl_name_t [@name "CALL"]
  | Cst of { value: vhdl_cst_val_t; unit_name: vhdl_name_t option [@default None]} [@name "CONSTANT_VALUE"]
  | Op of { id: string [@default ""]; args: vhdl_expr_t list [@default []]} [@name "EXPRESSION"]
  | IsNull [@name "IsNull"]
  | Time of { value: int; phy_unit: string [@default ""]}
  | Sig of { name: vhdl_name_t; att: vhdl_signal_attributes_t option [@default None]}
  | SuffixMod of { expr : vhdl_expr_t; selection : vhdl_suffix_selection_t }
  | Aggregate of { elems : vhdl_element_assoc_t list [@default []]} [@name "AGGREGATE"]
  | QualifiedExpression of { type_mark : vhdl_name_t; aggregate : vhdl_element_assoc_t list [@default []]; expression : vhdl_expr_t option [@default None]} [@name "QUALIFIED_EXPRESSION"]
  | Others [@name "OTHERS"]
and vhdl_name_t = (* Add something like TOKEN_NAME for specific keywords (open, all, ...) ? *)
  | Simple of string [@name "SIMPLE_NAME"]
  | Identifier of string [@name "IDENTIFIER"]
  | Selected of vhdl_name_t list [@name "SELECTED_NAME"]
  | Index of { id: vhdl_name_t; exprs: vhdl_expr_t list } [@name "INDEXED_NAME"]
  | Slice of { id: vhdl_name_t; range: vhdl_discrete_range_t } [@name "SLICE_NAME"]
  | Attribute of { id: vhdl_name_t; designator: vhdl_name_t; expr: vhdl_expr_t [@default IsNull]} [@name "ATTRIBUTE_NAME"]
  | Function of { id: vhdl_name_t; assoc_list: vhdl_assoc_element_t list } [@name "FUNCTION_CALL"]
  | NoName
and vhdl_assoc_element_t =
  {
    formal_name: vhdl_name_t option [@default None];
    formal_arg: vhdl_name_t option [@default None];
    actual_name: vhdl_name_t option [@default None];
    actual_designator: vhdl_name_t option [@default None];
    actual_expr: vhdl_expr_t option [@default None];
  }
and vhdl_element_assoc_t =
  {
    choices: vhdl_expr_t list [@default []];
    expr: vhdl_expr_t;
  }
and vhdl_array_attributes_t = 
  | AAttInt of { id: string; arg: int; } 
  | AAttAscending
and vhdl_signal_attributes_t = SigAtt of string
and vhdl_string_attributes_t = StringAtt of string
and vhdl_suffix_selection_t = Idx of int | SuffixRange of int * int
[@@deriving show { with_path = false }, yojson {strict = false}];;

(*
let rec pp_vhdl_type fmt t =
  match t with
  | Base s -> Format.fprintf fmt "%s" s 
  | Range(base, n, m) -> Format.fprintf fmt "%trange %i to %i" (fun fmt -> match base with Some s -> Format.fprintf fmt "%s " s | None -> ()) n m
  | Bit_vector (n,m) -> Format.fprintf fmt "bit_vector(%i downto %i)" n m
  | Array (n, m, base) -> Format.fprintf fmt "array (%i to %i) of %a" n m pp_vhdl_type base
  | Enumerated sl -> Format.fprintf fmt "(%a)" (Utils.fprintf_list ~sep:", " Format.pp_print_string) sl
  | Void -> Format.fprintf fmt ""
*)

(************************************************************************************)		   
(*            Attributes for types, arrays, signals and strings                     *)
(************************************************************************************)		   

type 'basetype vhdl_type_attributes_t =
  | TAttNoArg of { id: string }
  | TAttIntArg of { id: string; arg: int }
  | TAttValArg of { id: string; arg: 'basetype }
  | TAttStringArg of { id: string; arg: string }
[@@deriving show { with_path = false }, yojson {strict = false}];;

let typ_att_noarg = ["base"; "left"; "right"; "high"; "low"]
let typ_att_intarg = ["pos"; "val"; "succ"; "pred"; "leftof"; "rightof"]
let typ_att_valarg = ["image"]
let typ_att_stringarg = ["value"]
  
let array_att_intarg = ["left"; "right"; "high"; "low"; "range"; "reverse_range"; "length"]  

type vhdl_parameter_t =
  {
    names: vhdl_name_t list;
    mode: string list [@default []];
    typ: vhdl_subtype_indication_t;
    init_val: vhdl_cst_val_t option [@default None];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_subprogram_spec_t =
  {
    name: string [@default ""];
    subprogram_type: string [@default ""];
    typeMark: vhdl_name_t [@default NoName];
    parameters: vhdl_parameter_t list [@default []];
    isPure: bool [@default false];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

(************************************************************************************)		   
(*                        Expressions  / Statements                                 *)
(************************************************************************************)		   

let arith_funs = ["+";"-";"*";"/";"mod"; "rem";"abs";"**";"&"]
let bool_funs  = ["and"; "or"; "nand"; "nor"; "xor"; "not"]
let rel_funs   = ["<";">";"<=";">=";"/=";"=";"?=";"?/=";"?<";"?<=";"?>";"?>=";"??"]
let shift_funs = ["sll";"srl";"sla";"sra";"rol";"ror"]

type vhdl_waveform_element_t =
  {
    value: vhdl_expr_t option [@default None];
    delay: vhdl_expr_t option [@default None];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_sequential_stmt_t = 
  | VarAssign of { label: vhdl_name_t [@default NoName]; lhs: vhdl_name_t; rhs: vhdl_expr_t } [@name "VARIABLE_ASSIGNMENT_STATEMENT"]
  | SigSeqAssign of { label: vhdl_name_t [@default NoName]; lhs: vhdl_name_t; rhs: vhdl_waveform_element_t list} [@name "SIGNAL_ASSIGNMENT_STATEMENT"]
  | If of { label: vhdl_name_t [@default NoName]; if_cases: vhdl_if_case_t list;
    default: vhdl_sequential_stmt_t list [@default []]; } [@name "IF_STATEMENT"]
  | Case of { label: vhdl_name_t [@default NoName]; guard: vhdl_expr_t; branches: vhdl_case_item_t list } [@name "CASE_STATEMENT_TREE"]
  | Exit of { label: vhdl_name_t [@default NoName]; loop_label: string option [@default Some ""]; condition: vhdl_expr_t option [@default Some IsNull]} [@name "EXIT_STATEMENT"]
  | Assert of { label: vhdl_name_t [@default NoName]; cond: vhdl_expr_t; report: vhdl_expr_t [@default IsNull]; severity: vhdl_expr_t [@default IsNull]} [@name "ASSERTION_STATEMENT"]
  | ProcedureCall of { label: vhdl_name_t [@default NoName]; name: vhdl_name_t; assocs: vhdl_assoc_element_t list [@default []] } [@name "PROCEDURE_CALL_STATEMENT"]
  | Wait [@name "WAIT_STATEMENT"]
  | Null of { label: vhdl_name_t [@default NoName]} [@name "NULL_STATEMENT"]
  | Return of { label: vhdl_name_t option [@default None]; expr: vhdl_expr_t option [@default None]} [@name "RETURN_STATEMENT"]
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
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_port_mode_t = 
    InPort     [@name "in"]
  | OutPort    [@name "out"]
  | InoutPort  [@name "inout"]
  | BufferPort [@name "buffer"]
[@@deriving show { with_path = false }, yojson];;
	     
type vhdl_port_t =
  {
    names: vhdl_name_t list [@default []];
    mode: vhdl_port_mode_t [@default InPort];
    typ: vhdl_subtype_indication_t;
    expr: vhdl_expr_t [@default IsNull];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_declaration_t =
  | VarDecl of {
      names : vhdl_name_t list; 
      typ : vhdl_subtype_indication_t; 
      init_val : vhdl_expr_t [@default IsNull] 
    } [@name "VARIABLE_DECLARATION"]
  | CstDecl of { 
      names : vhdl_name_t list; 
      typ : vhdl_subtype_indication_t; 
      init_val : vhdl_expr_t
    } [@name "CONSTANT_DECLARATION"]
  | SigDecl of { 
      names : vhdl_name_t list; 
      typ : vhdl_subtype_indication_t; 
      init_val : vhdl_expr_t [@default IsNull]
    } [@name "SIGNAL_DECLARATION"]
  | ComponentDecl of {
      name: vhdl_name_t [@default NoName];
      generics: vhdl_port_t list [@default []];
      ports: vhdl_port_t list [@default []];
    } [@name "COMPONENT_DECLARATION"]
  | Subprogram of {
      spec: vhdl_subprogram_spec_t; 
      decl_part: vhdl_declaration_t list [@default []]; 
      stmts: vhdl_sequential_stmt_t list [@default []]
    } [@name "SUBPROGRAM_BODY"]
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_load_t = 
    Library of vhdl_name_t list [@name "LIBRARY_CLAUSE"] [@default []]
  | Use of vhdl_name_t list [@name "USE_CLAUSE"] [@default []]
[@@deriving show { with_path = false }, yojson];;

type vhdl_declarative_item_t =
  {
    use_clause: vhdl_load_t option [@default None];
    declaration: vhdl_declaration_t option [@default None];
    definition: vhdl_definition_t option [@default None];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_signal_condition_t =
  {                            
    expr: vhdl_waveform_element_t list [@default []];              (* when expression *)
    cond: vhdl_expr_t option [@default None];  (* optional else case expression. 
                                             If None, could be a latch  *)
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_signal_selection_t =
  {
    expr : vhdl_waveform_element_t list [@default []];
    when_sel: vhdl_expr_t list [@default []];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_conditional_signal_t =
  {
    postponed: bool [@default false];
    label: vhdl_name_t [@default NoName];
    lhs: vhdl_name_t;        (* assigned signal = target*)
    rhs: vhdl_signal_condition_t list;                   (* expression *)
    delay: vhdl_expr_t [@default IsNull];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_process_t =
  { 
    id: vhdl_name_t [@default NoName];
    declarations: vhdl_declarative_item_t list [@key "PROCESS_DECLARATIVE_PART"] [@default []];
    active_sigs: vhdl_name_t list [@default []];
    body: vhdl_sequential_stmt_t list [@key "PROCESS_STATEMENT_PART"] [@default []]
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_selected_signal_t = 
  { 
    postponed: bool [@default false];
    label: vhdl_name_t [@default NoName];
    lhs: vhdl_name_t;      (* assigned signal = target *)
    sel: vhdl_expr_t;
    branches: vhdl_signal_selection_t list [@default []];
    delay: vhdl_expr_t option [@default None];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_component_instantiation_t =
  {
    name: vhdl_name_t;
    inst_unit: vhdl_name_t;
    inst_unit_type : string [@default ""];
    archi_name: vhdl_name_t option [@default None];
    generic_map: vhdl_assoc_element_t list [@default []];
    port_map: vhdl_assoc_element_t list [@default []];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_concurrent_stmt_t =
  | SigAssign of vhdl_conditional_signal_t [@name "CONDITIONAL_SIGNAL_ASSIGNMENT"]
  | Process of vhdl_process_t [@name "PROCESS_STATEMENT"]
  | SelectedSig of vhdl_selected_signal_t [@name "SELECTED_SIGNAL_ASSIGNMENT"]
  | ComponentInst of vhdl_component_instantiation_t [@name "COMPONENT_INSTANTIATION_STATEMENT"]
[@@deriving show { with_path = false }, yojson {strict = false}];;
  (*
type vhdl_statement_t =
  
  (* | DeclarationStmt of declaration_stmt_t *)
  | ConcurrentStmt of vhdl_concurrent_stmt_t
  | SequentialStmt of vhdl_sequential_stmt_t
   *)
		     
(************************************************************************************)		   
(*                     Entities                                                     *)
(************************************************************************************)

type vhdl_entity_t =
  {
    name: vhdl_name_t [@default NoName];
    generics: vhdl_port_t list [@default []];
    ports: vhdl_port_t list [@default []];
    declaration: vhdl_declarative_item_t list [@key "ENTITY_DECLARATIVE_PART"] [@default []];
    stmts: vhdl_concurrent_stmt_t list [@key "ENTITY_STATEMENT_PART"] [@default []]; 
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

(************************************************************************************)		   
(*                    Packages / Library loading                                    *)
(************************************************************************************)		   
				
(* Optional. Describes shared definitions *)
type vhdl_package_t =
  {
    name: vhdl_name_t [@default NoName];
    shared_defs: vhdl_definition_t list [@default []];
    shared_decls: vhdl_declaration_t list [@default []];
    shared_uses: vhdl_load_t list [@default []];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

(************************************************************************************)		   
(*                        Architecture / VHDL Design                                *)
(************************************************************************************)		   
				       
type vhdl_architecture_t =
  {
    name: vhdl_name_t [@default NoName];
    entity: vhdl_name_t [@default NoName];
    declarations: vhdl_declarative_item_t list [@key "ARCHITECTURE_DECLARATIVE_PART"] [@default []];
    body: vhdl_concurrent_stmt_t list [@key "ARCHITECTURE_STATEMENT_PART"] [@default []]; 
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;
    
(* TODO. Configuration is optional *)
type vhdl_configuration_t = unit
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_library_unit_t = (* TODO: PACKAGE_BODY *)
    Package of vhdl_package_t [@name "PACKAGE_DECLARATION"]
  | Entities of vhdl_entity_t [@name "ENTITY_DECLARATION"]
  | Architecture of vhdl_architecture_t [@name "ARCHITECTURE_BODY"]
  | Configuration of vhdl_configuration_t [@name "CONFIGURATION_DECLARATION"]
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_design_unit_t =
  {
    contexts: vhdl_load_t list [@default []];
    library: vhdl_library_unit_t;
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_design_file_t =
  {
    design_units: vhdl_design_unit_t list [@default []];
  }
[@@deriving show { with_path = false }, yojson {strict = false}];;

type vhdl_file_t = 
  {
    design_file: vhdl_design_file_t [@default {design_units=[]}] [@key "DESIGN_FILE"];
  }
[@@deriving show { with_path = false }, yojson];;
