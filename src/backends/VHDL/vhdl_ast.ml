(* source: Synario VHDL Reference Manual - March 1997 *)

(* TODO ? *)
type vhdl_type_t =
  | Integer | Natural | Positive
  | Real
  | Range of string option * int * int
  | Byte
  | Bit_vector of int * int
  | Enumerated of string list
  
let pp_vhdl_type fmt t =
  match t with
  | Integer -> Format.fprintf fmt "integer"
  | Natural -> Format.fprintf fmt "natural"
  | Positive -> Format.fprintf fmt "positive"
  | Real -> Format.fprintf fmt "real"
  | Range(base, n, m) -> Format.fprintf fmt "%trange %i to %i" (fun fmt -> match base with Some s -> Format.fprintf fmt "%s " s | None -> ()) n m
  | Byte -> Format.fprintf fmt "byte"
  | Bit_vector (n,m) -> Format.fprintf fmt "bit_vector(%i downto %i)" n m
  | Enumerated sl -> Format.fprintf fmt "(%a)" (Utils.fprintf_list ~sep:", " Format.pp_print_string) sl

type vhdl_definition_t =
  | Type of {name : string ; definition: vhdl_type_t}
  | Subtype of {name : string ; definition: vhdl_type_t}
					
let pp_vhdl_definition fmt def =
  match def with
  | Type s -> Format.fprintf fmt "type %s is %a;" s.name pp_vhdl_type s.definition
  | Subtype s -> Format.fprintf fmt "subtype %s is %a;" s.name pp_vhdl_type s.definition

(* Optional. Describes shared definitions *)
type vhdl_package_t =
  {
    name: string;
    shared_defs: vhdl_definition_t list;
  }

let pp_vhdl_package fmt p =
  Format.fprintf
    fmt
    "@[<v 3>package %s is@ %a@ end %s;@]@ "
    p.name
    (Utils.fprintf_list  ~sep:"@ " pp_vhdl_definition) p.shared_defs
    p.name

type vhdl_load_t = Library of string | Use of string list
let pp_vhdl_load fmt l =
  match l with
  | Library s -> Format.fprintf fmt "library %s;" s
  | Use sl -> Format.fprintf fmt "use %a;s" (Utils.fprintf_list ~sep:"." Format.pp_print_string) sl


(* TODO *)
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
    "@[<v 3>entity %s is@ %t%t@]@ end %s;"
    e.name
    (fun fmt -> List.iter (fun g -> Format.fprintf fmt "generic %a;@ " pp_vhdl_generic g) e.generics)
    (fun fmt ->
     if e.ports = [] then () else
       Format.fprintf fmt "port (@[<hov 0>%a@]);" (Utils.fprintf_list ~sep:"@ " pp_vhdl_port) e.ports)
    e.name


(* TODO *)

type cst_val_t = CstInt of int
let pp_cst_val fmt c =
  match c with
  | CstInt i -> Format.fprintf fmt "%i" i
			       
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
		   (fun fmt -> match v.init_val with Some initv -> Format.fprintf fmt " := %a" pp_cst_val initv)
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
		   (fun fmt -> match v.init_val with Some initv -> Format.fprintf fmt " := %a" pp_cst_val initv)

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
let pp_signal_attribute fmt sa = match sa with
  | StringAtt s -> Format.fprintf fmt "'%s" s
let signal_att = [ "simple_name"; "path_name"; "instance_name" ]

				      
(* TODO *)  
type vhdl_expr_t =
  | Binop of { op: string; args: vhdl_expr_t list }
					     
let pp_vhdl_expr fmt e = ()

let arith_funs = ["+";"-";"*";"/";"mod"; "rem";"abs";"**"]
let bool_funs  = ["and"; "or"; "nand"; "nor"; "xor"; "not"]
let rel_funs   = ["<";">";"<=";">=";"/=";"="]
  
			   


type vhdl_sequential_stmt_t = unit 
(*  | VarAssign of { lhs: string; rhs: vhdl_expr_t }
  | Case of { guard: vhdl_expr_t; branches: { case: }
	    | Case of { guard: vhdl_expr_t; branches 
 *)
type vhdl_concurrent_stmt_t =
  | SigAssign of { lhs: string; rhs: vhdl_expr_t }
  | Process of { active_sigs: string list; body: vhdl_sequential_stmt_t list }
  
type vhdl_statement_t =
  
  (* | DeclarationStmt of declaration_stmt_t *)
  | ConcurrentStmt of vhdl_concurrent_stmt_t
  | SequentialStmt of vhdl_sequential_stmt_t
			

let rec pp_vhdl_statement fmt stmt = ()
(*  match stmt with
  | VarAssign va -> Format.fprintf fmt "%s := %a;" va.lhs pp_vhdl_expr va.rhs
  | SigAssign va -> Format.fprintf fmt "%s <= %a;" va.lhs pp_vhdl_expr va.rhs
  | Process p ->
     Format.fprintf
       fmt
       "@[<v 0>process %a@ @[<v 3>begin@ %a@]@ end process;@]"
       (fun fmt asigs ->
	if asigs <> [] then
	  Format.fprintf
	    fmt
	    "(@[<hov 0>%a)@]"
	    (Utils.fprintf_list ~sep:",@ " Format.pp_print_string) asigs)
       p.active_sigs
       (Utils.fprintf_list ~sep:"@ " pp_vhdl_statement) p.body
 *)     

type vhdl_architecture_t =
  {
    name: string;
    entity: string;
    declarations: vhdl_declaration_t list;
    body: vhdl_statement_t list;
  }
    
let pp_vhdl_architecture fmt a =
  Format.fprintf
    fmt
    "@[<v 3>architecture %s of %s is@ %a@]@ @[<v 3>begin@ %a@]end %s"
    a.name
    a.entity
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_declaration) a.declarations
    (Utils.fprintf_list ~sep:"@ " pp_vhdl_statement) a.body
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
