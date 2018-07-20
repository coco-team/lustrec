open Vhdl_ast_deriving

let _ = fun (_ : vhdl_cst_val_t)  -> () 
let _ = fun (_ : vhdl_type_t)  -> () 
let _ = fun (_ : vhdl_subtype_indication_t)  -> () 
let _ = fun (_ : vhdl_discrete_range_t)  -> () 
let _ = fun (_ : vhdl_constraint_t)  -> () 
let _ = fun (_ : vhdl_definition_t)  -> () 
let _ = fun (_ : vhdl_expr_t)  -> () 
let _ = fun (_ : vhdl_name_t)  -> () 
let _ = fun (_ : vhdl_assoc_element_t)  -> () 
let _ = fun (_ : vhdl_element_assoc_t)  -> () 
let _ = fun (_ : vhdl_array_attributes_t)  -> () 
let _ = fun (_ : vhdl_signal_attributes_t)  -> () 
let _ = fun (_ : vhdl_string_attributes_t)  -> () 
let _ = fun (_ : vhdl_suffix_selection_t)  -> () 
let _ = fun (_ : 'basetype vhdl_type_attributes_t)  -> () 
let _ = fun (_ : vhdl_parameter_t)  -> () 
let _ = fun (_ : vhdl_subprogram_spec_t)  -> () 
let _ = fun (_ : vhdl_sequential_stmt_t)  -> () 
let _ = fun (_ : vhdl_if_case_t)  -> () 
let _ = fun (_ : vhdl_case_item_t)  -> () 
let _ = fun (_ : vhdl_declaration_t)  -> () 
let _ = fun (_ : vhdl_signal_selection_t)  -> () 
let _ = fun (_ : vhdl_signal_condition_t)  -> () 
let _ = fun (_ : vhdl_conditional_signal_t)  -> () 
let _ = fun (_ : vhdl_process_t)  -> () 
let _ = fun (_ : vhdl_selected_signal_t)  -> () 
let _ = fun (_ : vhdl_port_mode_t)  -> () 
let _ = fun (_ : vhdl_component_instantiation_t)  -> ()
let _ = fun (_ : vhdl_concurrent_stmt_t)  -> () 
let _ = fun (_ : vhdl_port_t)  -> () 
let _ = fun (_ : vhdl_entity_t)  -> () 
let _ = fun (_ : vhdl_package_t)  -> () 
let _ = fun (_ : vhdl_load_t)  -> () 
let _ = fun (_ : vhdl_architecture_t)  -> () 
let _ = fun (_ : vhdl_configuration_t)  -> () 
let _ = fun (_ : vhdl_library_unit_t)  -> () 
let _ = fun (_ : vhdl_design_unit_t)  -> () 
let _ = fun (_ : vhdl_design_file_t)  -> () 
let _ = fun (_ : vhdl_file_t)  -> () 

class virtual vhdl_map =
  object (self)
    method virtual  string : string -> string
    method virtual  list : 'a . ('a -> 'a) -> 'a list -> 'a list
    method virtual  unit : unit -> unit
    method virtual  bool : bool -> bool
    method virtual  option : 'a . ('a -> 'a) -> 'a option -> 'a option
    method virtual  int : int -> int
    method virtual  vhdl_name_t : vhdl_name_t -> vhdl_name_t
    method virtual  vhdl_definition_t : vhdl_definition_t -> vhdl_definition_t
    method virtual  vhdl_port_t : vhdl_port_t -> vhdl_port_t
    method virtual  vhdl_expr_t : vhdl_expr_t -> vhdl_expr_t
    method virtual  vhdl_port_mode_t : vhdl_port_mode_t -> vhdl_port_mode_t
    method virtual  vhdl_subtype_indication_t : vhdl_subtype_indication_t -> vhdl_subtype_indication_t
    method virtual  vhdl_conditional_signal_t : vhdl_conditional_signal_t -> vhdl_conditional_signal_t
    method virtual  vhdl_process_t : vhdl_process_t -> vhdl_process_t
    method virtual  vhdl_selected_signal_t : vhdl_selected_signal_t -> vhdl_selected_signal_t
    method virtual  vhdl_signal_selection_t : vhdl_signal_selection_t -> vhdl_signal_selection_t
    method virtual  vhdl_suffix_selection_t : vhdl_suffix_selection_t -> vhdl_suffix_selection_t
    method virtual  vhdl_declaration_t : vhdl_declaration_t -> vhdl_declaration_t
    method virtual  vhdl_sequential_stmt_t : vhdl_sequential_stmt_t -> vhdl_sequential_stmt_t
    method virtual  vhdl_signal_condition_t : vhdl_signal_condition_t -> vhdl_signal_condition_t
    method virtual  vhdl_cst_val_t : vhdl_cst_val_t -> vhdl_cst_val_t
    method virtual  vhdl_subprogram_spec_t : vhdl_subprogram_spec_t -> vhdl_subprogram_spec_t
    method virtual  vhdl_discrete_range_t : vhdl_discrete_range_t -> vhdl_discrete_range_t
    method virtual  vhdl_parameter_t : vhdl_parameter_t -> vhdl_parameter_t
    method virtual  vhdl_component_instantiation_t : vhdl_component_instantiation_t -> vhdl_component_instantiation_t
    method virtual  vhdl_concurrent_stmt_t : vhdl_concurrent_stmt_t -> vhdl_concurrent_stmt_t
    method virtual  vhdl_declaration_t : vhdl_declaration_t -> vhdl_declaration_t
    method virtual  vhdl_architecture_t : vhdl_architecture_t -> vhdl_architecture_t
    method virtual  vhdl_configuration_t : vhdl_configuration_t -> vhdl_configuration_t
    method virtual  vhdl_entity_t : vhdl_entity_t -> vhdl_entity_t
    method virtual  vhdl_package_t : vhdl_package_t -> vhdl_package_t
    method virtual  vhdl_library_unit_t : vhdl_library_unit_t -> vhdl_library_unit_t
    method virtual  vhdl_load_t : vhdl_load_t -> vhdl_load_t
    method virtual  vhdl_design_unit_t : vhdl_design_unit_t -> vhdl_design_unit_t
    method virtual  vhdl_design_file_t : vhdl_design_file_t -> vhdl_design_file_t

    method vhdl_cst_val_t : vhdl_cst_val_t -> vhdl_cst_val_t=
      fun x  ->
        match x with
        | CstInt a -> let a = self#int a  in CstInt a
        | CstStdLogic a -> let a = self#string a  in CstStdLogic a
        | CstLiteral a -> let a = self#string a  in CstLiteral a

    method vhdl_type_t : vhdl_type_t -> vhdl_type_t=
      fun x  ->
        match x with
        | Base a -> let a = self#string a  in Base a
        | Range (a,b,c) ->
            let a = self#option self#string a  in
            let b = self#int b  in let c = self#int c  in Range (a, b, c)
        | Bit_vector (a,b) ->
            let a = self#int a  in let b = self#int b  in Bit_vector (a, b)
        | Array (a,b,c) ->
            let a = self#int a  in
            let b = self#int b  in
            let c = self#vhdl_type_t c  in Array (a, b, c)
        | Enumerated a -> let a = self#list self#string a  in Enumerated a
        | Void  -> Void
    method vhdl_subtype_indication_t :
      vhdl_subtype_indication_t -> vhdl_subtype_indication_t=
      fun { name; functionName; const }  ->
        let name = self#vhdl_name_t name  in
        let functionName = self#vhdl_name_t functionName  in
        let const = self#vhdl_constraint_t const  in
        { name; functionName; const }
    method vhdl_discrete_range_t :
      vhdl_discrete_range_t -> vhdl_discrete_range_t=
      fun x  ->
        match x with
        | SubDiscreteRange a ->
            let a = self#vhdl_subtype_indication_t a  in SubDiscreteRange a
        | NamedRange a -> let a = self#vhdl_name_t a  in NamedRange a
        | DirectedRange { direction; from; _to } ->
            let direction = self#string direction  in
            let from = self#vhdl_expr_t from  in
            let _to = self#vhdl_expr_t _to  in
            DirectedRange { direction; from; _to }

    method vhdl_constraint_t : vhdl_constraint_t -> vhdl_constraint_t=
      fun x  ->
        match x with
        | RefConstraint { ref_name } ->
            let ref_name = self#vhdl_name_t ref_name  in
            RefConstraint { ref_name }
        | RangeConstraint { range } ->
            let range = self#vhdl_discrete_range_t range  in
            RangeConstraint { range }
        | IndexConstraint { ranges } ->
            let ranges = self#list self#vhdl_discrete_range_t ranges  in
            IndexConstraint { ranges }
        | ArrayConstraint { ranges; sub } ->
            let ranges = self#list self#vhdl_discrete_range_t ranges  in
            let sub = self#vhdl_constraint_t sub  in
            ArrayConstraint { ranges; sub }
        | RecordConstraint  -> RecordConstraint
        | NoConstraint  -> NoConstraint

    method vhdl_definition_t : vhdl_definition_t -> vhdl_definition_t=
      fun x  ->
        match x with
        | Type { name; definition } ->
            let name = self#vhdl_name_t name  in
            let definition = self#vhdl_type_t definition  in
            Type { name; definition }
        | Subtype { name; typ } ->
            let name = self#vhdl_name_t name  in
            let typ = self#vhdl_subtype_indication_t typ  in
            Subtype { name; typ }
    method vhdl_expr_t : vhdl_expr_t -> vhdl_expr_t=
      fun x  ->
        match x with
        | Call a -> let a = self#vhdl_name_t a  in Call a
        | Cst a -> let a = self#vhdl_cst_val_t a  in Cst a
        | Op { id; args } ->
            let id = self#string id  in
            let args = self#list self#vhdl_expr_t args  in Op { id; args }
        | IsNull  -> IsNull
        | Time { value; phy_unit } ->
            let value = self#int value  in
            let phy_unit = self#string phy_unit  in Time { value; phy_unit }
        | Sig { name; att } ->
            let name = self#vhdl_name_t name  in
            let att = self#option self#vhdl_signal_attributes_t att  in
            Sig { name; att }
        | SuffixMod { expr; selection } ->
            let expr = self#vhdl_expr_t expr  in
            let selection = self#vhdl_suffix_selection_t selection  in
            SuffixMod { expr; selection }
        | Aggregate { elems } ->
            let elems = self#list self#vhdl_element_assoc_t elems  in
            Aggregate { elems }
        | Others  -> Others
    method vhdl_name_t : vhdl_name_t -> vhdl_name_t=
      fun x  ->
        match x with
        | Simple a -> let a = self#string a  in Simple a
        | Identifier a -> let a = self#string a  in Identifier a
        | Selected a -> let a = self#list self#vhdl_name_t a  in Selected a
        | Index { id; exprs } ->
            let id = self#vhdl_name_t id  in
            let exprs = self#list self#vhdl_expr_t exprs  in
            Index { id; exprs }
        | Slice { id; range } ->
            let id = self#vhdl_name_t id  in
            let range = self#vhdl_discrete_range_t range  in
            Slice { id; range }
        | Attribute { id; designator; expr } ->
            let id = self#vhdl_name_t id  in
            let designator = self#vhdl_name_t designator  in
            let expr = self#vhdl_expr_t expr  in
            Attribute { id; designator; expr }
        | Function { id; assoc_list } ->
            let id = self#vhdl_name_t id  in
            let assoc_list = self#list self#vhdl_assoc_element_t assoc_list
               in
            Function { id; assoc_list }
        | NoName  -> NoName
    method vhdl_assoc_element_t :
      vhdl_assoc_element_t -> vhdl_assoc_element_t=
      fun
        { formal_name; formal_arg; actual_name; actual_designator;
          actual_expr }
         ->
        let formal_name = self#option self#vhdl_name_t formal_name  in
        let formal_arg = self#option self#vhdl_name_t formal_arg  in
        let actual_name = self#option self#vhdl_name_t actual_name  in
        let actual_designator =
          self#option self#vhdl_name_t actual_designator  in
        let actual_expr = self#option self#vhdl_expr_t actual_expr  in
        {
          formal_name;
          formal_arg;
          actual_name;
          actual_designator;
          actual_expr
        }
    method vhdl_element_assoc_t :
      vhdl_element_assoc_t -> vhdl_element_assoc_t=
      fun { choices; expr }  ->
        let choices = self#list self#vhdl_expr_t choices  in
        let expr = self#vhdl_expr_t expr  in { choices; expr }
    method vhdl_array_attributes_t :
      vhdl_array_attributes_t -> vhdl_array_attributes_t=
      fun x  ->
        match x with
        | AAttInt { id; arg } ->
            let id = self#string id  in
            let arg = self#int arg  in AAttInt { id; arg }
        | AAttAscending  -> AAttAscending
    method vhdl_signal_attributes_t :
      vhdl_signal_attributes_t -> vhdl_signal_attributes_t=
      fun x  -> match x with | SigAtt a -> let a = self#string a  in SigAtt a
    method vhdl_string_attributes_t :
      vhdl_string_attributes_t -> vhdl_string_attributes_t=
      fun x  ->
        match x with | StringAtt a -> let a = self#string a  in StringAtt a
    method vhdl_suffix_selection_t : vhdl_suffix_selection_t -> vhdl_suffix_selection_t=
      fun x  ->
        match x with
        | Idx a -> let a = self#int a  in Idx a
        | SuffixRange (a,b) ->
            let a = self#int a  in let b = self#int b  in SuffixRange (a, b)

    method vhdl_type_attributes_t :
      'a .
        ('a -> 'a) -> 'a vhdl_type_attributes_t -> 'a vhdl_type_attributes_t=
      fun _basetype  ->
        fun x  ->
          match x with
          | TAttNoArg { id } -> let id = self#string id  in TAttNoArg { id }
          | TAttIntArg { id; arg } ->
              let id = self#string id  in
              let arg = self#int arg  in TAttIntArg { id; arg }
          | TAttValArg { id; arg } ->
              let id = self#string id  in
              let arg = _basetype arg  in TAttValArg { id; arg }
          | TAttStringArg { id; arg } ->
              let id = self#string id  in
              let arg = self#string arg  in TAttStringArg { id; arg }

    method vhdl_parameter_t : vhdl_parameter_t -> vhdl_parameter_t=
      fun { names; mode; typ; init_val }  ->
        let names = self#list self#vhdl_name_t names  in
        let mode = self#list self#string mode  in
        let typ = self#vhdl_subtype_indication_t typ  in
        let init_val = self#option self#vhdl_cst_val_t init_val  in
        { names; mode; typ; init_val }

    method vhdl_subprogram_spec_t :
      vhdl_subprogram_spec_t -> vhdl_subprogram_spec_t=
      fun { name; typeMark; parameters; isPure }  ->
        let name = self#string name  in
        let typeMark = self#vhdl_name_t typeMark  in
        let parameters = self#list self#vhdl_parameter_t parameters  in
        let isPure = self#bool isPure  in
        { name; typeMark; parameters; isPure }

    method vhdl_sequential_stmt_t :
      vhdl_sequential_stmt_t -> vhdl_sequential_stmt_t=
      fun x  ->
        match x with
        | VarAssign { label; lhs; rhs } ->
            let label = self#vhdl_name_t label  in
            let lhs = self#vhdl_name_t lhs  in
            let rhs = self#vhdl_expr_t rhs  in VarAssign { label; lhs; rhs }
        | SigSeqAssign { label; lhs; rhs } ->
            let label = self#vhdl_name_t label  in
            let lhs = self#vhdl_name_t lhs  in
            let rhs = self#list self#vhdl_expr_t rhs  in
            SigSeqAssign { label; lhs; rhs }
        | If { label; if_cases; default } ->
            let label = self#vhdl_name_t label  in
            let if_cases = self#list self#vhdl_if_case_t if_cases  in
            let default = self#list self#vhdl_sequential_stmt_t default  in
            If { label; if_cases; default }
        | Case { label; guard; branches } ->
            let label = self#vhdl_name_t label  in
            let guard = self#vhdl_expr_t guard  in
            let branches = self#list self#vhdl_case_item_t branches  in
            Case { label; guard; branches }
        | Exit { label; loop_label; condition } ->
            let label = self#vhdl_name_t label  in
            let loop_label = self#option self#string loop_label  in
            let condition = self#option self#vhdl_expr_t condition  in
            Exit { label; loop_label; condition }
        | Assert { label; cond; report; severity } ->
            let label = self#vhdl_name_t label  in
            let cond = self#vhdl_expr_t cond  in
            let report = self#vhdl_expr_t report  in
            let severity = self#vhdl_expr_t severity  in
            Assert { label; cond; report; severity }
        | ProcedureCall { label; name; assocs } ->
            let label = self#vhdl_name_t label  in
            let name = self#vhdl_name_t name  in
            let assocs = self#list self#vhdl_assoc_element_t assocs  in
            ProcedureCall { label; name; assocs }
        | Wait  -> Wait
        | Null { label } ->
            let label = self#vhdl_name_t label  in Null { label }
        | Return { label } ->
            let label = self#vhdl_name_t label  in Return { label }
    method vhdl_if_case_t : vhdl_if_case_t -> vhdl_if_case_t=
      fun { if_cond; if_block }  ->
        let if_cond = self#vhdl_expr_t if_cond  in
        let if_block = self#list self#vhdl_sequential_stmt_t if_block  in
        { if_cond; if_block }
    method vhdl_case_item_t : vhdl_case_item_t -> vhdl_case_item_t=
      fun { when_cond; when_stmt }  ->
        let when_cond = self#list self#vhdl_expr_t when_cond  in
        let when_stmt = self#list self#vhdl_sequential_stmt_t when_stmt  in
        { when_cond; when_stmt }

    method vhdl_declaration_t : vhdl_declaration_t -> vhdl_declaration_t=
      fun x  ->
        match x with
        | VarDecl { names; typ; init_val } ->
            let names = self#list self#vhdl_name_t names  in
            let typ = self#vhdl_subtype_indication_t typ  in
            let init_val = self#vhdl_expr_t init_val  in
            VarDecl { names; typ; init_val }
        | CstDecl { names; typ; init_val } ->
            let names = self#list self#vhdl_name_t names  in
            let typ = self#vhdl_subtype_indication_t typ  in
            let init_val = self#vhdl_expr_t init_val  in
            CstDecl { names; typ; init_val }
        | SigDecl { names; typ; init_val } ->
            let names = self#list self#vhdl_name_t names  in
            let typ = self#vhdl_subtype_indication_t typ  in
            let init_val = self#vhdl_expr_t init_val  in
            SigDecl { names; typ; init_val }
        | Subprogram { name; kind; spec; decl_part; stmts } ->
            let name = self#vhdl_name_t name  in
            let kind = self#string kind  in
            let spec = self#vhdl_subprogram_spec_t spec  in
            let decl_part = self#list self#vhdl_declaration_t decl_part  in
            let stmts = self#list self#vhdl_sequential_stmt_t stmts  in
            Subprogram { name; kind; spec; decl_part; stmts }

    method vhdl_signal_condition_t : vhdl_signal_condition_t -> vhdl_signal_condition_t=
      fun { expr; cond }  ->
        let expr = self#list self#vhdl_expr_t expr  in
        let cond = self#vhdl_expr_t cond  in { expr; cond }

    method vhdl_signal_selection_t : vhdl_signal_selection_t -> vhdl_signal_selection_t=
      fun { expr; when_sel }  ->
        let expr = self#vhdl_expr_t expr  in
        let when_sel = self#list self#vhdl_expr_t when_sel  in
        { expr; when_sel }

    method vhdl_conditional_signal_t :
      vhdl_conditional_signal_t -> vhdl_conditional_signal_t=
      fun { postponed; label; lhs; rhs; cond; delay }  ->
        let postponed = self#bool postponed  in
        let label = self#vhdl_name_t label  in
        let lhs = self#vhdl_name_t lhs  in
        let rhs = self#list self#vhdl_signal_condition_t rhs  in
        let cond = self#vhdl_expr_t cond  in
        let delay = self#vhdl_expr_t delay  in
        { postponed; label; lhs; rhs; cond; delay }


    method vhdl_process_t : vhdl_process_t -> vhdl_process_t=
      fun { id; declarations; active_sigs; body }  ->
        let id = self#vhdl_name_t id  in
        let declarations =
          self#option (self#list self#vhdl_declaration_t) declarations  in
        let active_sigs = self#list self#vhdl_name_t active_sigs  in
        let body = self#list self#vhdl_sequential_stmt_t body  in
        { id; declarations; active_sigs; body }

    method vhdl_selected_signal_t : vhdl_selected_signal_t -> vhdl_selected_signal_t=
      fun { postponed; label; lhs; sel; branches; delay }  ->
        let postponed = self#bool postponed  in
        let label = self#vhdl_name_t label  in
        let lhs = self#vhdl_name_t lhs  in
        let sel = self#vhdl_expr_t sel  in
        let branches = self#list self#vhdl_signal_selection_t branches  in
        let delay = self#option self#vhdl_expr_t delay  in
        { postponed; label; lhs; sel; branches; delay }

    method vhdl_port_mode_t : vhdl_port_mode_t -> vhdl_port_mode_t=
      fun x  -> x

    method vhdl_component_instantiation_t :
      vhdl_component_instantiation_t -> vhdl_component_instantiation_t=
      fun { name; inst_unit; archi_name; generic_map; port_map }  ->
        let name = self#vhdl_name_t name  in
        let inst_unit = self#vhdl_name_t inst_unit  in
        let archi_name = self#option self#vhdl_name_t archi_name  in
        let generic_map = self#option self#vhdl_assoc_element_t generic_map
           in
        let port_map = self#option self#vhdl_assoc_element_t port_map  in
        { name; inst_unit; archi_name; generic_map; port_map }

    method vhdl_concurrent_stmt_t :
      vhdl_concurrent_stmt_t -> vhdl_concurrent_stmt_t=
      fun x  ->
        match x with
        | SigAssign a -> let a = self#vhdl_conditional_signal_t a  in SigAssign a
        | Process a -> let a = self#vhdl_process_t a  in Process a
        | SelectedSig a -> let a = self#vhdl_selected_signal_t a  in SelectedSig a
        | ComponentInst a -> let a = self#vhdl_component_instantiation_t a  in ComponentInst a 

    method vhdl_port_t : vhdl_port_t -> vhdl_port_t=
      fun { names; mode; typ; expr }  ->
        let names = self#list self#vhdl_name_t names  in
        let mode = self#vhdl_port_mode_t mode  in
        let typ = self#vhdl_subtype_indication_t typ  in
        let expr = self#vhdl_expr_t expr  in { names; mode; typ; expr }

    method vhdl_entity_t : vhdl_entity_t -> vhdl_entity_t=
      fun { name; generics; ports; declaration; stmts }  ->
        let name = self#vhdl_name_t name  in
        let generics = self#list self#vhdl_port_t generics  in
        let ports = self#list self#vhdl_port_t ports  in
        let declaration = self#list self#vhdl_declaration_t declaration  in
        let stmts = self#list self#vhdl_concurrent_stmt_t stmts  in
        { name; generics; ports; declaration; stmts }

    method vhdl_package_t : vhdl_package_t -> vhdl_package_t=
      fun { name; shared_defs }  ->
        let name = self#vhdl_name_t name  in
        let shared_defs = self#list self#vhdl_definition_t shared_defs  in
        { name; shared_defs }

    method vhdl_load_t : vhdl_load_t -> vhdl_load_t=
      fun x  ->
        match x with
        | Library a -> let a = self#list self#vhdl_name_t a  in Library a
        | Use a -> let a = self#list self#vhdl_name_t a  in Use a

    method vhdl_architecture_t : vhdl_architecture_t -> vhdl_architecture_t=
      fun { name; entity; declarations; body }  ->
        let name = self#vhdl_name_t name  in
        let entity = self#vhdl_name_t entity  in
        let declarations = self#list self#vhdl_declaration_t declarations  in
        let body = self#list self#vhdl_concurrent_stmt_t body  in
        { name; entity; declarations; body }

    method vhdl_configuration_t :
      vhdl_configuration_t -> vhdl_configuration_t= self#unit

    method vhdl_library_unit_t : vhdl_library_unit_t -> vhdl_library_unit_t=
      fun x  ->
        match x with
        | Package a -> let a = self#vhdl_package_t a  in Package a
        | Entities a -> let a = self#vhdl_entity_t a  in Entities a
        | Architecture a ->
            let a = self#vhdl_architecture_t a  in Architecture a
        | Configuration a ->
            let a = self#vhdl_configuration_t a  in Configuration a

    method vhdl_design_unit_t : vhdl_design_unit_t -> vhdl_design_unit_t=
      fun { contexts; library }  ->
        let contexts = self#list self#vhdl_load_t contexts  in
        let library = self#vhdl_library_unit_t library  in
        { contexts; library }

    method vhdl_design_file_t : vhdl_design_file_t -> vhdl_design_file_t=
      fun { design_units }  ->
        let design_units = self#list self#vhdl_design_unit_t design_units  in
        { design_units }

    method vhdl_file_t : vhdl_file_t -> vhdl_file_t=
      fun { design_file }  ->
        let design_file = self#vhdl_design_file_t design_file  in
        { design_file }
  end
