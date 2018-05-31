val pp_val: Format.formatter -> Machine_code_types.value_t -> unit
val is_memory: Machine_code_types.machine_t -> Lustre_types.var_decl -> bool
val is_output: Machine_code_types.machine_t -> Lustre_types.var_decl -> bool
val is_const_value: Machine_code_types.value_t -> bool
val get_const_assign: Machine_code_types.machine_t -> Lustre_types.var_decl -> Machine_code_types.value_t
val get_stateless_status: Machine_code_types.machine_t -> bool * bool
val mk_val: Machine_code_types.value_t_desc -> Types.type_expr -> Machine_code_types.value_t
val mk_conditional: ?lustre_eq:Lustre_types.eq -> Machine_code_types.value_t -> Machine_code_types.instr_t list -> Machine_code_types.instr_t list -> Machine_code_types.instr_t
val empty_machine: Machine_code_types.machine_t
val arrow_machine: Machine_code_types.machine_t
val new_instance: Lustre_types.node_desc -> Lustre_types.top_decl -> Lustre_types.tag -> Lustre_types.ident
val value_of_dimension: Machine_code_types.machine_t -> Dimension.dim_expr -> Machine_code_types.value_t
val dimension_of_value:Machine_code_types.value_t -> Dimension.dim_expr
val pp_instr: Format.formatter -> Machine_code_types.instr_t -> unit
val pp_instrs: Format.formatter -> Machine_code_types.instr_t list -> unit
val pp_machines: Format.formatter -> Machine_code_types.machine_t list -> unit
val get_machine_opt: string -> Machine_code_types.machine_t list -> Machine_code_types.machine_t option
val get_node_def: string -> Machine_code_types.machine_t -> Lustre_types.node_desc
val join_guards_list: Machine_code_types.instr_t list -> Machine_code_types.instr_t list
