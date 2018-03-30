val mk_expr_alias_opt: bool -> Lustre_types.node_desc -> (Lustre_types.eq list * Lustre_types.var_decl list)-> Lustre_types.expr -> (Lustre_types.eq list * Lustre_types.var_decl list) * Lustre_types.expr
val normalize_prog: ?backend:string -> Lustre_types.program -> Lustre_types.program
