(************ Machine code types *************)
open Lustre_types
  
type value_t =
  {
    value_desc: value_t_desc;
    value_type: Types.type_expr;
    value_annot: expr_annot option
  }
and value_t_desc =
  | Cst of constant
  | LocalVar of var_decl
  | StateVar of var_decl
  | Fun of ident * value_t list
  | Array of value_t list
  | Access of value_t * value_t
  | Power of value_t * value_t

type instr_t =
  {
    instr_desc: instr_t_desc; (* main data: the content *)
    (* lustre_expr: expr option; (* possible representation as a lustre expression *) *)
    lustre_eq: eq option;     (* possible representation as a lustre flow equation *)
  }
and instr_t_desc =
  | MLocalAssign of var_decl * value_t
  | MStateAssign of var_decl * value_t
  | MReset of ident
  | MNoReset of ident
  | MStep of var_decl list * ident * value_t list
  | MBranch of value_t * (label * instr_t list) list
  | MComment of string

      type step_t = {
  step_checks: (Location.t * value_t) list;
  step_inputs: var_decl list;
  step_outputs: var_decl list;
  step_locals: var_decl list;
  step_instrs: instr_t list;
  step_asserts: value_t list;
}

type static_call = top_decl * (Dimension.dim_expr list)

type machine_t = {
  mname: node_desc;
  mmemory: var_decl list;
  mcalls: (ident * static_call) list; (* map from stateful/stateless instance to node, no internals *)
  minstances: (ident * static_call) list; (* sub-map of mcalls, from stateful instance to node *)
  minit: instr_t list;
  mstatic: var_decl list; (* static inputs only *)
  mconst: instr_t list; (* assignments of node constant locals *)
  mstep: step_t;
  mspec: node_annot option;
  mannot: expr_annot list;
}
