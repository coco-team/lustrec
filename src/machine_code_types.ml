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
