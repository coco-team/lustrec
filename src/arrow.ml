open Lustre_types

let arrow_id = "_arrow"

let arrow_typ = Types.new_ty Types.Tunivar

let arrow_desc =
  {
    node_id = arrow_id;
    node_type = Type_predef.type_bin_poly_op;
    node_clock = Clock_predef.ck_bin_univ;
    node_inputs= [Corelang.dummy_var_decl "_in1" arrow_typ; Corelang.dummy_var_decl "_in2" arrow_typ];
    node_outputs= [Corelang.dummy_var_decl "_out" arrow_typ];
    node_locals= [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_stmts= [];
    node_dec_stateless = false;
    node_stateless = Some false;
    node_spec = None;
    node_annot = [];  }

let arrow_top_decl =
  {
    top_decl_desc = Node arrow_desc;
    top_decl_owner = (Options_management.core_dependency "arrow");
    top_decl_itf = false;
    top_decl_loc = Location.dummy_loc
  }
