open Format


let merge_expr_annot ann1 ann2 =
  match ann1, ann2 with
    | None, None -> assert false
    | Some _, None -> ann1
    | None, Some _ -> ann2
    | Some ann1, Some ann2 -> Some (ann1@ann2)


type ident = Utils.ident
type rat = Utils.rat
type tag = Utils.tag

type constant =
  | EConst_int of int
  | EConst_real of string
  | EConst_float of float
  | EConst_bool of bool
  | EConst_string of string

type type_dec =
    {ty_dec_desc: type_dec_desc;
     ty_dec_loc: Location.t}

and type_dec_desc =
  | Tydec_any
  | Tydec_int
  | Tydec_real
  | Tydec_float
  | Tydec_bool
  | Tydec_clock of type_dec_desc
  | Tydec_const of ident
  | Tydec_enum of ident list
  | Tydec_struct of (ident * type_dec_desc) list
  | Tydec_array of Dimension.dim_expr * type_dec_desc

type clock_dec =
    {ck_dec_desc: clock_dec_desc;
     ck_dec_loc: Location.t}

and clock_dec_desc =
  | Ckdec_any
  | Ckdec_bool of (ident * ident) list 
  | Ckdec_pclock of int * rat

type var_decl = 
    {var_id: ident;
     var_dec_type: type_dec;
     var_dec_clock: clock_dec;
     var_dec_const: bool;
     mutable var_type: Types.type_expr;
     mutable var_clock: Clocks.clock_expr;
     var_loc: Location.t}

(* The tag of an expression is a unique identifier used to distinguish
   different instances of the same node *)
type eexpr =
    {eexpr_tag: tag;
     eexpr_desc: eexpr_desc;
     mutable eexpr_type: Types.type_expr;
     mutable eexpr_clock: Clocks.clock_expr;
     eexpr_loc: Location.t}

and eexpr_desc =
  | EExpr_const of constant
  | EExpr_ident of ident
  | EExpr_tuple of eexpr list
  | EExpr_arrow of eexpr * eexpr
  | EExpr_fby of eexpr * eexpr
  (* | EExpr_concat of eexpr * eexpr *)
  (* | EExpr_tail of eexpr *)
  | EExpr_pre of eexpr
  | EExpr_when of eexpr * ident
  (* | EExpr_whennot of eexpr * ident *)
  | EExpr_merge of ident * eexpr * eexpr
  | EExpr_appl of ident * eexpr * ident option
  (* | EExpr_uclock of eexpr * int *)
  (* | EExpr_dclock of eexpr * int *)
  (* | EExpr_phclock of eexpr * rat *)
  | EExpr_exists of var_decl list * eexpr
  | EExpr_forall of var_decl list * eexpr

type ensures_t = EnsuresExpr of eexpr | SpecObserverNode of (string * eexpr list)

type node_annot = {
  requires: eexpr list;
  ensures: ensures_t list;
  behaviors: (string * eexpr list * ensures_t list) list
}


type expr_annot = (string list * eexpr) list 

let merge_node_annot ann1 ann2 =
  { requires = ann1.requires @ ann2.requires;
    ensures = ann1.ensures @ ann2.ensures;
    behaviors = ann1.behaviors @ ann2.behaviors;
  }

let mkeexpr loc d =
  { eexpr_tag = Utils.new_tag ();
    eexpr_desc = d;
    eexpr_type = Types.new_var ();
    eexpr_clock = Clocks.new_var true;
    eexpr_loc = loc }

let mkepredef_call loc funname args =
  mkeexpr loc (EExpr_appl (funname, mkeexpr loc (EExpr_tuple args), None))

let mkepredef_unary_call loc funname arg =
  mkeexpr loc (EExpr_appl (funname, arg, None))

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
