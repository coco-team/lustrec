(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

open Utils
open Lustre_types
open Machine_code_types
open Corelang
open Normalization
open Machine_code_common

let mpfr_module = mktop (Open(false, "mpfr_lustre"))
let cpt_fresh = ref 0
  
let mpfr_rnd () = "MPFR_RNDN"

let mpfr_prec () = !Options.mpfr_prec

let inject_id = "MPFRId"

let inject_copy_id = "mpfr_set"

let inject_real_id = "mpfr_set_flt"

let inject_init_id = "mpfr_init2"

let inject_clear_id = "mpfr_clear"

let mpfr_t = "mpfr_t"

let unfoldable_value value =
  not (Types.is_real_type value.value_type && is_const_value value)

let inject_id_id expr =
  let e = mkpredef_call expr.expr_loc inject_id [expr] in
  { e with
    expr_type = Type_predef.type_real;
    expr_clock = expr.expr_clock;
  }

let pp_inject_real pp_var pp_val fmt var value =
  Format.fprintf fmt "%s(%a, %a, %s);"
    inject_real_id
    pp_var var
    pp_val value
    (mpfr_rnd ())

let inject_assign expr =
  let e = mkpredef_call expr.expr_loc inject_copy_id [expr] in
  { e with
    expr_type = Type_predef.type_real;
    expr_clock = expr.expr_clock;
  }

let pp_inject_copy pp_var fmt var value =
  Format.fprintf fmt "%s(%a, %a, %s);"
    inject_copy_id
    pp_var var
    pp_var value
    (mpfr_rnd ())

let rec pp_inject_assign pp_var fmt var value =
  if is_const_value value
  then
    pp_inject_real pp_var pp_var fmt var value
  else
    pp_inject_copy pp_var fmt var value

let pp_inject_init pp_var fmt var =
  Format.fprintf fmt "%s(%a, %i);"
    inject_init_id
    pp_var var
    (mpfr_prec ())

let pp_inject_clear pp_var fmt var =
  Format.fprintf fmt "%s(%a);"
    inject_clear_id
    pp_var var

let base_inject_op id =
  match id with
  | "+"      -> "MPFRPlus"
  | "-"      -> "MPFRMinus"
  | "*"      -> "MPFRTimes"
  | "/"      -> "MPFRDiv"
  | "uminus" -> "MPFRUminus"
  | "<="     -> "MPFRLe"
  | "<"      -> "MPFRLt"
  | ">="     -> "MPFRGe"
  | ">"      -> "MPFRGt"
  | "="      -> "MPFREq"
  | "!="     -> "MPFRNeq"
  (* Math library functions *)
  | "acos" -> "MPFRacos"
  | "acosh" -> "MPFRacosh"
  | "asin" -> "MPFRasin"
  | "asinh" -> "MPFRasinh"
  | "atan" -> "MPFRatan"
  | "atan2" -> "MPFRatan2"
  | "atanh" -> "MPFRatanh"
  | "cbrt" -> "MPFRcbrt"
  | "cos" -> "MPFRcos"
  | "cosh" -> "MPFRcosh"
  | "ceil" -> "MPFRceil"
  | "erf" -> "MPFRerf"
  | "exp" -> "MPFRexp"
  | "fabs" -> "MPFRfabs"
  | "floor" -> "MPFRfloor"
  | "fmod" -> "MPFRfmod"
  | "log" -> "MPFRlog"
  | "log10" -> "MPFRlog10"
  | "pow" -> "MPFRpow"
  | "round" -> "MPFRround"
  | "sin" -> "MPFRsin"
  | "sinh" -> "MPFRsinh"
  | "sqrt" -> "MPFRsqrt"
  | "trunc" -> "MPFRtrunc"
  | "tan" -> "MPFRtan"
  | _        -> raise Not_found

let inject_op id =
  Format.eprintf "trying to inject mpfr into function %s@." id;
  try
    base_inject_op id
  with Not_found -> id

let homomorphic_funs =
  List.fold_right (fun id res -> try base_inject_op id :: res with Not_found -> res) Basic_library.internal_funs []

let is_homomorphic_fun id =
  List.mem id homomorphic_funs

let inject_call expr =
  match expr.expr_desc with
  | Expr_appl (id, args, None) when not (Basic_library.is_expr_internal_fun expr) ->
    { expr with expr_desc = Expr_appl (inject_op id, args, None) }
  | _ -> expr

let expr_of_const_array expr =
  match expr.expr_desc with
  | Expr_const (Const_array cl) ->
    let typ = Types.array_element_type expr.expr_type in
    let expr_of_const c =
      { expr_desc = Expr_const c;
	expr_type = typ;
	expr_clock = expr.expr_clock;
	expr_loc = expr.expr_loc;
	expr_delay = Delay.new_var ();
	expr_annot = None;
	expr_tag = new_tag ();
      }
    in { expr with expr_desc = Expr_array (List.map expr_of_const cl) }
  | _                           -> assert false

(* inject_<foo> : defs * used vars -> <foo> -> (updated defs * updated vars) * normalized <foo> *)
let rec inject_list alias node inject_element defvars elist =
  List.fold_right
    (fun t (defvars, qlist) ->
      let defvars, norm_t = inject_element alias node defvars t in
      (defvars, norm_t :: qlist)
    ) elist (defvars, [])

let rec inject_expr ?(alias=true) node defvars expr =
let res =
  match expr.expr_desc with
  | Expr_const (Const_real _)  -> mk_expr_alias_opt alias node defvars expr
  | Expr_const (Const_array _) -> inject_expr ~alias:alias node defvars (expr_of_const_array expr)
  | Expr_const (Const_struct _) -> assert false
  | Expr_ident _
  | Expr_const _  -> defvars, expr
  | Expr_array elist ->
    let defvars, norm_elist = inject_list alias node (fun _ -> inject_expr ~alias:true) defvars elist in
    let norm_expr = { expr with expr_desc = Expr_array norm_elist } in
    defvars, norm_expr
  | Expr_power (e1, d) ->
    let defvars, norm_e1 = inject_expr node defvars e1 in
    let norm_expr = { expr with expr_desc = Expr_power (norm_e1, d) } in
    defvars, norm_expr
  | Expr_access (e1, d) ->
    let defvars, norm_e1 = inject_expr node defvars e1 in
    let norm_expr = { expr with expr_desc = Expr_access (norm_e1, d) } in
    defvars, norm_expr
  | Expr_tuple elist -> 
    let defvars, norm_elist =
      inject_list alias node (fun alias -> inject_expr ~alias:alias) defvars elist in
    let norm_expr = { expr with expr_desc = Expr_tuple norm_elist } in
    defvars, norm_expr
  | Expr_appl (id, args, r) ->
    let defvars, norm_args = inject_expr node defvars args in
    let norm_expr = { expr with expr_desc = Expr_appl (id, norm_args, r) } in
    mk_expr_alias_opt alias node defvars (inject_call norm_expr)
  | Expr_arrow _ -> defvars, expr
  | Expr_pre e ->
    let defvars, norm_e = inject_expr node defvars e in
    let norm_expr = { expr with expr_desc = Expr_pre norm_e } in
    defvars, norm_expr
  | Expr_fby (e1, e2) ->
    let defvars, norm_e1 = inject_expr node defvars e1 in
    let defvars, norm_e2 = inject_expr node defvars e2 in
    let norm_expr = { expr with expr_desc = Expr_fby (norm_e1, norm_e2) } in
    defvars, norm_expr
  | Expr_when (e, c, l) ->
    let defvars, norm_e = inject_expr node defvars e in
    let norm_expr = { expr with expr_desc = Expr_when (norm_e, c, l) } in
    defvars, norm_expr
  | Expr_ite (c, t, e) ->
    let defvars, norm_c = inject_expr node defvars c in
    let defvars, norm_t = inject_expr node defvars t in
    let defvars, norm_e = inject_expr node defvars e in
    let norm_expr = { expr with expr_desc = Expr_ite (norm_c, norm_t, norm_e) } in
    defvars, norm_expr
  | Expr_merge (c, hl) ->
    let defvars, norm_hl = inject_branches node defvars hl in
    let norm_expr = { expr with expr_desc = Expr_merge (c, norm_hl) } in
    defvars, norm_expr
in
(*Format.eprintf "inject_expr %B %a = %a@." alias Printers.pp_expr expr Printers.pp_expr (snd res);*)
res

and inject_branches node defvars hl =
 List.fold_right
   (fun (t, h) (defvars, norm_q) ->
     let (defvars, norm_h) = inject_expr node defvars h in
     defvars, (t, norm_h) :: norm_q
   )
   hl (defvars, [])


let rec inject_eq node defvars eq =
  let (defs', vars'), norm_rhs = inject_expr ~alias:false node defvars eq.eq_rhs in
  let norm_eq = { eq with eq_rhs = norm_rhs } in
  norm_eq::defs', vars'

(* let inject_eexpr ee =
 *   { ee with eexpr_qfexpr = inject_expr ee.eexpr_qfexpr }
 *   
 * let inject_spec s =
 *   { s with
 *     assume = List.map inject_eexpr s.assume;
 *     guarantees = List.map inject_eexpr s.guarantees;
 *     modes = List.map (fun m ->
 *                 { m with
 *                   require = List.map inject_eexpr m.require;
 *                   ensure = List.map inject_eexpr m.ensure
 *                 }
 *               ) s.modes
 *   } *)
  
(** normalize_node node returns a normalized node, 
    ie. 
    - updated locals
    - new equations
    - 
*)
let inject_node node = 
  cpt_fresh := 0;
  let inputs_outputs = node.node_inputs@node.node_outputs in
  let is_local v =
    List.for_all ((!=) v) inputs_outputs in
  let orig_vars = inputs_outputs@node.node_locals in
  let defs, vars =
    let eqs, auts = get_node_eqs node in
    if auts != [] then assert false; (* Automata should be expanded by now. *)
    List.fold_left (inject_eq node) ([], orig_vars) eqs in
  (* Normalize the asserts *)
  let vars, assert_defs, asserts = 
    List.fold_left (
    fun (vars, def_accu, assert_accu) assert_ ->
      let assert_expr = assert_.assert_expr in
      let (defs, vars'), expr = 
	inject_expr 
	  ~alias:false 
	  node 
	  ([], vars) (* defvar only contains vars *)
	  assert_expr
      in
      vars', defs@def_accu, {assert_ with assert_expr = expr}::assert_accu
    ) (vars, [], []) node.node_asserts in
  let new_locals = List.filter is_local vars in
  (* Compute traceability info: 
     - gather newly bound variables
     - compute the associated expression without aliases     
  *)
  (* let diff_vars = List.filter (fun v -> not (List.mem v node.node_locals)) new_locals in *)
  (* See comment below
   *  let spec = match node.node_spec with
   *   | None -> None
   *   | Some spec -> Some (inject_spec spec)
   * in *)
  let node =
  { node with 
    node_locals = new_locals; 
    node_stmts = List.map (fun eq -> Eq eq) (defs @ assert_defs);
    (* Incomplete work: TODO. Do we have to inject MPFR code here?
       Does it make sense for annotations? For me, only if we produce
       C code for annotations. Otherwise the various verification
       backend should have their own understanding, but would not
       necessarily require this additional normalization. *)
    (* 
       node_spec = spec;
       node_annot = List.map (fun ann -> {ann with
           annots = List.map (fun (ids, ee) -> ids, inject_eexpr ee) ann.annots}
         ) node.node_annot *)
  }
  in ((*Printers.pp_node Format.err_formatter node;*) node)

let inject_decl decl =
  match decl.top_decl_desc with
  | Node nd ->
    {decl with top_decl_desc = Node (inject_node nd)}
  | Open _ | ImportedNode _ | Const _ | TypeDef _ -> decl
  
let inject_prog decls = 
  List.map inject_decl decls


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
