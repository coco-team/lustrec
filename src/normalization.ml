(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2013, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 * Copyright (C) 2012-2013, INPT, Toulouse, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

(* This module is used for the lustre to C compiler *)


open Utils
open LustreSpec
open Corelang
(* open Clocks *)
open Format

let expr_true loc ck =
{ expr_tag = Utils.new_tag ();
  expr_desc = Expr_const (Const_tag tag_true);
  expr_type = Type_predef.type_bool;
  expr_clock = ck;
  expr_delay = Delay.new_var ();
  expr_annot = None;
  expr_loc = loc }

let expr_false loc ck =
{ expr_tag = Utils.new_tag ();
  expr_desc = Expr_const (Const_tag tag_false);
  expr_type = Type_predef.type_bool;
  expr_clock = ck;
  expr_delay = Delay.new_var ();
  expr_annot = None;
  expr_loc = loc }

let expr_once loc ck =
 { expr_tag = Utils.new_tag ();
  expr_desc = Expr_arrow (expr_true loc ck, expr_false loc ck);
  expr_type = Type_predef.type_bool;
  expr_clock = ck;
  expr_delay = Delay.new_var ();
  expr_annot = None;
  expr_loc = loc }

let is_expr_once =
  let dummy_expr_once = expr_once Location.dummy_loc (Clocks.new_var true) in
  fun expr -> Corelang.is_eq_expr expr dummy_expr_once

let unfold_arrow expr =
 match expr.expr_desc with
 | Expr_arrow (e1, e2) ->
    let loc = expr.expr_loc in
    let ck = List.hd (Clocks.clock_list_of_clock expr.expr_clock) in
    { expr with expr_desc = Expr_ite (expr_once loc ck, e1, e2) }
 | _                   -> assert false

let unfold_arrow_active = ref true 
let cpt_fresh = ref 0

(* Generate a new local [node] variable *)
let mk_fresh_var node loc ty ck =
  let vars = get_node_vars node in
  let rec aux () =
  incr cpt_fresh;
  let s = Printf.sprintf "__%s_%d" node.node_id !cpt_fresh in
  if List.exists (fun v -> v.var_id = s) vars then aux () else
  {
    var_id = s;
    var_dec_type = dummy_type_dec;
    var_dec_clock = dummy_clock_dec;
    var_dec_const = false;
    var_type = ty;
    var_clock = ck;
    var_loc = loc
  }
  in aux ()

(* Generate a new ident expression from a declared variable *)
let mk_ident_expr v =
  { expr_tag = new_tag ();
    expr_desc = Expr_ident v.var_id;
    expr_type = v.var_type;
    expr_clock = v.var_clock;
    expr_delay = Delay.new_var ();
    expr_annot = None;
    expr_loc = v.var_loc }

(* Get the equation in [defs] with [expr] as rhs, if any *)
let get_expr_alias defs expr =
 try Some (List.find (fun eq -> is_eq_expr eq.eq_rhs expr) defs)
 with
   Not_found -> None

(* Replace [expr] with (tuple of) [locals] *)
let replace_expr locals expr =
 match locals with
 | []  -> assert false
 | [v] -> { expr with
   expr_tag = Utils.new_tag ();
   expr_desc = Expr_ident v.var_id }
 | _   -> { expr with
   expr_tag = Utils.new_tag ();
   expr_desc = Expr_tuple (List.map mk_ident_expr locals) }

let unfold_offsets e offsets =
  let add_offset e d =
(*Format.eprintf "add_offset %a %a@." Dimension.pp_dimension (Types.array_type_dimension e.expr_type) Dimension.pp_dimension d;*)
    { e with
      expr_tag = Utils.new_tag ();
      expr_loc = d.Dimension.dim_loc;
      expr_type = Types.array_element_type e.expr_type;
      expr_desc = Expr_access (e, d) } in
 List.fold_left add_offset e offsets

(* Create an alias for [expr], if none exists yet *)
let mk_expr_alias node (defs, vars) expr =
(*Format.eprintf "mk_expr_alias %a %a %a@." Printers.pp_expr expr Types.print_ty expr.expr_type Clocks.print_ck expr.expr_clock;*)
  match get_expr_alias defs expr with
  | Some eq ->
    let aliases = List.map (fun id -> List.find (fun v -> v.var_id = id) vars) eq.eq_lhs in
    (defs, vars), replace_expr aliases expr
  | None    ->
    let new_aliases =
      List.map2
	(mk_fresh_var node expr.expr_loc)
	(Types.type_list_of_type expr.expr_type)
	(Clocks.clock_list_of_clock expr.expr_clock) in
    let new_def =
      mkeq expr.expr_loc (List.map (fun v -> v.var_id) new_aliases, expr)
    in (new_def::defs, new_aliases@vars), replace_expr new_aliases expr

(* Create an alias for [expr], if [expr] is not already an alias (i.e. an ident)
   and [opt] is true *)
let mk_expr_alias_opt opt node defvars expr =
  match expr.expr_desc with
  | Expr_ident alias ->
    defvars, expr
  | _                -> 
    if opt
    then
      mk_expr_alias node defvars expr
    else
      defvars, expr

(* Create a (normalized) expression from [ref_e], 
   replacing description with [norm_d],
   taking propagated [offsets] into account 
   in order to change expression type *)
let mk_norm_expr offsets ref_e norm_d =
  let drop_array_type ty =
    Types.map_tuple_type Types.array_element_type ty in
  { ref_e with
    expr_desc = norm_d;
    expr_type = Utils.repeat (List.length offsets) drop_array_type ref_e.expr_type }

(* normalize_<foo> : defs * used vars -> <foo> -> (updated defs * updated vars) * normalized <foo> *)
let rec normalize_list alias node offsets norm_element defvars elist =
  List.fold_right
    (fun t (defvars, qlist) ->
      let defvars, norm_t = norm_element alias node offsets defvars t in
      (defvars, norm_t :: qlist)
    ) elist (defvars, [])

let rec normalize_expr ?(alias=true) node offsets defvars expr =
(*  Format.eprintf "normalize %B %a [%a]@." alias Printers.pp_expr expr (Utils.fprintf_list ~sep:"," Dimension.pp_dimension) offsets;*)
  match expr.expr_desc with
  | Expr_const _ 
  | Expr_ident _ -> defvars, unfold_offsets expr offsets
  | Expr_array elist ->
    let defvars, norm_elist = normalize_list alias node offsets (fun _ -> normalize_array_expr ~alias:true) defvars elist in
    let norm_expr = mk_norm_expr offsets expr (Expr_array norm_elist) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_power (e1, d) when offsets = [] ->
    let defvars, norm_e1 = normalize_expr node offsets defvars e1 in
    let norm_expr = mk_norm_expr offsets expr (Expr_power (norm_e1, d)) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_power (e1, d) ->
    normalize_expr ~alias:alias node (List.tl offsets) defvars e1
  | Expr_access (e1, d) ->
    normalize_expr ~alias:alias node (d::offsets) defvars e1
  | Expr_tuple elist -> 
    let defvars, norm_elist =
      normalize_list alias node offsets (fun alias -> normalize_expr ~alias:alias) defvars elist in
    defvars, mk_norm_expr offsets expr (Expr_tuple norm_elist)
  | Expr_appl (id, args, None) 
      when Basic_library.is_internal_fun id 
	&& Types.is_array_type expr.expr_type ->
    let defvars, norm_args = 
      normalize_list 
	alias
	node
	offsets 
	(fun _ -> normalize_array_expr ~alias:true) 
	defvars 
	(expr_list_of_expr args) 
    in
    defvars, mk_norm_expr offsets expr (Expr_appl (id, expr_of_expr_list args.expr_loc norm_args, None))
  | Expr_appl (id, args, None) when Basic_library.is_internal_fun id ->
    let defvars, norm_args = normalize_expr ~alias:true node offsets defvars args in
    defvars, mk_norm_expr offsets expr (Expr_appl (id, norm_args, None))
  | Expr_appl (id, args, r) ->
    let defvars, norm_args = normalize_expr node [] defvars args in
    let norm_expr = mk_norm_expr [] expr (Expr_appl (id, norm_args, r)) in
    if offsets <> []
    then
      let defvars, norm_expr = normalize_expr node [] defvars norm_expr in
      normalize_expr ~alias:alias node offsets defvars norm_expr
    else
      mk_expr_alias_opt (alias && not (Basic_library.is_internal_fun id)) node defvars norm_expr
  | Expr_arrow (e1,e2) when !unfold_arrow_active && not (is_expr_once expr) -> (* Here we differ from Colaco paper: arrows are pushed to the top *)
    normalize_expr ~alias:alias node offsets defvars (unfold_arrow expr)
  | Expr_arrow (e1,e2) ->
    let defvars, norm_e1 = normalize_expr node offsets defvars e1 in
    let defvars, norm_e2 = normalize_expr node offsets defvars e2 in
    let norm_expr = mk_norm_expr offsets expr (Expr_arrow (norm_e1, norm_e2)) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_pre e ->
    let defvars, norm_e = normalize_expr node offsets defvars e in
    let norm_expr = mk_norm_expr offsets expr (Expr_pre norm_e) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_fby (e1, e2) ->
    let defvars, norm_e1 = normalize_expr node offsets defvars e1 in
    let defvars, norm_e2 = normalize_expr node offsets defvars e2 in
    let norm_expr = mk_norm_expr offsets expr (Expr_fby (norm_e1, norm_e2)) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_when (e, c, l) ->
    let defvars, norm_e = normalize_expr node offsets defvars e in
    defvars, mk_norm_expr offsets expr (Expr_when (norm_e, c, l))
  | Expr_ite (c, t, e) ->
    let defvars, norm_c = normalize_guard node defvars c in
    let defvars, norm_t = normalize_cond_expr  node offsets defvars t in
    let defvars, norm_e = normalize_cond_expr  node offsets defvars e in
    let norm_expr = mk_norm_expr offsets expr (Expr_ite (norm_c, norm_t, norm_e)) in
    mk_expr_alias_opt alias node defvars norm_expr
  | Expr_merge (c, hl) ->
    let defvars, norm_hl = normalize_branches node offsets defvars hl in
    let norm_expr = mk_norm_expr offsets expr (Expr_merge (c, norm_hl)) in
    mk_expr_alias_opt alias node defvars norm_expr
  
(* Creates a conditional with a merge construct, which is more lazy *)
(*
let norm_conditional_as_merge alias node norm_expr offsets defvars expr =
 match expr.expr_desc with
 | Expr_ite (c, t, e) ->
   let defvars, norm_t = norm_expr (alias node offsets defvars t in
 | _ -> assert false
*)
and normalize_branches node offsets defvars hl =
 List.fold_right
   (fun (t, h) (defvars, norm_q) ->
     let (defvars, norm_h) = normalize_cond_expr node offsets defvars h in
     defvars, (t, norm_h) :: norm_q
   )
   hl (defvars, [])

and normalize_array_expr ?(alias=true) node offsets defvars expr =
(*  Format.eprintf "normalize_array %B %a [%a]@." alias Printers.pp_expr expr (Utils.fprintf_list ~sep:"," Dimension.pp_dimension) offsets;*)
  match expr.expr_desc with
  | Expr_power (e1, d) when offsets = [] ->
    let defvars, norm_e1 = normalize_expr node offsets defvars e1 in
    defvars, mk_norm_expr offsets expr (Expr_power (norm_e1, d))
  | Expr_power (e1, d) ->
    normalize_array_expr ~alias:alias node (List.tl offsets) defvars e1
  | Expr_access (e1, d) -> normalize_array_expr ~alias:alias node (d::offsets) defvars e1
  | Expr_array elist when offsets = [] ->
    let defvars, norm_elist = normalize_list alias node offsets (fun _ -> normalize_array_expr ~alias:true) defvars elist in
    defvars, mk_norm_expr offsets expr (Expr_array norm_elist)
  | Expr_appl (id, args, None) when Basic_library.is_internal_fun id ->
    let defvars, norm_args = normalize_list alias node offsets (fun _ -> normalize_array_expr ~alias:true) defvars (expr_list_of_expr args) in
    defvars, mk_norm_expr offsets expr (Expr_appl (id, expr_of_expr_list args.expr_loc norm_args, None))
  |  _ -> normalize_expr ~alias:alias node offsets defvars expr

and normalize_cond_expr ?(alias=true) node offsets defvars expr =
  (*Format.eprintf "normalize_cond %B %a [%a]@." alias Printers.pp_expr expr (Utils.fprintf_list ~sep:"," Dimension.pp_dimension) offsets;*)
  match expr.expr_desc with
  | Expr_access (e1, d) ->
    normalize_cond_expr ~alias:alias node (d::offsets) defvars e1
  | Expr_ite (c, t, e) ->
    let defvars, norm_c = normalize_guard node defvars c in
    let defvars, norm_t = normalize_cond_expr node offsets defvars t in
    let defvars, norm_e = normalize_cond_expr node offsets defvars e in
    defvars, mk_norm_expr offsets expr (Expr_ite (norm_c, norm_t, norm_e))
  | Expr_merge (c, hl) ->
    let defvars, norm_hl = normalize_branches node offsets defvars hl in
    defvars, mk_norm_expr offsets expr (Expr_merge (c, norm_hl))
  | _ -> normalize_expr ~alias:alias node offsets defvars expr

and normalize_guard node defvars expr =
  let defvars, norm_expr = normalize_expr node [] defvars expr in
  mk_expr_alias_opt true node defvars norm_expr

(* outputs cannot be memories as well. If so, introduce new local variable.
*)
let decouple_outputs node defvars eq =
  let rec fold_lhs defvars lhs tys cks =
   match lhs, tys, cks with
   | [], [], []          -> defvars, []
   | v::qv, t::qt, c::qc -> let (defs_q, vars_q), lhs_q = fold_lhs defvars qv qt qc in
			    if List.exists (fun o -> o.var_id = v) node.node_outputs
			    then
			      let newvar = mk_fresh_var node eq.eq_loc t c in
			      let neweq  = mkeq eq.eq_loc ([v], mk_ident_expr newvar) in
			      (neweq :: defs_q, newvar :: vars_q), newvar.var_id :: lhs_q
			    else
			      (defs_q, vars_q), v::lhs_q
   | _                   -> assert false in
  let defvars', lhs' =
    fold_lhs
      defvars
      eq.eq_lhs
      (Types.type_list_of_type eq.eq_rhs.expr_type)
      (Clocks.clock_list_of_clock eq.eq_rhs.expr_clock) in
  defvars', {eq with eq_lhs = lhs' }

let rec normalize_eq node defvars eq = 
  match eq.eq_rhs.expr_desc with
  | Expr_pre _
  | Expr_fby _  ->
    let (defvars', eq') = decouple_outputs node defvars eq in
    let (defs', vars'), norm_rhs = normalize_expr ~alias:false node [] defvars' eq'.eq_rhs in
    let norm_eq = { eq' with eq_rhs = norm_rhs } in
    (norm_eq::defs', vars')
  | Expr_array _ ->
    let (defs', vars'), norm_rhs = normalize_array_expr ~alias:false node [] defvars eq.eq_rhs in
    let norm_eq = { eq with eq_rhs = norm_rhs } in
    (norm_eq::defs', vars')
  | Expr_appl (id, _, None) when Basic_library.is_internal_fun id && Types.is_array_type eq.eq_rhs.expr_type ->
    let (defs', vars'), norm_rhs = normalize_array_expr ~alias:false node [] defvars eq.eq_rhs in
    let norm_eq = { eq with eq_rhs = norm_rhs } in
    (norm_eq::defs', vars')
  | Expr_appl _ ->
    let (defs', vars'), norm_rhs = normalize_expr ~alias:false node [] defvars eq.eq_rhs in
    let norm_eq = { eq with eq_rhs = norm_rhs } in
    (norm_eq::defs', vars')
  | _ ->
    let (defs', vars'), norm_rhs = normalize_cond_expr ~alias:false node [] defvars eq.eq_rhs in
    let norm_eq = { eq with eq_rhs = norm_rhs } in
    norm_eq::defs', vars'

(** normalize_node node returns a normalized node, 
    ie. 
    - updated locals
    - new equations
    - 
*)
let normalize_node node = 
  cpt_fresh := 0;
  let inputs_outputs = node.node_inputs@node.node_outputs in
  let is_local v =
    List.for_all ((!=) v) inputs_outputs in
  let orig_vars = inputs_outputs@node.node_locals in
  let defs, vars = 
    List.fold_left (normalize_eq node) ([], orig_vars) node.node_eqs in
  (* Normalize the asserts *)
  let vars, assert_defs, asserts = 
    List.fold_left (
    fun (vars, def_accu, assert_accu) assert_ ->
      let assert_expr = assert_.assert_expr in
      let (defs, vars'), expr = 
	normalize_expr 
	  ~alias:false 
	  node 
	  [] (* empty offset for arrays *)
	  ([], vars) (* defvar only contains vars *)
	  assert_expr
      in
      vars', defs@def_accu, {assert_ with assert_expr = expr}::assert_accu
    ) (vars, [], []) node.node_asserts in
  let new_locals = List.filter is_local vars in
  (* Compute tracebaility info: 
     - gather newly bound variables
     - compute the associated expression without aliases     
  *)
  let diff_vars = List.filter (fun v -> not (List.mem v node.node_locals) ) new_locals in
  let norm_traceability = {
    annots =
      List.map 
	(fun v -> 
	  let expr = substitute_expr diff_vars defs (
	    let eq = try
		       List.find (fun eq -> eq.eq_lhs = [v.var_id]) defs 
	      with Not_found -> (Format.eprintf "var not found %s@." v.var_id; assert false) in
	    eq.eq_rhs) in 
	  let pair = mkeexpr expr.expr_loc (mkexpr expr.expr_loc (Expr_tuple [expr_of_ident v.var_id expr.expr_loc; expr])) in 
	  ["horn_backend";"trace"], pair 
	)
    diff_vars ;
    annot_loc = Location.dummy_loc
  }

  in
  let node =
  { node with 
    node_locals = new_locals; 
    node_eqs = defs @ assert_defs;
    node_asserts = asserts;
    node_annot = norm_traceability::node.node_annot;
  }
  in ((*Printers.pp_node Format.err_formatter node;*) node)

let normalize_decl decl =
  match decl.top_decl_desc with
  | Node nd ->
    {decl with top_decl_desc = Node (normalize_node nd)}
  | Open _ | ImportedNode _ | Consts _ -> decl
  
let normalize_prog decls = 
  List.map normalize_decl decls

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
