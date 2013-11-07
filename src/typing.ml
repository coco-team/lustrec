(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
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

(** Main typing module. Classic inference algorithm with destructive
    unification. *)

let debug fmt args = () (* Format.eprintf "%a"  *)
(* Though it shares similarities with the clock calculus module, no code
    is shared.  Simple environments, very limited identifier scoping, no
    identifier redefinition allowed. *)

open Utils
(* Yes, opening both modules is dirty as some type names will be
   overwritten, yet this makes notations far lighter.*)
open LustreSpec
open Corelang
open Types
open Format

let pp_typing_env fmt env =
  Env.pp_env print_ty fmt env

(** [occurs tvar ty] returns true if the type variable [tvar] occurs in
    type [ty]. False otherwise. *)
let rec occurs tvar ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tvar -> ty=tvar
  | Tarrow (t1, t2) ->
      (occurs tvar t1) || (occurs tvar t2)
  | Ttuple tl ->
      List.exists (occurs tvar) tl
  | Tarray (_, t)
  | Tstatic (_, t)
  | Tclock t
  | Tlink t -> occurs tvar t
  | Tenum _ | Tconst _ | Tunivar | Tint | Treal | Tbool | Trat -> false

(** Promote monomorphic type variables to polymorphic type variables. *)
(* Generalize by side-effects *)
let rec generalize ty =
  match ty.tdesc with
  | Tvar ->
      (* No scopes, always generalize *)
      ty.tdesc <- Tunivar
  | Tarrow (t1,t2) ->
      generalize t1; generalize t2
  | Ttuple tlist ->
      List.iter generalize tlist
  | Tstatic (d, t)
  | Tarray (d, t) -> Dimension.generalize d; generalize t
  | Tclock t
  | Tlink t ->
      generalize t
  | Tenum _ | Tconst _ | Tunivar | Tint | Treal | Tbool | Trat -> ()

(** Downgrade polymorphic type variables to monomorphic type variables *)
let rec instantiate inst_vars inst_dim_vars ty =
  let ty = repr ty in
  match ty.tdesc with
  | Tenum _ | Tconst _ | Tvar | Tint | Treal | Tbool | Trat -> ty
  | Tarrow (t1,t2) ->
      {ty with tdesc =
       Tarrow ((instantiate inst_vars inst_dim_vars t1), (instantiate inst_vars inst_dim_vars t2))}
  | Ttuple tlist ->
      {ty with tdesc = Ttuple (List.map (instantiate inst_vars inst_dim_vars) tlist)}
  | Tclock t ->
	{ty with tdesc = Tclock (instantiate inst_vars inst_dim_vars t)}
  | Tstatic (d, t) ->
	{ty with tdesc = Tstatic (Dimension.instantiate inst_dim_vars d, instantiate inst_vars inst_dim_vars t)}
  | Tarray (d, t) ->
	{ty with tdesc = Tarray (Dimension.instantiate inst_dim_vars d, instantiate inst_vars inst_dim_vars t)}
  | Tlink t ->
	(* should not happen *)
	{ty with tdesc = Tlink (instantiate inst_vars inst_dim_vars t)}
  | Tunivar ->
      try
        List.assoc ty.tid !inst_vars
      with Not_found ->
        let var = new_var () in
	inst_vars := (ty.tid, var)::!inst_vars;
	var

(* [type_coretype cty] types the type declaration [cty] *)
let rec type_coretype type_dim cty =
  match (*get_repr_type*) cty with
  | Tydec_any -> new_var ()
  | Tydec_int -> Type_predef.type_int
  | Tydec_real -> Type_predef.type_real
  | Tydec_float -> Type_predef.type_real
  | Tydec_bool -> Type_predef.type_bool
  | Tydec_clock ty -> Type_predef.type_clock (type_coretype type_dim ty)
  | Tydec_const c -> Type_predef.type_const c
  | Tydec_enum tl -> Type_predef.type_enum tl
  | Tydec_array (d, ty) ->
    begin
      type_dim d;
      Type_predef.type_array d (type_coretype type_dim ty)
    end

(* [coretype_type is the reciprocal of [type_typecore] *)
let rec coretype_type ty =
 match (repr ty).tdesc with
 | Tvar           -> Tydec_any
 | Tint           -> Tydec_int
 | Treal          -> Tydec_real
 | Tbool          -> Tydec_bool
 | Tconst c       -> Tydec_const c
 | Tclock t       -> Tydec_clock (coretype_type t)
 | Tenum tl       -> Tydec_enum tl
 | Tarray (d, t)  -> Tydec_array (d, coretype_type t)
 | Tstatic (_, t) -> coretype_type t
 | _         -> assert false

let get_type_definition tname =
  try
    type_coretype (fun d -> ()) (Hashtbl.find type_table (Tydec_const tname))
  with Not_found -> raise (Error (Location.dummy_loc, Unbound_type tname))

(** [unify env t1 t2] unifies types [t1] and [t2]. Raises [Unify
    (t1,t2)] if the types are not unifiable.*)
(* Standard destructive unification *)
let rec unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1=t2 then
    ()
  else
    (* No type abbreviations resolution for now *)
    match t1.tdesc,t2.tdesc with
      (* This case is not mandory but will keep "older" types *)
    | Tvar, Tvar ->
        if t1.tid < t2.tid then
          t2.tdesc <- Tlink t1
        else
          t1.tdesc <- Tlink t2
    | (Tvar, _) when (not (occurs t1 t2)) ->
        t1.tdesc <- Tlink t2
    | (_,Tvar) when (not (occurs t2 t1)) ->
        t2.tdesc <- Tlink t1
    | Tarrow (t1,t2), Tarrow (t1',t2') ->
      begin
        unify t1 t1';
	unify t2 t2'
      end
    | Ttuple tlist1, Ttuple tlist2 ->
        if (List.length tlist1) <> (List.length tlist2) then
	  raise (Unify (t1, t2))
	else
          List.iter2 unify tlist1 tlist2
    | Tclock _, Tstatic _
    | Tstatic _, Tclock _ -> raise (Unify (t1, t2))
    | Tclock t1', _ -> unify t1' t2
    | _, Tclock t2' -> unify t1 t2'
    | Tint, Tint | Tbool, Tbool | Trat, Trat
    | Tunivar, _ | _, Tunivar -> ()
    | (Tconst t, _) ->
      let def_t = get_type_definition t in
      unify def_t t2
    | (_, Tconst t)  ->
      let def_t = get_type_definition t in
      unify t1 def_t
    | Tenum tl, Tenum tl' when tl == tl' -> ()
    | Tstatic (e1, t1'), Tstatic (e2, t2')
    | Tarray (e1, t1'), Tarray (e2, t2') ->
      begin
	unify t1' t2';
	Dimension.eval Basic_library.eval_env (fun c -> None) e1;
	Dimension.eval Basic_library.eval_env (fun c -> None) e2;
	Dimension.unify e1 e2;
      end
    | _,_ -> raise (Unify (t1, t2))

let try_unify ty1 ty2 loc =
  try
    unify ty1 ty2
  with
  | Unify _ ->
    raise (Error (loc, Type_clash (ty1,ty2)))
  | Dimension.Unify _ ->
    raise (Error (loc, Type_clash (ty1,ty2)))

let rec type_const loc c = 
  match c with
  | Const_int _ -> Type_predef.type_int
  | Const_real _ -> Type_predef.type_real
  | Const_float _ -> Type_predef.type_real
  | Const_array ca -> let d = Dimension.mkdim_int loc (List.length ca) in
		      let ty = new_var () in
		      List.iter (fun e -> try_unify (type_const loc e) ty loc) ca;
		      Type_predef.type_array d ty
  | Const_tag t  ->
    if Hashtbl.mem tag_table t
    then type_coretype (fun d -> ()) (Hashtbl.find tag_table t)
    else raise (Error (loc, Unbound_value ("enum tag " ^ t)))

(* The following typing functions take as parameter an environment [env]
   and whether the element being typed is expected to be constant [const]. 
   [env] is a pair composed of:
  - a map from ident to type, associating to each ident, i.e. 
    variables, constants and (imported) nodes, its type including whether
    it is constant or not. This latter information helps in checking constant 
    propagation policy in Lustre.
  - a vdecl list, in order to modify types of declared variables that are
    later discovered to be clocks during the typing process.
*)
let check_constant loc const_expected const_real =
  if const_expected && not const_real
  then raise (Error (loc, Not_a_constant))

let rec type_standard_args env in_main const expr_list =
  let ty_list = List.map (fun e -> dynamic_type (type_expr env in_main const e)) expr_list in
  let ty_res = new_var () in
  List.iter2 (fun e ty -> try_unify ty_res ty e.expr_loc) expr_list ty_list;
  ty_res

(* emulates a subtyping relation between types t and (d : t),
   used during node application only *)
and type_subtyping_arg env in_main ?(sub=true) const real_arg formal_type =
  let loc = real_arg.expr_loc in
  let const = const || (Types.get_static_value formal_type <> None) in
  let real_type = type_expr env in_main const real_arg in
  let real_type =
    if const
    then let d =
	   if is_dimension_type real_type
	   then dimension_of_expr real_arg
	   else Dimension.mkdim_var () in
	 let eval_const id = Types.get_static_value (Env.lookup_value (fst env) id) in
	 Dimension.eval Basic_library.eval_env eval_const d;
	 let real_static_type = Type_predef.type_static d (Types.dynamic_type real_type) in
	 (match Types.get_static_value real_type with
	 | None    -> ()
	 | Some d' -> try_unify real_type real_static_type loc);
	 real_static_type
    else real_type in
(*Format.eprintf "subtyping const %B real %a:%a vs formal %a@." const Printers.pp_expr real_arg Types.print_ty real_type Types.print_ty formal_type;*)
  match (repr real_type).tdesc, (repr formal_type).tdesc with
  | Tstatic _          , Tstatic _ when sub -> try_unify formal_type real_type loc
  | Tstatic (r_d, r_ty), _         when sub -> try_unify formal_type r_ty loc
  | _                                       -> try_unify formal_type real_type loc

and type_ident env in_main loc const id =
  type_expr env in_main const (expr_of_ident id loc)

(* typing an application implies:
   - checking that const formal parameters match real const (maybe symbolic) arguments
   - checking type adequation between formal and real arguments
*)
and type_appl env in_main loc const f args =
  let tfun = type_ident env in_main loc const f in
  let tins, touts = split_arrow tfun in
  let tins = type_list_of_type tins in
  let args = expr_list_of_expr args in
  List.iter2 (type_subtyping_arg env in_main const) args tins;
  touts

(** [type_expr env in_main expr] types expression [expr] in environment
    [env], expecting it to be [const] or not. *)
and type_expr env in_main const expr =
  let res = 
  match expr.expr_desc with
  | Expr_const c ->
    let ty = type_const expr.expr_loc c in
    let ty = Type_predef.type_static (Dimension.mkdim_var ()) ty in
    expr.expr_type <- ty;
    ty
  | Expr_ident v ->
    let tyv =
      try
        Env.lookup_value (fst env) v
      with Not_found ->
	Format.eprintf "Failure in typing expr %a@." Printers.pp_expr expr;
        raise (Error (expr.expr_loc, Unbound_value ("identifier " ^ v)))
    in
    let ty = instantiate (ref []) (ref []) tyv in
    let ty' =
      if const
      then Type_predef.type_static (Dimension.mkdim_var ()) (new_var ())
      else new_var () in
    try_unify ty ty' expr.expr_loc;
    expr.expr_type <- ty;
    ty 
  | Expr_array elist ->
    let ty_elt = type_standard_args env in_main const elist in
    let d = Dimension.mkdim_int expr.expr_loc (List.length elist) in
    let ty = Type_predef.type_array d ty_elt in
    expr.expr_type <- ty;
    ty
  | Expr_access (e1, d) ->
    type_subtyping_arg env in_main true (expr_of_dimension d) Type_predef.type_int;
    let ty_elt = new_var () in
    let d = Dimension.mkdim_var () in
    type_subtyping_arg env in_main const e1 (Type_predef.type_array d ty_elt);
    expr.expr_type <- ty_elt;
    ty_elt
  | Expr_power (e1, d) ->
    let eval_const id = Types.get_static_value (Env.lookup_value (fst env) id) in
    type_subtyping_arg env in_main true (expr_of_dimension d) Type_predef.type_int;
    Dimension.eval Basic_library.eval_env eval_const d;
    let ty_elt = type_standard_args env in_main const [e1] in
    let ty = Type_predef.type_array d ty_elt in
    expr.expr_type <- ty;
    ty
  | Expr_tuple elist ->
    let ty = new_ty (Ttuple (List.map (type_expr env in_main const) elist)) in
    expr.expr_type <- ty;
    ty
  | Expr_ite (c, t, e) ->
    type_subtyping_arg env in_main const c Type_predef.type_bool;
    let ty = type_standard_args env in_main const [t; e] in
    expr.expr_type <- ty;
    ty
  | Expr_appl (id, args, r) ->
    (* application of non internal function is not legal in a constant
       expression *)
    (match r with
    | None        -> ()
    | Some (x, l) -> 
      check_constant expr.expr_loc const false;
      let expr_x = expr_of_ident x expr.expr_loc in	
      let typ_l = 
	Type_predef.type_clock 
	  (type_const expr.expr_loc (Const_tag l)) in
      type_subtyping_arg env in_main ~sub:false const expr_x typ_l);
    let touts = type_appl env in_main expr.expr_loc const id args in
    expr.expr_type <- touts;
    touts
  | Expr_fby (e1,e2)
  | Expr_arrow (e1,e2) ->
    (* fby/arrow is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let ty = type_standard_args env in_main const [e1; e2] in
    expr.expr_type <- ty;
    ty
  | Expr_pre e ->
    (* pre is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let ty = type_standard_args env in_main const [e] in
    expr.expr_type <- ty;
    ty
  | Expr_when (e1,c,l) ->
    (* when is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let typ_l = Type_predef.type_clock (type_const expr.expr_loc (Const_tag l)) in
    let expr_c = expr_of_ident c expr.expr_loc in
    type_subtyping_arg env in_main ~sub:false const expr_c typ_l;
    update_clock env in_main c expr.expr_loc typ_l;
    let ty = type_standard_args env in_main const [e1] in
    expr.expr_type <- ty;
    ty
  | Expr_merge (c,hl) ->
    (* merge is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let typ_in, typ_out = type_branches env in_main expr.expr_loc const hl in
    let expr_c = expr_of_ident c expr.expr_loc in
    let typ_l = Type_predef.type_clock typ_in in
    type_subtyping_arg env in_main ~sub:false const expr_c typ_l;
    update_clock env in_main c expr.expr_loc typ_l;
    expr.expr_type <- typ_out;
    typ_out
  | Expr_uclock (e,k) | Expr_dclock (e,k) ->
      let ty = type_expr env in_main const e in
      expr.expr_type <- ty;
      ty
  | Expr_phclock (e,q) ->
      let ty = type_expr env in_main const e in
      expr.expr_type <- ty;
      ty
  in (*Format.eprintf "typing %B %a at %a = %a@." const Printers.pp_expr expr Location.pp_loc expr.expr_loc Types.print_ty res;*) res

and type_branches env in_main loc const hl =
  let typ_in = new_var () in
  let typ_out = new_var () in
  try
    let used_labels =
      List.fold_left (fun accu (t, h) ->
	unify typ_in (type_const loc (Const_tag t));
	type_subtyping_arg env in_main const h typ_out;
	if List.mem t accu
	then raise (Error (loc, Already_bound t))
	else t :: accu) [] hl in
    let type_labels = get_enum_type_tags (coretype_type typ_in) in
    if List.sort compare used_labels <> List.sort compare type_labels
    then let unbound_tag = List.find (fun t -> not (List.mem t used_labels)) type_labels in
	 raise (Error (loc, Unbound_value ("branching tag " ^ unbound_tag)))
    else (typ_in, typ_out)
  with Unify (t1, t2) ->
    raise (Error (loc, Type_clash (t1,t2)))

and update_clock env in_main id loc typ =
 (*Log.report ~level:1 (fun fmt -> Format.fprintf fmt "update_clock %s with %a@ " id print_ty typ);*)
 try
   let vdecl = List.find (fun v -> v.var_id = id) (snd env)
   in vdecl.var_type <- typ
 with
   Not_found ->
   raise (Error (loc, Unbound_value ("clock " ^ id)))

(** [type_eq env eq] types equation [eq] in environment [env] *)
let type_eq env in_main undefined_vars eq =
  (* Check undefined variables, type lhs *)
  let expr_lhs = expr_of_expr_list eq.eq_loc (List.map (fun v -> expr_of_ident v eq.eq_loc) eq.eq_lhs) in
  let ty_lhs = type_expr env in_main false expr_lhs in
  (* Check multiple variable definitions *)
  let define_var id uvars =
    try
      ignore(IMap.find id uvars);
      IMap.remove id uvars
    with Not_found ->
      raise (Error (eq.eq_loc, Already_defined id))
  in
  let undefined_vars =
    List.fold_left (fun uvars v -> define_var v uvars) undefined_vars eq.eq_lhs in
  (* Type rhs wrt to lhs type with subtyping, i.e. a constant rhs value may be assigned
     to a (always non-constant) lhs variable *)
  type_subtyping_arg env in_main false eq.eq_rhs ty_lhs;
  undefined_vars


(* [type_coreclock env ck id loc] types the type clock declaration [ck]
   in environment [env] *)
let type_coreclock env ck id loc =
  match ck.ck_dec_desc with
  | Ckdec_any | Ckdec_pclock (_,_) -> ()
  | Ckdec_bool cl ->
      let dummy_id_expr = expr_of_ident id loc in
      let when_expr =
        List.fold_left
          (fun expr (x, l) ->
                {expr_tag = new_tag ();
                 expr_desc= Expr_when (expr,x,l);
                 expr_type = new_var ();
                 expr_clock = Clocks.new_var true;
                 expr_delay = Delay.new_var ();
                 expr_loc=loc;
		 expr_annot = None})
          dummy_id_expr cl
      in
Format.eprintf "yiihii@.";
      ignore (type_expr env false false when_expr)

let rec check_type_declaration loc cty =
 match cty with
 | Tydec_clock ty
 | Tydec_array (_, ty) -> check_type_declaration loc ty
 | Tydec_const tname   ->
   if not (Hashtbl.mem type_table cty)
   then raise (Error (loc, Unbound_type tname));
 | _                   -> ()

let type_var_decl vd_env env vdecl =
  check_type_declaration vdecl.var_loc vdecl.var_dec_type.ty_dec_desc;
  let eval_const id = Types.get_static_value (Env.lookup_value env id) in
  let type_dim d =
    begin
      type_subtyping_arg (env, vd_env) false true (expr_of_dimension d) Type_predef.type_int;
      Dimension.eval Basic_library.eval_env eval_const d;
    end in
  let ty = type_coretype type_dim vdecl.var_dec_type.ty_dec_desc in
  let ty_status =
    if vdecl.var_dec_const
    then Type_predef.type_static (Dimension.mkdim_var ()) ty
    else ty in
  let new_env = Env.add_value env vdecl.var_id ty_status in
  type_coreclock (new_env,vd_env) vdecl.var_dec_clock vdecl.var_id vdecl.var_loc;
  vdecl.var_type <- ty_status;
  new_env

let type_var_decl_list vd_env env l =
  List.fold_left (type_var_decl vd_env) env l

let type_of_vlist vars =
  let tyl = List.map (fun v -> v.var_type) vars in
  type_of_type_list tyl

let add_vdecl vd_env vdecl =
 if List.exists (fun v -> v.var_id = vdecl.var_id) vd_env
 then raise (Error (vdecl.var_loc, Already_bound vdecl.var_id))
 else vdecl::vd_env

let check_vd_env vd_env =
  ignore (List.fold_left add_vdecl [] vd_env)

(** [type_node env nd loc] types node [nd] in environment env. The
    location is used for error reports. *)
let type_node env nd loc =
  let is_main = nd.node_id = !Options.main_node in
  let vd_env_ol = nd.node_outputs@nd.node_locals in
  let vd_env =  nd.node_inputs@vd_env_ol in
  check_vd_env vd_env;
  let init_env = env in
  let delta_env = type_var_decl_list vd_env init_env nd.node_inputs in
  let delta_env = type_var_decl_list vd_env delta_env nd.node_outputs in
  let delta_env = type_var_decl_list vd_env delta_env nd.node_locals in
  let new_env = Env.overwrite env delta_env in
  let undefined_vars_init =
    List.fold_left
      (fun uvs v -> IMap.add v.var_id () uvs)
      IMap.empty vd_env_ol in
  let undefined_vars =
    List.fold_left (type_eq (new_env, vd_env) is_main) undefined_vars_init nd.node_eqs
  in
  (* check that table is empty *)
  if (not (IMap.is_empty undefined_vars)) then
    raise (Error (loc, Undefined_var undefined_vars));
  let ty_ins = type_of_vlist nd.node_inputs in
  let ty_outs = type_of_vlist nd.node_outputs in
  let ty_node = new_ty (Tarrow (ty_ins,ty_outs)) in
  generalize ty_node;
  (* TODO ? Check that no node in the hierarchy remains polymorphic ? *)
  nd.node_type <- ty_node;
  Env.add_value env nd.node_id ty_node

let type_imported_node env nd loc =
  let new_env = type_var_decl_list nd.nodei_inputs env nd.nodei_inputs in
  let vd_env = nd.nodei_inputs@nd.nodei_outputs in
  check_vd_env vd_env;
  ignore(type_var_decl_list vd_env new_env nd.nodei_outputs);
  let ty_ins = type_of_vlist nd.nodei_inputs in
  let ty_outs = type_of_vlist nd.nodei_outputs in
  let ty_node = new_ty (Tarrow (ty_ins,ty_outs)) in
  generalize ty_node;
(*
  if (is_polymorphic ty_node) then
    raise (Error (loc, Poly_imported_node nd.nodei_id));
*)
  let new_env = Env.add_value env nd.nodei_id ty_node in
  nd.nodei_type <- ty_node;
  new_env

let type_imported_fun env nd loc =
  let new_env = type_var_decl_list nd.fun_inputs env nd.fun_inputs in
  let vd_env =  nd.fun_inputs@nd.fun_outputs in
  check_vd_env vd_env;
  ignore(type_var_decl_list vd_env new_env nd.fun_outputs);
  let ty_ins = type_of_vlist nd.fun_inputs in
  let ty_outs = type_of_vlist nd.fun_outputs in
  let ty_node = new_ty (Tarrow (ty_ins,ty_outs)) in
  generalize ty_node;
(*
  if (is_polymorphic ty_node) then
    raise (Error (loc, Poly_imported_node nd.fun_id));
*)
  let new_env = Env.add_value env nd.fun_id ty_node in
  nd.fun_type <- ty_node;
  new_env

let type_top_consts env clist =
  List.fold_left (fun env cdecl ->
    let ty = type_const cdecl.const_loc cdecl.const_value in
    let d =
      if is_dimension_type ty
      then dimension_of_const cdecl.const_loc cdecl.const_value
      else Dimension.mkdim_var () in
    let ty = Type_predef.type_static d ty in
    let new_env = Env.add_value env cdecl.const_id ty in
    cdecl.const_type <- ty;
    new_env) env clist

let type_top_decl env decl =
  match decl.top_decl_desc with
  | Node nd ->
      type_node env nd decl.top_decl_loc
  | ImportedNode nd ->
      type_imported_node env nd decl.top_decl_loc
  | ImportedFun nd ->
      type_imported_fun env nd decl.top_decl_loc
  | Consts clist ->
      type_top_consts env clist
  | Open _  -> env

let type_prog env decls =
try
  List.fold_left type_top_decl env decls
with Failure _ as exc -> raise exc

(* Once the Lustre program is fully typed,
   we must get back to the original description of dimensions,
   with constant parameters, instead of unifiable internal variables. *)

(* The following functions aims at 'unevaluating' dimension expressions occuring in array types,
   i.e. replacing unifiable second_order variables with the original static parameters.
   Once restored in this formulation, dimensions may be meaningfully printed.
*)
(*
let uneval_vdecl_generics vdecl ty =
 if vdecl.var_dec_const
 then
   match get_static_value ty with
   | None   -> (Format.eprintf "internal error: %a@." Types.print_ty vdecl.var_type; assert false)
   | Some d -> Dimension.unify d (Dimension.mkdim_ident vdecl.var_loc vdecl.var_id)

let uneval_node_generics vdecls =
  let inst_typ_vars = ref [] in
  let inst_dim_vars = ref [] in
  let inst_ty_list = List.map (fun v -> instantiate inst_typ_vars inst_dim_vars v.var_type) vdecls in
  List.iter2 (fun v ty -> uneval_vdecl_generics v ty) vdecls inst_ty_list;
  List.iter2 (fun v ty -> generalize ty; v.var_type <- ty) vdecls inst_ty_list
*)
let uneval_vdecl_generics vdecl =
 if vdecl.var_dec_const
 then
   match get_static_value vdecl.var_type with
   | None   -> (Format.eprintf "internal error: %a@." Types.print_ty vdecl.var_type; assert false)
   | Some d -> Dimension.uneval vdecl.var_id d

let uneval_node_generics vdecls =
  List.iter uneval_vdecl_generics vdecls

let uneval_top_generics decl =
  match decl.top_decl_desc with
  | Node nd ->
      uneval_node_generics (nd.node_inputs @ nd.node_outputs)
  | ImportedNode nd ->
      uneval_node_generics (nd.nodei_inputs @ nd.nodei_outputs)
  | ImportedFun nd ->
      ()
  | Consts clist -> ()
  | Open _  -> ()

let uneval_prog_generics prog =
 List.iter uneval_top_generics prog

let check_env_compat declared computed =
  Env.iter declared (fun k decl_type_k -> 
    let computed_t = Env.lookup_value computed k in
    try_unify decl_type_k computed_t Location.dummy_loc
  ) 

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
