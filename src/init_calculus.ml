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

(* Though it shares similarities with the clock calculus module, no code
    is shared.  Simple environments, very limited identifier scoping, no
    identifier redefinition allowed. *)

open Utils
(* Yes, opening both modules is dirty as some type names will be
   overwritten, yet this makes notations far lighter.*)
open Corelang
open Init
open Format

(** [occurs tvar ty] returns true if the type variable [tvar] occurs in
    type [ty]. False otherwise. *)
let rec occurs tvar ty =
  let ty = repr ty in
  match ty.tdesc with
  | Ivar -> ty=tvar
  | Iarrow (t1, t2) ->
      (occurs tvar t1) || (occurs tvar t2)
  | Ituple tl ->
      List.exists (occurs tvar) tl
  | Ilink t -> occurs tvar t
  | Isucc t -> occurs tvar t
  | Iunivar -> false

(** Promote monomorphic type variables to polymorphic type variables. *)
(* Generalize by side-effects *)
let rec generalize ty =
  match ty.tdesc with
  | Ivar ->
      (* No scopes, always generalize *)
      ty.idesc <- Iunivar
  | Iarrow (t1,t2) ->
      generalize t1; generalize t2
  | Ituple tlist ->
      List.iter generalize tlist
  | Ilink t ->
      generalize t
  | Isucc t ->
      generalize t
  | Tunivar -> ()

(** Downgrade polymorphic type variables to monomorphic type variables *)
let rec instanciate inst_vars ty =
  let ty = repr ty in
  match ty.idesc with
  | Ivar -> ty
  | Iarrow (t1,t2) ->
      {ty with idesc =
       Iarrow ((instanciate inst_vars t1), (instanciate inst_vars t2))}
  | Ituple tlist ->
      {ty with idesc = Ituple (List.map (instanciate inst_vars) tlist)}
  | Isucc t ->
	(* should not happen *)
	{ty with idesc = Isucc (instanciate inst_vars t)}
  | Ilink t ->
	(* should not happen *)
	{ty with idesc = Ilink (instanciate inst_vars t)}
  | Iunivar ->
      try
        List.assoc ty.iid !inst_vars
      with Not_found ->
        let var = new_var () in
	inst_vars := (ty.iid, var)::!inst_vars;
	var

(** [unify env t1 t2] unifies types [t1] and [t2]. Raises [Unify
    (t1,t2)] if the types are not unifiable.*)

(* Standard destructive unification *)
(* may loop for omega types *)
let rec unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1=t2 then
    ()
  else
    (* No type abbreviations resolution for now *)
    match t1.idesc,t2.idesc with
      (* This case is not mandory but will keep "older" types *)
    | Ivar, Ivar ->
        if t1.iid < t2.iid then
          t2.idesc <- Ilink t1
        else
          t1.idesc <- Ilink t2
    | (Ivar, _) when (not (occurs t1 t2)) ->
        t1.idesc <- Ilink t2
    | (_,Ivar) when (not (occurs t2 t1)) ->
        t2.idesc <- Ilink t1
    | Isucc t1, Isucc t1' -> unify t1 t1'
    | Iarrow (t1,t2), Iarrow (t1',t2') ->
        unify t1 t1'; unify t2 t2'
    | Ituple tlist1, Ituple tlist2 ->
        if (List.length tlist1) <> (List.length tlist2) then
	  raise (Unify (t1, t2))
	else
          List.iter2 unify tlist1 tlist2
    | Iunivar,_ | _, Iunivar -> 
        ()
    | _,_ -> raise (Unify (t1, t2))

let init_of_const c = 
 Init_predef.init_zero

let rec init_expect env in_main expr ty =
  let texpr = init_expr env in_main expr in
  try
    unify texpr ty
  with Unify (t1,t2) ->
    raise (Error (expr.expr_loc, Init_clash (t1,t2)))

and init_ident env in_main id loc = 
  init_expr env in_main (expr_of_ident id loc)

(** [type_expr env in_main expr] types expression [expr] in environment
    [env]. *)
and init_expr env in_main expr =
  match expr.expr_desc with
  | Expr_const c ->
      let ty = init_of_const c in
      expr.expr_init <- ty;
      ty
  | Expr_ident v ->
      let tyv =
        try
          Env.lookup_value env v
        with Not_found ->
          raise (Error (expr.expr_loc, Unbound_value v))
      in
      let ty = instanciate (ref []) tyv in
      expr.expr_init <- ty;
      ty
  | Expr_tuple elist ->
      let ty = new_ty (Ttuple (List.map (type_expr env in_main) elist)) in
      expr.expr_init <- ty;
      ty
  | Expr_appl (id, args, r) ->
    (match r with
    | None   -> ()
    | Some x -> let expr_x = expr_of_ident x expr.expr_loc in
		init_expect env in_main expr_x Init_predef.init_zero);
      let tfun = type_ident env in_main id expr.expr_loc in
      let tins,touts= split_arrow tfun in
      type_expect env in_main args tins;
      expr.expr_type <- touts;
      touts
  | Expr_arrow (e1,e2) ->
      let ty = type_expr env in_main e1 in
      type_expect env in_main e2 ty;
      expr.expr_type <- ty;
      ty
  | Expr_fby (init,e) ->
      let ty = type_of_const init in
      type_expect env in_main e ty;
      expr.expr_type <- ty;
      ty
  | Expr_concat (hd,e) ->
      let ty = type_of_const hd in
      type_expect env in_main e ty;
      expr.expr_type <- ty;
      ty
  | Expr_tail e ->
      let ty = type_expr env in_main e in
      expr.expr_type <- ty;
      ty
  | Expr_pre e ->
      let ty = type_expr env in_main e in
      expr.expr_type <- ty;
      ty
  | Expr_when (e1,c) | Expr_whennot (e1,c) ->
      let expr_c = expr_of_ident c expr.expr_loc in
      type_expect env in_main expr_c Type_predef.type_bool;
      let tlarg = type_expr env in_main e1 in
      expr.expr_type <- tlarg;
      tlarg
  | Expr_merge (c,e2,e3) ->
      let expr_c = expr_of_ident c expr.expr_loc in
      type_expect env in_main expr_c Type_predef.type_bool;
      let te2 = type_expr env in_main e2 in
      type_expect env in_main e3 te2;
      expr.expr_type <- te2;
      te2
  | Expr_uclock (e,k) | Expr_dclock (e,k) ->
      let ty = type_expr env in_main e in
      expr.expr_type <- ty;
      ty
  | Expr_phclock (e,q) ->
      let ty = type_expr env in_main e in
      expr.expr_type <- ty;
      ty

(** [type_eq env eq] types equation [eq] in environment [env] *)
let type_eq env in_main undefined_vars eq =
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
  (* Type lhs *)
  let get_value_type id =
    try
      Env.lookup_value env id
    with Not_found ->
      raise (Error (eq.eq_loc, Unbound_value id))
  in
  let tyl_lhs = List.map get_value_type eq.eq_lhs in
  let ty_lhs = type_of_type_list tyl_lhs in
  (* Type rhs *) 
  type_expect env in_main eq.eq_rhs ty_lhs;
  undefined_vars

(* [type_coretype cty] types the type declaration [cty] *)
let type_coretype cty =
  match cty with
  | Tydec_any -> new_var ()
  | Tydec_int -> Type_predef.type_int
  | Tydec_real -> Type_predef.type_real
  | Tydec_float -> Type_predef.type_real
  | Tydec_bool -> Type_predef.type_bool
  | Tydec_clock -> Type_predef.type_clock

(* [type_coreclock env ck id loc] types the type clock declaration [ck]
   in environment [env] *)
let type_coreclock env ck id loc =
  match ck.ck_dec_desc with
  | Ckdec_any | Ckdec_pclock (_,_) -> ()
  | Ckdec_bool cl ->
      let dummy_id_expr = expr_of_ident id loc in
      let when_expr =
        List.fold_left
          (fun expr c ->
            match c with
            | Wtrue id ->
                {expr_tag = new_tag ();
                 expr_desc=Expr_when (expr,id);
                 expr_type = new_var ();
                 expr_clock = Clocks.new_var true;
                 expr_loc=loc}
            | Wfalse id ->
                {expr_tag = new_tag ();
                 expr_desc=Expr_whennot (expr,id);
                 expr_type = new_var ();
                 expr_clock = Clocks.new_var true;
                 expr_loc=loc})
          dummy_id_expr cl
      in
      ignore (type_expr env false when_expr)

let type_var_decl env vdecl =
  if (Env.exists_value env vdecl.var_id) then
    raise (Error (vdecl.var_loc,Already_bound vdecl.var_id))
  else
    let ty = type_coretype vdecl.var_dec_type.ty_dec_desc in
    let new_env = Env.add_value env vdecl.var_id ty in
    type_coreclock new_env vdecl.var_dec_clock vdecl.var_id vdecl.var_loc;
    vdecl.var_type <- ty;
    new_env

let type_var_decl_list env l =
  List.fold_left type_var_decl env l

let type_of_vlist vars =
  let tyl = List.map (fun v -> v.var_type) vars in
  type_of_type_list tyl
      
(** [type_node env nd loc] types node [nd] in environment env. The
    location is used for error reports. *)
let type_node env nd loc =
  let is_main = nd.node_id = !Options.main_node in
  let delta_env = type_var_decl_list IMap.empty nd.node_inputs in
  let delta_env = type_var_decl_list delta_env nd.node_outputs in
  let delta_env = type_var_decl_list delta_env nd.node_locals in
  let new_env = Env.overwrite env delta_env in
  let undefined_vars_init =
    List.fold_left
      (fun uvs v -> IMap.add v.var_id () uvs)
      IMap.empty (nd.node_outputs@nd.node_locals) in
  let undefined_vars =
    List.fold_left (type_eq new_env is_main) undefined_vars_init nd.node_eqs
  in
  (* check that table is empty *)
  if (not (IMap.is_empty undefined_vars)) then
    raise (Error (loc,Undefined_var undefined_vars));
  let ty_ins = type_of_vlist nd.node_inputs in
  let ty_outs = type_of_vlist nd.node_outputs in
  let ty_node = new_ty (Tarrow (ty_ins,ty_outs)) in
  generalize ty_node;
  (* TODO ? Check that no node in the hierarchy remains polymorphic ? *)
  nd.node_type <- ty_node;
  Env.add_value env nd.node_id ty_node

let type_imported_node env nd loc =
  let new_env = type_var_decl_list env nd.nodei_inputs in
  ignore(type_var_decl_list new_env nd.nodei_outputs);
  let ty_ins = type_of_vlist nd.nodei_inputs in
  let ty_outs = type_of_vlist nd.nodei_outputs in
  let ty_node = new_ty (Tarrow (ty_ins,ty_outs)) in
  generalize ty_node;
  if (is_polymorphic ty_node) then
    raise (Error (loc, Poly_imported_node nd.nodei_id));
  let new_env = Env.add_value env nd.nodei_id ty_node in
  nd.nodei_type <- ty_node;
  new_env

let type_top_decl env decl =
  match decl.top_decl_desc with
  | Node nd -> 
      type_node env nd decl.top_decl_loc
  | ImportedNode nd ->
      type_imported_node env nd decl.top_decl_loc
  | SensorDecl _ | ActuatorDecl _ | Consts _ -> env

let type_top_consts env decl =
  match decl.top_decl_desc with
    | Consts clist -> 
      List.fold_left (fun env (id, c) ->
	let ty = type_of_const c in
	Env.add_value env id ty       
      ) env clist
    | Node _ | ImportedNode _ | SensorDecl _ | ActuatorDecl _ -> env

let type_prog env decls =
  let new_env = List.fold_left type_top_consts env decls in
  ignore(List.fold_left type_top_decl new_env decls)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
