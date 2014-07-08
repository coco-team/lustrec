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
  | Tstruct fl ->
     List.exists (fun (f, t) -> occurs tvar t) fl
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
  | Ttuple tl ->
     List.iter generalize tl
  | Tstruct fl ->
     List.iter (fun (f, t) -> generalize t) fl
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
  | Tstruct flist ->
      {ty with tdesc = Tstruct (List.map (fun (f, t) -> (f, instantiate inst_vars inst_dim_vars t)) flist)}
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
  | Tydec_struct fl -> Type_predef.type_struct (List.map (fun (f, ty) -> (f, type_coretype type_dim ty)) fl)
  | Tydec_array (d, ty) ->
    begin
      type_dim d;
      Type_predef.type_array d (type_coretype type_dim ty)
    end

(* [coretype_type] is the reciprocal of [type_typecore] *)
let rec coretype_type ty =
 match (repr ty).tdesc with
 | Tvar           -> Tydec_any
 | Tint           -> Tydec_int
 | Treal          -> Tydec_real
 | Tbool          -> Tydec_bool
 | Tconst c       -> Tydec_const c
 | Tclock t       -> Tydec_clock (coretype_type t)
 | Tenum tl       -> Tydec_enum tl
 | Tstruct fl     -> Tydec_struct (List.map (fun (f, t) -> (f, coretype_type t)) fl)
 | Tarray (d, t)  -> Tydec_array (d, coretype_type t)
 | Tstatic (_, t) -> coretype_type t
 | _         -> assert false

let get_type_definition tname =
  try
    type_coretype (fun d -> ()) (Hashtbl.find type_table (Tydec_const tname))
  with Not_found -> raise (Error (Location.dummy_loc, Unbound_type tname))

(* Equality on ground types only *)
(* Should be used between local variables which must have a ground type *)
let rec eq_ground t1 t2 =
  match t1.tdesc, t2.tdesc with
  | Tint, Tint | Tbool, Tbool | Trat, Trat | Treal, Treal -> true
  | Tenum tl, Tenum tl' when tl == tl' -> true
  | Ttuple tl, Ttuple tl' when List.length tl = List.length tl' -> List.for_all2 eq_ground tl tl'
  | Tstruct fl, Tstruct fl' when List.map fst fl = List.map fst fl' -> List.for_all2 (fun (_, t) (_, t') -> eq_ground t t') fl fl'
  | (Tconst t, _) ->
    let def_t = get_type_definition t in
    eq_ground def_t t2
  | (_, Tconst t)  ->
    let def_t = get_type_definition t in
    eq_ground t1 def_t
  | Tarrow (t1,t2), Tarrow (t1',t2') -> eq_ground t1 t1' && eq_ground t2 t2'
  | Tclock t1', Tclock t2' -> eq_ground t1' t2'
  | Tstatic (e1, t1'), Tstatic (e2, t2')
  | Tarray (e1, t1'), Tarray (e2, t2') -> Dimension.is_eq_dimension e1 e2 && eq_ground t1' t2'
  | _ -> false

(** [unify t1 t2] unifies types [t1] and [t2]
    using standard destructive unification.
    Raises [Unify (t1,t2)] if the types are not unifiable.
    [t1] is a expected/formal/spec type, [t2] is a computed/real/implem type,
    so in case of unification error: expected type [t1], got type [t2].
    If [sub]-typing is allowed, [t2] may be a subtype of [t1].
    If [semi] unification is required,
    [t1] should furthermore be an instance of [t2]
    and constants are handled differently.*)
let unify ?(sub=false) ?(semi=false) t1 t2 =
  let rec unif t1 t2 =
    let t1 = repr t1 in
    let t2 = repr t2 in
    if t1==t2 then
      ()
    else
      match t1.tdesc,t2.tdesc with
      (* strictly subtyping cases first *)
      | _ , Tclock t2 when sub && (get_clock_base_type t1 = None) ->
	unif t1 t2
      | _ , Tstatic (d2, t2) when sub && (get_static_value t1 = None) ->
	unif t1 t2
      (* This case is not mandatory but will keep "older" types *)
      | Tvar, Tvar ->
        if t1.tid < t2.tid then
          t2.tdesc <- Tlink t1
        else
          t1.tdesc <- Tlink t2
      | Tvar, _ when (not semi) && (not (occurs t1 t2)) ->
        t1.tdesc <- Tlink t2
      | _, Tvar when (not (occurs t2 t1)) ->
        t2.tdesc <- Tlink t1
      | Tarrow (t1,t2), Tarrow (t1',t2') ->
	begin
          unif t2 t2';
	  unif t1' t1
	end
      | Ttuple tl, Ttuple tl' when List.length tl = List.length tl' ->
	List.iter2 unif tl tl'
      | Ttuple [t1]        , _                  -> unif t1 t2
      | _                  , Ttuple [t2]        -> unif t1 t2
      | Tstruct fl, Tstruct fl' when List.map fst fl = List.map fst fl' ->
	List.iter2 (fun (_, t) (_, t') -> unif t t') fl fl'
      | Tclock _, Tstatic _
      | Tstatic _, Tclock _ -> raise (Unify (t1, t2))
      | Tclock t1', Tclock t2' -> unif t1' t2'
      | Tint, Tint | Tbool, Tbool | Trat, Trat | Treal, Treal
      | Tunivar, _ | _, Tunivar -> ()
      | (Tconst t, _) ->
	let def_t = get_type_definition t in
	unif def_t t2
      | (_, Tconst t)  ->
	let def_t = get_type_definition t in
	unif t1 def_t
      | Tenum tl, Tenum tl' when tl == tl' -> ()
      | Tstatic (e1, t1'), Tstatic (e2, t2')
      | Tarray (e1, t1'), Tarray (e2, t2') ->
	let eval_const =
	  if semi
	  then (fun c -> Some (Dimension.mkdim_ident Location.dummy_loc c))
	  else (fun c -> None) in
	begin
	  unif t1' t2';
	  Dimension.eval Basic_library.eval_env eval_const e1;
	  Dimension.eval Basic_library.eval_env eval_const e2;
	  Dimension.unify ~semi:semi e1 e2;
	end
      | _,_ -> raise (Unify (t1, t2))
  in unif t1 t2

(* Expected type ty1, got type ty2 *)
let try_unify ?(sub=false) ?(semi=false) ty1 ty2 loc =
  try
    unify ~sub:sub ~semi:semi ty1 ty2
  with
  | Unify _ ->
    raise (Error (loc, Type_clash (ty1,ty2)))
  | Dimension.Unify _ ->
    raise (Error (loc, Type_clash (ty1,ty2)))

let rec type_struct_const_field loc (label, c) =
  if Hashtbl.mem field_table label
  then let tydec = Hashtbl.find field_table label in
       let tydec_struct = get_struct_type_fields tydec in
       let ty_label = type_coretype (fun d -> ()) (List.assoc label tydec_struct) in
       begin
	 try_unify ty_label (type_const loc c) loc;
	 type_coretype (fun d -> ()) tydec
       end
  else raise (Error (loc, Unbound_value ("struct field " ^ label)))

and type_const loc c = 
  match c with
  | Const_int _     -> Type_predef.type_int
  | Const_real _    -> Type_predef.type_real
  | Const_float _   -> Type_predef.type_real
  | Const_array ca  -> let d = Dimension.mkdim_int loc (List.length ca) in
		      let ty = new_var () in
		      List.iter (fun e -> try_unify ty (type_const loc e) loc) ca;
		      Type_predef.type_array d ty
  | Const_tag t     ->
    if Hashtbl.mem tag_table t
    then type_coretype (fun d -> ()) (Hashtbl.find tag_table t)
    else raise (Error (loc, Unbound_value ("enum tag " ^ t)))
  | Const_struct fl ->
    let ty_struct = new_var () in
    begin
      let used =
	List.fold_left
	  (fun acc (l, c) ->
	    if List.mem l acc
	    then raise (Error (loc, Already_bound ("struct field " ^ l)))
	    else try_unify ty_struct (type_struct_const_field loc (l, c)) loc; l::acc)
	  [] fl in
      try
	let total = List.map fst (get_struct_type_fields (coretype_type ty_struct)) in
(*	List.iter (fun l -> Format.eprintf "total: %s@." l) total;
	List.iter (fun l -> Format.eprintf "used: %s@." l) used; *)
	let undef = List.find (fun l -> not (List.mem l used)) total
	in raise (Error (loc, Unbound_value ("struct field " ^ undef)))
      with Not_found -> 
	ty_struct
    end
  | Const_string _ -> assert false (* string should only appear in annotations *)

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

let rec type_add_const env const arg targ =
  if const
  then let d =
	 if is_dimension_type targ
	 then dimension_of_expr arg
	 else Dimension.mkdim_var () in
       let eval_const id = Types.get_static_value (Env.lookup_value (fst env) id) in
       Dimension.eval Basic_library.eval_env eval_const d;
       let real_static_type = Type_predef.type_static d (Types.dynamic_type targ) in
       (match Types.get_static_value targ with
       | None    -> ()
       | Some d' -> try_unify targ real_static_type arg.expr_loc);
       real_static_type
  else targ

(* emulates a subtyping relation between types t and (d : t),
   used during node applications and assignments *)
and type_subtyping_arg env in_main ?(sub=true) const real_arg formal_type =
  let loc = real_arg.expr_loc in
  let const = const || (Types.get_static_value formal_type <> None) in
  let real_type = type_add_const env const real_arg (type_expr env in_main const real_arg) in
  (*Format.eprintf "subtyping const %B real %a:%a vs formal %a@." const Printers.pp_expr real_arg Types.print_ty real_type Types.print_ty formal_type;*)
  try_unify ~sub:sub formal_type real_type loc

and type_ident env in_main loc const id =
  type_expr env in_main const (expr_of_ident id loc)

(* typing an application implies:
   - checking that const formal parameters match real const (maybe symbolic) arguments
   - checking type adequation between formal and real arguments
   An application may embed an homomorphic/internal function, in which case we need to split
   it in many calls
*)
and type_appl env in_main loc const f args =
  let targs = List.map (type_expr env in_main const) args in
  if Basic_library.is_internal_fun f && List.exists is_tuple_type targs
  then
    try
      let targs = Utils.transpose_list (List.map type_list_of_type targs) in
      Types.type_of_type_list (List.map (type_simple_call env in_main loc const f) targs)
    with
      Utils.TransposeError (l, l') -> raise (Error (loc, WrongMorphism (l, l')))
  else
    type_dependent_call env in_main loc const f (List.combine args targs)

(* type a call with possible dependent types. [targs] is here a list of (argument, type) pairs. *)
and type_dependent_call env in_main loc const f targs =
  let tins, touts = new_var (), new_var () in
  let tfun = Type_predef.type_arrow tins touts in
  type_subtyping_arg env in_main const (expr_of_ident f loc) tfun;
  let tins = type_list_of_type tins in
  if List.length targs <> List.length tins then
    raise (Error (loc, WrongArity (List.length tins, List.length targs)))
  else
    begin
      List.iter2 (fun (a,t) ti ->
	let t' = type_add_const env (const || Types.get_static_value ti <> None) a t
	in try_unify ~sub:true ti t' a.expr_loc) targs tins;
      touts
    end

(* type a simple call without dependent types 
   but possible homomorphic extension.
   [targs] is here a list of arguments' types. *)
and type_simple_call env in_main loc const f targs =
  let tins, touts = new_var (), new_var () in
  let tfun = Type_predef.type_arrow tins touts in
  type_subtyping_arg env in_main const (expr_of_ident f loc) tfun;
  (*Format.eprintf "try unify %a %a@." Types.print_ty tins Types.print_ty (type_of_type_list targs);*)
  try_unify ~sub:true tins (type_of_type_list targs) loc;
  touts

(** [type_expr env in_main expr] types expression [expr] in environment
    [env], expecting it to be [const] or not. *)
and type_expr env in_main const expr =
  let resulting_ty = 
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
    let ty_elt = new_var () in
    List.iter (fun e -> try_unify ty_elt (type_appl env in_main expr.expr_loc const "uminus" [e]) e.expr_loc) elist;
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
    let ty_elt = type_appl env in_main expr.expr_loc const "uminus" [e1] in
    let ty = Type_predef.type_array d ty_elt in
    expr.expr_type <- ty;
    ty
  | Expr_tuple elist ->
    let ty = new_ty (Ttuple (List.map (type_expr env in_main const) elist)) in
    expr.expr_type <- ty;
    ty
  | Expr_ite (c, t, e) ->
    type_subtyping_arg env in_main const c Type_predef.type_bool;
    let ty = type_appl env in_main expr.expr_loc const "+" [t; e] in
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
    let touts = type_appl env in_main expr.expr_loc const id (expr_list_of_expr args) in
    expr.expr_type <- touts;
    touts
  | Expr_fby (e1,e2)
  | Expr_arrow (e1,e2) ->
    (* fby/arrow is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let ty = type_appl env in_main expr.expr_loc const "+" [e1; e2] in
    expr.expr_type <- ty;
    ty
  | Expr_pre e ->
    (* pre is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let ty = type_appl env in_main expr.expr_loc const "uminus" [e] in
    expr.expr_type <- ty;
    ty
  | Expr_when (e1,c,l) ->
    (* when is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let typ_l = Type_predef.type_clock (type_const expr.expr_loc (Const_tag l)) in
    let expr_c = expr_of_ident c expr.expr_loc in
    type_subtyping_arg env in_main ~sub:false const expr_c typ_l;
    let ty = type_appl env in_main expr.expr_loc const "uminus" [e1] in
    expr.expr_type <- ty;
    ty
  | Expr_merge (c,hl) ->
    (* merge is not legal in a constant expression *)
    check_constant expr.expr_loc const false;
    let typ_in, typ_out = type_branches env in_main expr.expr_loc const hl in
    let expr_c = expr_of_ident c expr.expr_loc in
    let typ_l = Type_predef.type_clock typ_in in
    type_subtyping_arg env in_main ~sub:false const expr_c typ_l;
    expr.expr_type <- typ_out;
    typ_out
  in 
  Log.report ~level:3 (fun fmt -> Format.fprintf fmt "Type of expr %a: %a@." Printers.pp_expr expr Types.print_ty resulting_ty);
  resulting_ty

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
  (* check assignment of declared constant, assignment of clock *)
  let ty_lhs =
    type_of_type_list
      (List.map2 (fun ty id ->
	if get_static_value ty <> None
	then raise (Error (eq.eq_loc, Assigned_constant id)) else
	match get_clock_base_type ty with
	| None -> ty
	| Some ty -> ty)
	 (type_list_of_type ty_lhs) eq.eq_lhs) in
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
  (* Typing asserts *)
  List.iter (fun assert_ ->
    let assert_expr =  assert_.assert_expr in
    type_subtyping_arg (new_env, vd_env) is_main false assert_expr Type_predef.type_bool
  )  nd.node_asserts;
  
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
  | Node nd -> (
      try
	type_node env nd decl.top_decl_loc
      with Error (loc, err) as exc -> (
	if !Options.global_inline then
	  Format.eprintf "Type error: failing node@.%a@.@?"
	    Printers.pp_node nd
	;
	raise exc)
  )
  | ImportedNode nd ->
      type_imported_node env nd decl.top_decl_loc
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
  | Consts clist -> ()
  | Open _  -> ()

let uneval_prog_generics prog =
 List.iter uneval_top_generics prog

let rec get_imported_node decls id =
  match decls with
  | [] -> assert false
  | decl::q ->
     (match decl.top_decl_desc with
      | ImportedNode nd when id = nd.nodei_id -> decl
      | _ -> get_imported_node q id)

let check_env_compat header declared computed = 
  uneval_prog_generics header;
  Env.iter declared (fun k decl_type_k -> 
    let computed_t = instantiate (ref []) (ref []) 
				 (try Env.lookup_value computed k
				  with Not_found ->
				    let loc = (get_imported_node header k).top_decl_loc in 
				    raise (Error (loc, Declared_but_undefined k))) in
    (*Types.print_ty Format.std_formatter decl_type_k;
    Types.print_ty Format.std_formatter computed_t;*)
    try_unify ~sub:true ~semi:true decl_type_k computed_t Location.dummy_loc
		    )

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
