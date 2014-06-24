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

open Format

type dim_expr =
  {mutable dim_desc: dim_desc;
   dim_loc: Location.t;
   dim_id: int}

and dim_desc =
| Dbool of bool
| Dint  of int
| Dident of Utils.ident
| Dappl of Utils.ident * dim_expr list
| Dite of dim_expr * dim_expr * dim_expr
| Dlink of dim_expr
| Dvar
| Dunivar

exception Unify of dim_expr * dim_expr
exception InvalidDimension

let new_id = ref (-1)

let mkdim loc dim =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = dim;}

let mkdim_var () =
  incr new_id;
  { dim_loc = Location.dummy_loc;
    dim_id = !new_id;
    dim_desc = Dvar;}

let mkdim_ident loc id =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = Dident id;}

let mkdim_bool loc b =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = Dbool b;}

let mkdim_int loc i =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = Dint i;}

let mkdim_appl loc f args =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = Dappl (f, args);}

let mkdim_ite loc i t e =
  incr new_id;
  { dim_loc = loc;
    dim_id = !new_id;
    dim_desc = Dite (i, t, e);}

let rec pp_dimension fmt dim =
(*fprintf fmt "<%d>" (Obj.magic dim: int);*)
 match dim.dim_desc with
 | Dident id       ->
     fprintf fmt "%s" id
 | Dint i          ->
     fprintf fmt "%d" i
 | Dbool b         ->
     fprintf fmt "%B" b
 | Dite (i, t, e)  ->
     fprintf fmt "if %a then %a else %a"
       pp_dimension i pp_dimension t pp_dimension e
 | Dappl (f, [arg]) ->
     fprintf fmt "(%s%a)" f pp_dimension arg
 | Dappl (f, [arg1; arg2]) ->
     fprintf fmt "(%a%s%a)" pp_dimension arg1 f pp_dimension arg2
 | Dappl (_, _) -> assert false
 | Dlink dim' -> fprintf fmt "%a" pp_dimension dim'
 | Dvar       -> fprintf fmt "_%s" (Utils.name_of_dimension dim.dim_id)
 | Dunivar    -> fprintf fmt "'%s" (Utils.name_of_dimension dim.dim_id)

let rec multi_dimension_product loc dim_list =
 match dim_list with
 | []   -> mkdim_int loc 1
 | [d]  -> d
 | d::q -> mkdim_appl loc "*" [d; multi_dimension_product loc q]

(* Builds a dimension expr representing 0<=d *)
let check_bound loc d =
 mkdim_appl loc "<=" [mkdim_int loc 0; d]

(* Builds a dimension expr representing 0<=i<d *)
let check_access loc d i =
 mkdim_appl loc "&&"
   [mkdim_appl loc "<=" [mkdim_int loc 0; i];
    mkdim_appl loc "<"  [i; d]]

let rec repr dim =
 match dim.dim_desc with
 | Dlink dim' -> repr dim'
 | _          -> dim

let rec is_eq_dimension d1 d2 =
  let d1 = repr d1 in
  let d2 = repr d2 in
  d1.dim_id = d2.dim_id ||
  match d1.dim_desc, d2.dim_desc with
  | Dappl (f1, args1), Dappl (f2, args2) ->
    f1 = f2 && List.length args1 = List.length args2 && List.for_all2 is_eq_dimension args1 args2
  | Dite (c1, t1, e1), Dite (c2, t2, e2) ->
    is_eq_dimension c1 c2 && is_eq_dimension t1 t2 && is_eq_dimension e1 e2
  | Dvar, _
  | _, Dvar
  | Dunivar, _
  | _, Dunivar -> false
  | _ -> d1 = d2

let is_dimension_const dim =
 match (repr dim).dim_desc with
 | Dint _
 | Dbool _ -> true
 | _       -> false

let size_const_dimension dim =
  match (repr dim).dim_desc with
 | Dint i  -> i
 | Dbool b -> if b then 1 else 0
 | _       -> (Format.eprintf "internal error: size_const_dimension %a@." pp_dimension dim; assert false)

let rec is_polymorphic dim =
  match dim.dim_desc with
  | Dident _
  | Dint _
  | Dbool _
  | Dvar             -> false
  | Dite (i, t, e)   ->
      is_polymorphic i || is_polymorphic t || is_polymorphic e
  | Dappl (_, args) -> List.exists is_polymorphic args
  | Dlink dim' -> is_polymorphic dim'
  | Dunivar    -> true

(* Normalizes a dimension expression, i.e. canonicalize all polynomial
   sub-expressions, where unsupported operations (eg. '/') are treated
   as variables.
*)

let rec factors dim =
  match dim.dim_desc with
  | Dappl (f, args) when f = "*" -> List.flatten (List.map factors args)
  | _                            -> [dim]

let rec factors_constant fs =
  match fs with
  | []   -> 1
  | f::q ->
    match f.dim_desc with
    | Dint i -> i * (factors_constant q)
    | _      -> factors_constant q

let norm_factors fs =
  let k = factors_constant fs in
  let nk = List.filter (fun d -> not (is_dimension_const d)) fs in
  (k, List.sort Pervasives.compare nk)

let rec terms dim =
 match dim.dim_desc with
 | Dappl (f, args) when f = "+" -> List.flatten (List.map terms args)
 | _                            -> [dim]

let rec normalize dim =
 dim
(*
let rec unnormalize loc l =
  let l = List.sort (fun (k, l) (k', l') -> compare l l') (List.map (fun (k, l) -> (k, List.sort compare l)) l) in
  match l with
  | []   -> mkdim_int loc 0
  | t::q -> 
 List.fold_left (fun res (k, l) -> mkdim_appl loc "+" res (mkdim_appl loc "*" (mkdim_int loc k) l)) t q
*)
let copy copy_dim_vars dim =
  let rec cp dim =
  match dim.dim_desc with
  | Dbool _
  | Dint _    -> dim
  | Dident id -> mkdim_ident dim.dim_loc id
  | Dite (c, t, e) -> mkdim_ite dim.dim_loc (cp c) (cp t) (cp e)
  | Dappl (id, args) -> mkdim_appl dim.dim_loc id (List.map cp args)
  | Dlink dim' -> cp dim'
  | Dunivar -> assert false
  | Dvar      ->
    try
      List.assoc dim.dim_id !copy_dim_vars
    with Not_found ->
      let var = mkdim dim.dim_loc Dvar in
      copy_dim_vars := (dim.dim_id, var)::!copy_dim_vars;
      var
  in cp dim

(* Partially evaluates a 'simple' dimension expr [dim], i.e. an expr containing only int and bool 
   constructs, with conditionals. [eval_const] is a typing environment for static values. [eval_op] is an evaluation env for basic operators. The argument [dim] is modified in-place. 
*)
let rec eval eval_op eval_const dim =
  match dim.dim_desc with
  | Dbool _
  | Dint _    -> ()
  | Dident id ->
    (match eval_const id with
    | Some val_dim -> dim.dim_desc <- Dlink val_dim
    | None         -> raise InvalidDimension)
  | Dite (c, t, e) ->
    begin
      eval eval_op eval_const c;
      eval eval_op eval_const t;
      eval eval_op eval_const e;
      match (repr c).dim_desc with
      | Dbool b -> dim.dim_desc <- Dlink (if b then t else e)
      | _       -> ()
       end
  | Dappl (id, args) ->
    begin
      List.iter (eval eval_op eval_const) args;
      if List.for_all is_dimension_const args
      then dim.dim_desc <- Env.lookup_value eval_op id (List.map (fun d -> (repr d).dim_desc) args)
    end
  | Dlink dim' ->
    begin
      eval eval_op eval_const dim';
      dim.dim_desc <- Dlink (repr dim')
    end
  | Dvar -> ()
  | Dunivar -> assert false

let uneval const univar =
  let univar = repr univar in
  match univar.dim_desc with
  | Dunivar -> univar.dim_desc <- Dident const
  | _       -> assert false

(** [occurs dvar dim] returns true if the dimension variable [dvar] occurs in
    dimension expression [dim]. False otherwise. *)
let rec occurs dvar dim =
  let dim = repr dim in
  match dim.dim_desc with
  | Dvar  -> dim.dim_id = dvar.dim_id
  | Dident _
  | Dint _
  | Dbool _
  | Dunivar          -> false
  | Dite (i, t, e)   ->
      occurs dvar i || occurs dvar t || occurs dvar e
  | Dappl (_, args) -> List.exists (occurs dvar) args
  | Dlink _ -> assert false

(* Promote monomorphic dimension variables to polymorphic variables.
   Generalize by side-effects *)
let rec generalize dim =
  match dim.dim_desc with
  | Dvar -> dim.dim_desc <- Dunivar
  | Dident _
  | Dint _
  | Dbool _
  | Dunivar          -> ()
  | Dite (i, t, e)   ->
      generalize i; generalize t; generalize e
  | Dappl (_, args) -> List.iter generalize args
  | Dlink dim' -> generalize dim'

(* Instantiate polymorphic dimension variables to monomorphic variables.
   Also duplicates the whole term structure (but the constant sub-terms).
*)
let rec instantiate inst_dim_vars dim =
  let dim = repr dim in
  match dim.dim_desc with
  | Dvar _
  | Dident _
  | Dint _
  | Dbool _ -> dim
  | Dite (i, t, e)   ->
      mkdim_ite dim.dim_loc
	(instantiate inst_dim_vars i)
	(instantiate inst_dim_vars t)
	(instantiate inst_dim_vars e)
  | Dappl (f, args) -> mkdim_appl dim.dim_loc f (List.map (instantiate inst_dim_vars) args)
  | Dlink dim' -> assert false (*mkdim dim.dim_loc (Dlink (instantiate inst_dim_vars dim'))*)
  | Dunivar ->
      try
        List.assoc dim.dim_id !inst_dim_vars
      with Not_found ->
        let var = mkdim dim.dim_loc Dvar in
	inst_dim_vars := (dim.dim_id, var)::!inst_dim_vars;
	var

(** destructive unification of [dim1] and [dim2].
   Raises [Unify (t1,t2)] if the types are not unifiable.
   if [semi] unification is required,
   [dim1] should furthermore be an instance of [dim2] *)
let unify ?(semi=false) dim1 dim2 =
  let rec unif dim1 dim2 =
    let dim1 = repr dim1 in
    let dim2 = repr dim2 in
    if dim1.dim_id = dim2.dim_id then () else
      match dim1.dim_desc, dim2.dim_desc with
      | Dunivar, _
      | _      , Dunivar -> assert false
      | Dvar   , Dvar    ->
	if dim1.dim_id < dim2.dim_id
	then dim2.dim_desc <- Dlink dim1
	else dim1.dim_desc <- Dlink dim2
      | Dvar   , _ when (not semi) && not (occurs dim1 dim2) ->
	dim1.dim_desc <- Dlink dim2
      | _      , Dvar when not (occurs dim2 dim1) ->
	dim2.dim_desc <- Dlink dim1
      | Dite(i1, t1, e1), Dite(i2, t2, e2) ->
	begin
          unif i1 i2;
	  unif t1 t2;
	  unif e1 e2
	end
      | Dappl(f1, args1), Dappl(f2, args2) when f1 = f2 && List.length args1 = List.length args2 ->
	List.iter2 unif args1 args2
      | Dbool b1, Dbool b2 when b1 = b2 -> ()
      | Dint i1 , Dint i2 when i1 = i2 -> ()
      | Dident id1, Dident id2 when id1 = id2 -> ()
      | _ -> raise (Unify (dim1, dim2))
  in unif dim1 dim2

let rec expr_replace_var fvar e = 
 { e with dim_desc = expr_replace_desc fvar e.dim_desc }
and expr_replace_desc fvar e =
  let re = expr_replace_var fvar in
  match e with
  | Dvar
  | Dunivar
  | Dbool _
  | Dint _ -> e
  | Dident v -> Dident (fvar v)
  | Dappl (id, el) -> Dappl (id, List.map re el)
  | Dite (g,t,e) -> Dite (re g, re t, re e)
  | Dlink e -> Dlink (re e)
