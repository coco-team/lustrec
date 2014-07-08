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

(** Main clock-calculus module. Based on type inference algorithms with
  destructive unification. Uses a bit of subtyping for periodic clocks. *)

(* Though it shares similarities with the typing module, no code
    is shared.  Simple environments, very limited identifier scoping, no
    identifier redefinition allowed. *)
open Utils
open LustreSpec
open Corelang
open Clocks
open Format

let loc_of_cond loc_containing id =
  let pos_start =
    {loc_containing.Location.loc_end with 
     Lexing.pos_cnum=loc_containing.Location.loc_end.Lexing.pos_cnum-(String.length id)}
  in
  {Location.loc_start = pos_start;
   Location.loc_end = loc_containing.Location.loc_end}

(** [occurs cvar ck] returns true if the clock variable [cvar] occurs in
    clock [ck]. False otherwise. *)
let rec occurs cvar ck =
  let ck = repr ck in
  match ck.cdesc with
  | Carrow (ck1, ck2) ->
      (occurs cvar ck1) || (occurs cvar ck2)
  | Ctuple ckl ->
      List.exists (occurs cvar) ckl
  | Con (ck',_,_) -> occurs cvar ck'
  | Pck_up (ck',_) -> occurs cvar ck'
  | Pck_down (ck',_) -> occurs cvar ck'
  | Pck_phase (ck',_) -> occurs cvar ck'
  | Cvar _ -> ck=cvar
  | Cunivar _ | Pck_const (_,_) -> false
  | Clink ck' -> occurs cvar ck'
  | Ccarrying (_,ck') -> occurs cvar ck'

(* Clocks generalization *)
let rec generalize_carrier cr =
  match cr.carrier_desc with
  | Carry_const _
  | Carry_name ->
      if cr.carrier_scoped then
        raise (Scope_carrier cr);
      cr.carrier_desc <- Carry_var
  | Carry_var -> ()
  | Carry_link cr' -> generalize_carrier cr'

(** Promote monomorphic clock variables to polymorphic clock variables. *)
(* Generalize by side-effects *)
let rec generalize ck =
    match ck.cdesc with
    | Carrow (ck1,ck2) ->
        generalize ck1; generalize ck2
    | Ctuple clist ->
        List.iter generalize clist
    | Con (ck',cr,_) -> generalize ck'; generalize_carrier cr
    | Cvar cset ->
        if ck.cscoped then
          raise (Scope_clock ck);
        ck.cdesc <- Cunivar cset
    | Pck_up (ck',_) -> generalize ck'
    | Pck_down (ck',_) -> generalize ck'
    | Pck_phase (ck',_) -> generalize ck'
    | Pck_const (_,_) | Cunivar _ -> ()
    | Clink ck' ->
        generalize ck'
    | Ccarrying (cr,ck') ->
        generalize_carrier cr; generalize ck'

let try_generalize ck_node loc =
  try 
    generalize ck_node
  with (Scope_carrier cr) ->
    raise (Error (loc, Carrier_extrusion (ck_node, cr)))
  | (Scope_clock ck) ->
    raise (Error (loc, Clock_extrusion (ck_node, ck)))

(* Clocks instanciation *)
let instantiate_carrier cr inst_cr_vars =
  let cr = carrier_repr cr in
  match cr.carrier_desc with
  | Carry_const _
  | Carry_name -> cr
  | Carry_link _ ->
      failwith "Internal error"
  | Carry_var ->
      try
        List.assoc cr.carrier_id !inst_cr_vars
      with Not_found ->            
        let cr_var = new_carrier Carry_name true in
        inst_cr_vars := (cr.carrier_id,cr_var)::!inst_cr_vars;
        cr_var

(** Downgrade polymorphic clock variables to monomorphic clock variables *)
(* inst_ck_vars ensures that a polymorphic variable is instanciated with
   the same monomorphic variable if it occurs several times in the same
   type. inst_cr_vars is the same for carriers. *)
let rec instantiate inst_ck_vars inst_cr_vars ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      {ck with cdesc =
       Carrow ((instantiate inst_ck_vars inst_cr_vars ck1),
               (instantiate inst_ck_vars inst_cr_vars ck2))}
  | Ctuple cklist ->
      {ck with cdesc = Ctuple 
         (List.map (instantiate inst_ck_vars inst_cr_vars) cklist)}
  | Con (ck',c,l) ->
      let c' = instantiate_carrier c inst_cr_vars in
      {ck with cdesc = Con ((instantiate inst_ck_vars inst_cr_vars ck'),c',l)}
  | Cvar _ | Pck_const (_,_) -> ck
  | Pck_up (ck',k) ->
      {ck with cdesc = Pck_up ((instantiate inst_ck_vars inst_cr_vars ck'),k)}
  | Pck_down (ck',k) ->
      {ck with cdesc = Pck_down ((instantiate inst_ck_vars inst_cr_vars ck'),k)}
  | Pck_phase (ck',q) ->
      {ck with cdesc = Pck_phase ((instantiate inst_ck_vars inst_cr_vars ck'),q)}
  | Clink ck' ->
      {ck with cdesc = Clink (instantiate inst_ck_vars inst_cr_vars ck')}
  | Ccarrying (cr,ck') ->
      let cr' = instantiate_carrier cr inst_cr_vars in
        {ck with cdesc =
         Ccarrying (cr',instantiate inst_ck_vars inst_cr_vars ck')}
  | Cunivar cset ->
      try
        List.assoc ck.cid !inst_ck_vars
      with Not_found ->
        let var = new_ck (Cvar cset) true in
	inst_ck_vars := (ck.cid, var)::!inst_ck_vars;
	var

(** [subsume pck1 cset1] subsumes clock [pck1] by clock subset
    [cset1]. The clock constraint is actually recursively transfered to
    the clock variable appearing in [pck1] *)
let subsume pck1 cset1 =
  let rec aux pck cset =
    match cset with
    | CSet_all ->
        ()
    | CSet_pck (k,(a,b)) ->
        match pck.cdesc with
        | Cvar cset' ->
            pck.cdesc <- Cvar (intersect cset' cset)
        | Pck_up (pck',k') ->
            let rat = if a=0 then (0,1) else (a,b*k') in
            aux pck' (CSet_pck ((k*k'),rat))
        | Pck_down (pck',k') ->
            let k''=k/(gcd k k') in
            aux pck' (CSet_pck (k'',(a*k',b)))
        | Pck_phase (pck',(a',b')) ->
            let (a'',b'')= max_rat (sum_rat (a,b) (-a',b')) (0,1) in
            aux pck' (CSet_pck (k, (a'',b'')))
        | Pck_const (n,(a',b')) ->
            if n mod k <> 0 || (max_rat (a,b) (a',b')) <> (a',b') then
              raise (Subsume (pck1, cset1))
        | Clink pck' ->
            aux pck' cset
        | Cunivar _ -> ()
        | Ccarrying (_,ck') ->
            aux ck' cset
        | _ -> raise (Subsume (pck1, cset1))
  in
  aux pck1 cset1

let rec update_scope_carrier scoped cr =
  if (not scoped) then
    begin
      cr.carrier_scoped <- scoped;
      match cr.carrier_desc with
	| Carry_const _ | Carry_name | Carry_var -> ()
      | Carry_link cr' -> update_scope_carrier scoped cr'
    end

let rec update_scope scoped ck =
  if (not scoped) then
    begin
      ck.cscoped <- scoped;
      match ck.cdesc with
      | Carrow (ck1,ck2) ->
          update_scope scoped ck1; update_scope scoped ck2
      | Ctuple clist ->
          List.iter (update_scope scoped) clist
      | Con (ck',cr,_) -> update_scope scoped ck'(*; update_scope_carrier scoped cr*)
      | Cvar cset ->
          ck.cdesc <- Cvar cset
      | Pck_up (ck',_) -> update_scope scoped ck'
      | Pck_down (ck',_) -> update_scope scoped ck'
      | Pck_phase (ck',_) -> update_scope scoped ck'
      | Pck_const (_,_) | Cunivar _ -> ()
      | Clink ck' ->
          update_scope scoped ck'
      | Ccarrying (cr,ck') ->
          update_scope_carrier scoped cr; update_scope scoped ck'
    end

(* Unifies two static pclocks. *)
let unify_static_pck ck1 ck2 =
  if (period ck1 <> period ck2) || (phase ck1 <> phase ck2) then
    raise (Unify (ck1,ck2))

(* Unifies two clock carriers *)
let unify_carrier cr1 cr2 =
  let cr1 = carrier_repr cr1 in
  let cr2 = carrier_repr cr2 in
  if cr1==cr2 then ()
  else
    match cr1.carrier_desc, cr2.carrier_desc with
    | Carry_const id1, Carry_const id2 ->
      if id1 <> id2 then raise (Mismatch (cr1, cr2))
    | Carry_const _, Carry_name ->
      begin
	cr2.carrier_desc <- Carry_link cr1;
	update_scope_carrier cr2.carrier_scoped cr1
      end
    | Carry_name, Carry_const _ ->
      begin
        cr1.carrier_desc <- Carry_link cr2;
        update_scope_carrier cr1.carrier_scoped cr2
      end
    | Carry_name, Carry_name ->
      if cr1.carrier_id < cr2.carrier_id then
        begin
          cr2.carrier_desc <- Carry_link cr1;
          update_scope_carrier cr2.carrier_scoped cr1
        end
      else
        begin
          cr1.carrier_desc <- Carry_link cr2;
          update_scope_carrier cr1.carrier_scoped cr2
        end
    | _,_ -> assert false

(* Semi-unifies two clock carriers *)
let semi_unify_carrier cr1 cr2 =
  let cr1 = carrier_repr cr1 in
  let cr2 = carrier_repr cr2 in
  if cr1==cr2 then ()
  else
    match cr1.carrier_desc, cr2.carrier_desc with
    | Carry_const id1, Carry_const id2 ->
      if id1 <> id2 then raise (Mismatch (cr1, cr2))
    | Carry_const _, Carry_name ->
      begin
	cr2.carrier_desc <- Carry_link cr1;
	update_scope_carrier cr2.carrier_scoped cr1
      end
    | Carry_name, Carry_const _ -> raise (Mismatch (cr1, cr2))
    | Carry_name, Carry_name ->
      if cr1.carrier_id < cr2.carrier_id then
        begin
          cr2.carrier_desc <- Carry_link cr1;
          update_scope_carrier cr2.carrier_scoped cr1
        end
      else
        begin
          cr1.carrier_desc <- Carry_link cr2;
          update_scope_carrier cr1.carrier_scoped cr2
        end
    | _,_ -> assert false

(** [unify ck1 ck2] unifies clocks [ck1] and [ck2]. Raises [Unify
    (ck1,ck2)] if the clocks are not unifiable.*)
let rec unify ck1 ck2 =
  let ck1 = repr ck1 in
  let ck2 = repr ck2 in
  if ck1==ck2 then
    ()
  else
    let left_const = is_concrete_pck ck1 in
    let right_const = is_concrete_pck ck2 in
    if left_const && right_const then
      unify_static_pck ck1 ck2
    else
      match ck1.cdesc,ck2.cdesc with
      | Cvar cset1,Cvar cset2->
          if ck1.cid < ck2.cid then
            begin
              ck2.cdesc <- Clink (simplify ck1);
              update_scope ck2.cscoped ck1;
              subsume ck1 cset2
            end
          else
            begin
              ck1.cdesc <- Clink (simplify ck2);
              update_scope ck1.cscoped ck2;
              subsume ck2 cset1
            end
      | Cvar cset, Pck_up (_,_) | Cvar cset, Pck_down (_,_)
      | Cvar cset, Pck_phase (_,_) | Cvar cset, Pck_const (_,_) ->
          if (occurs ck1 ck2) then
            begin
              if (simplify ck2 = ck1) then
                ck2.cdesc <- Clink (simplify ck1)
              else
                raise (Unify (ck1,ck2));
              end
          else
            begin
              ck1.cdesc <- Clink (simplify ck2);
              subsume ck2 cset
            end
      | Pck_up (_,_), Cvar cset | Pck_down (_,_), Cvar cset
      | Pck_phase (_,_), Cvar cset | Pck_const (_,_), Cvar cset ->
            if (occurs ck2 ck1) then
              begin
                if ((simplify ck1) = ck2) then
                  ck1.cdesc <- Clink (simplify ck2)
                else
                  raise (Unify (ck1,ck2));
              end
            else
              begin
                ck2.cdesc <- Clink (simplify ck1);
                subsume ck1 cset
              end
      | (Cvar cset,_) when (not (occurs ck1 ck2)) ->
          subsume ck2 cset;
          update_scope ck1.cscoped ck2;
          ck1.cdesc <- Clink (simplify ck2)
      | (_, (Cvar cset)) when (not (occurs ck2 ck1)) ->
          subsume ck1 cset;
          update_scope ck2.cscoped ck1;
          ck2.cdesc <- Clink (simplify ck1)
      | Ccarrying (cr1,ck1'),Ccarrying (cr2,ck2') ->
          unify_carrier cr1 cr2;
          unify ck1' ck2'
      | Ccarrying (_,_),_ | _,Ccarrying (_,_) ->
          raise (Unify (ck1,ck2))
      | Carrow (c1,c2), Carrow (c1',c2') ->
          unify c1 c1'; unify c2 c2'
      | Ctuple ckl1, Ctuple ckl2 ->
          if (List.length ckl1) <> (List.length ckl2) then
            raise (Unify (ck1,ck2));
          List.iter2 unify ckl1 ckl2
      | Con (ck',c1,l1), Con (ck'',c2,l2) when l1=l2 ->
          unify_carrier c1 c2;
          unify ck' ck''
      | Pck_const (i,r), Pck_const (i',r') ->
          if i<>i' || r <> r' then
            raise (Unify (ck1,ck2))
      | (_, Pck_up (pck2',k)) when (not right_const) ->
          let ck1' = simplify (new_ck (Pck_down (ck1,k)) true) in
          unify ck1' pck2'
      | (_,Pck_down (pck2',k)) when (not right_const) ->
          subsume ck1 (CSet_pck (k,(0,1)));
          let ck1' = simplify (new_ck (Pck_up (ck1,k)) true) in
          unify ck1' pck2'
      | (_,Pck_phase (pck2',(a,b))) when (not right_const) ->
          subsume ck1 (CSet_pck (b,(a,b)));
          let ck1' = simplify (new_ck (Pck_phase (ck1,(-a,b))) true) in
          unify ck1' pck2'
      | Pck_up (pck1',k),_ ->
          let ck2' = simplify (new_ck (Pck_down (ck2,k)) true) in
          unify pck1' ck2'
      | Pck_down (pck1',k),_ ->
          subsume ck2 (CSet_pck (k,(0,1)));
          let ck2' = simplify (new_ck (Pck_up (ck2,k)) true) in
          unify pck1' ck2'
      | Pck_phase (pck1',(a,b)),_ ->
          subsume ck2 (CSet_pck (b,(a,b)));
          let ck2' = simplify (new_ck (Pck_phase (ck2,(-a,b))) true) in
          unify pck1' ck2'
      | Cunivar _, _ | _, Cunivar _ -> ()
      | _,_ -> raise (Unify (ck1,ck2))

(** [unify ck1 ck2] semi-unifies clocks [ck1] and [ck2]. Raises [Unify
    (ck1,ck2)] if the clocks are not semi-unifiable.*)
let rec semi_unify ck1 ck2 =
  let ck1 = repr ck1 in
  let ck2 = repr ck2 in
  if ck1==ck2 then
    ()
  else
      match ck1.cdesc,ck2.cdesc with
      | Cvar cset1,Cvar cset2->
          if ck1.cid < ck2.cid then
            begin
              ck2.cdesc <- Clink (simplify ck1);
              update_scope ck2.cscoped ck1
            end
          else
            begin
              ck1.cdesc <- Clink (simplify ck2);
              update_scope ck1.cscoped ck2
            end
      | (Cvar cset,_) -> raise (Unify (ck1,ck2))
      | (_, (Cvar cset)) when (not (occurs ck2 ck1)) ->
          update_scope ck2.cscoped ck1;
          ck2.cdesc <- Clink (simplify ck1)
      | Ccarrying (cr1,ck1'),Ccarrying (cr2,ck2') ->
          semi_unify_carrier cr1 cr2;
          semi_unify ck1' ck2'
      | Ccarrying (_,_),_ | _,Ccarrying (_,_) ->
          raise (Unify (ck1,ck2))
      | Carrow (c1,c2), Carrow (c1',c2') ->
	begin
          semi_unify c1 c1';
	  semi_unify c2 c2'
	end
      | Ctuple ckl1, Ctuple ckl2 ->
          if (List.length ckl1) <> (List.length ckl2) then
            raise (Unify (ck1,ck2));
          List.iter2 semi_unify ckl1 ckl2
      | Con (ck',c1,l1), Con (ck'',c2,l2) when l1=l2 ->
          semi_unify_carrier c1 c2;
          semi_unify ck' ck''
      | Cunivar _, _ | _, Cunivar _ -> ()
      | _,_ -> raise (Unify (ck1,ck2))

(* Returns the value corresponding to a pclock (integer) factor
   expression. Expects a constant expression (checked by typing). *)
let int_factor_of_expr e =
  match e.expr_desc with 
  | Expr_const
      (Const_int i) -> i
  | _ -> failwith "Internal error: int_factor_of_expr"

(* Unifies all the clock variables in the clock type of a tuple 
   expression, so that the clock type only uses at most one clock variable *)
let unify_tuple_clock ref_ck_opt ck =
  let ck_var = ref ref_ck_opt in
  let rec aux ck =
    match (repr ck).cdesc with
    | Con _
    | Cvar _ ->
        begin
          match !ck_var with
          | None ->
              ck_var:=Some ck
          | Some v ->
              (* may fail *)
              unify v ck
        end
    | Ctuple cl ->
        List.iter aux cl
    | Carrow _ -> assert false (* should not occur *)
    | Ccarrying (_, ck1) ->
        aux ck1
    | _ -> ()
  in
  aux ck

(* Unifies all the clock variables in the clock type of an imported
   node, so that the clock type only uses at most one base clock variable,
   that is, the activation clock of the node *)
let unify_imported_clock ref_ck_opt ck =
  let ck_var = ref ref_ck_opt in
  let rec aux ck =
    match (repr ck).cdesc with
    | Cvar _ ->
        begin
          match !ck_var with
          | None ->
              ck_var:=Some ck
          | Some v ->
              (* cannot fail *)
              unify v ck
        end
    | Ctuple cl ->
        List.iter aux cl
    | Carrow (ck1,ck2) ->
        aux ck1; aux ck2
    | Ccarrying (_, ck1) ->
        aux ck1
    | Con (ck1, _, _) -> aux ck1
    | _ -> ()
  in
  aux ck

(** [clock_uncarry ck] drops the possible carrier name from clock [ck] *)
let clock_uncarry ck =
  let ck = repr ck in
  match ck.cdesc with
  | Ccarrying (_, ck') -> ck'
  | _                  -> ck

let try_unify ck1 ck2 loc =
  try
    unify ck1 ck2
  with
  | Unify (ck1',ck2') ->
    raise (Error (loc, Clock_clash (ck1',ck2')))
  | Subsume (ck,cset) ->
    raise (Error (loc, Clock_set_mismatch (ck,cset)))
  | Mismatch (cr1,cr2) ->
    raise (Error (loc, Carrier_mismatch (cr1,cr2)))

let try_semi_unify ck1 ck2 loc =
  try
    semi_unify ck1 ck2
  with
  | Unify (ck1',ck2') ->
    raise (Error (loc, Clock_clash (ck1',ck2')))
  | Subsume (ck,cset) ->
    raise (Error (loc, Clock_set_mismatch (ck,cset)))
  | Mismatch (cr1,cr2) ->
    raise (Error (loc, Carrier_mismatch (cr1,cr2)))

(* ck1 is a subtype of ck2 *)
let rec sub_unify sub ck1 ck2 =
  match (repr ck1).cdesc, (repr ck2).cdesc with
  | Ctuple cl1         , Ctuple cl2         ->
    if List.length cl1 <> List.length cl2
    then raise (Unify (ck1, ck2))
    else List.iter2 (sub_unify sub) cl1 cl2
  | Ctuple [c1]        , _                  -> sub_unify sub c1 ck2
  | _                  , Ctuple [c2]        -> sub_unify sub ck1 c2
  | Con (c1, cr1, t1)  , Con (c2, cr2, t2) when t1=t2 ->
    begin
      unify_carrier cr1 cr2;
      sub_unify sub c1 c2
    end
  | Ccarrying (cr1, c1), Ccarrying (cr2, c2)->
    begin
      unify_carrier cr1 cr2;
      sub_unify sub c1 c2
    end
  | Ccarrying (_, c1)  , _         when sub -> sub_unify sub c1 ck2
  | _                                       -> unify ck1 ck2

let try_sub_unify sub ck1 ck2 loc =
  try
    sub_unify sub ck1 ck2
  with
  | Unify (ck1',ck2') ->
    raise (Error (loc, Clock_clash (ck1',ck2')))
  | Subsume (ck,cset) ->
    raise (Error (loc, Clock_set_mismatch (ck,cset)))
  | Mismatch (cr1,cr2) ->
    raise (Error (loc, Carrier_mismatch (cr1,cr2)))

(* Clocks a list of arguments of Lustre builtin operators:
   - type each expression, remove carriers of clocks as
     carriers may only denote variables, not arbitrary expr.
   - try to unify these clocks altogether
*)
let rec clock_standard_args env expr_list =
  let ck_list = List.map (fun e -> clock_uncarry (clock_expr env e)) expr_list in
  let ck_res = new_var true in
  List.iter2 (fun e ck -> try_unify ck ck_res e.expr_loc) expr_list ck_list;
  ck_res

(* emulates a subtyping relation between clocks c and (cr : c),
   used during node application only *)
and clock_subtyping_arg env ?(sub=true) real_arg formal_clock =
  let loc = real_arg.expr_loc in
  let real_clock = clock_expr env real_arg in
  try_sub_unify sub real_clock formal_clock loc

(* computes clocks for node application *)
and clock_appl env f args clock_reset loc =
 let args = expr_list_of_expr args in
  if Basic_library.is_internal_fun f && List.exists is_tuple_expr args
  then
      let args = Utils.transpose_list (List.map expr_list_of_expr args) in
      Clocks.clock_of_clock_list (List.map (fun args -> clock_call env f args clock_reset loc) args)
  else
    clock_call env f args clock_reset loc

and clock_call env f args clock_reset loc =
  let cfun = clock_ident false env f loc in
  let cins, couts = split_arrow cfun in
  let cins = clock_list_of_clock cins in
  List.iter2 (clock_subtyping_arg env) args cins;
  unify_imported_clock (Some clock_reset) cfun;
  couts

and clock_ident nocarrier env id loc =
  clock_expr ~nocarrier:nocarrier env (expr_of_ident id loc)

and clock_carrier env c loc ce =
  let expr_c = expr_of_ident c loc in
  let ck = clock_expr ~nocarrier:false env expr_c in
  let cr = new_carrier Carry_name (*Carry_const c*) ck.cscoped in
  let ckcarry = new_ck (Ccarrying (cr,ce)) ck.cscoped in
  try_unify ck ckcarry expr_c.expr_loc;
  cr

(** [clock_expr env expr] performs the clock calculus for expression [expr] in
    environment [env] *)
and clock_expr ?(nocarrier=true) env expr =
  let resulting_ck = 
    match expr.expr_desc with
      | Expr_const cst ->
      let ck = new_var true in
      expr.expr_clock <- ck;
      ck
  | Expr_ident v ->
      let ckv =
        try
          Env.lookup_value env v
        with Not_found -> 
	  failwith ("Internal error, variable \""^v^"\" not found")
      in
      let ck = instantiate (ref []) (ref []) ckv in
      expr.expr_clock <- ck;
      ck
  | Expr_array elist ->
    let ck = clock_standard_args env elist in
    expr.expr_clock <- ck;
    ck
  | Expr_access (e1, d) ->
    (* dimension, being a static value, doesn't need to be clocked *)
    let ck = clock_standard_args env [e1] in
    expr.expr_clock <- ck;
    ck
  | Expr_power (e1, d) ->
    (* dimension, being a static value, doesn't need to be clocked *)
    let ck = clock_standard_args env [e1] in
    expr.expr_clock <- ck;
    ck
  | Expr_tuple elist ->
    let ck = new_ck (Ctuple (List.map (clock_expr env) elist)) true in
    expr.expr_clock <- ck;
    ck
  | Expr_ite (c, t, e) ->
    let ck_c = clock_standard_args env [c] in
    let ck = clock_standard_args env [t; e] in
    (* Here, the branches may exhibit a tuple clock, not the condition *)
    unify_tuple_clock (Some ck_c) ck;
    expr.expr_clock <- ck;
    ck
  | Expr_appl (id, args, r) ->
    (try
(* for a modular compilation scheme, all inputs/outputs must share the same clock !
   this is also the reset clock !
*)
    let cr =
      match r with
      | None        -> new_var true
      | Some (x, _) -> let loc_r = loc_of_cond expr.expr_loc x in
		       let expr_r = expr_of_ident x loc_r in
		       clock_expr env expr_r in
    let couts = clock_appl env id args cr expr.expr_loc in
    expr.expr_clock <- couts;
    couts
    with exn -> (
      Format.eprintf "Current expr: %a@." Printers.pp_expr expr; 
      raise exn
    ))
  | Expr_fby (e1,e2)
  | Expr_arrow (e1,e2) ->
    let ck = clock_standard_args env [e1; e2] in
    unify_tuple_clock None ck;
    expr.expr_clock <- ck;
    ck
  | Expr_pre e -> (* todo : deal with phases as in tail ? *)
      let ck = clock_standard_args env [e] in
      expr.expr_clock <- ck;
      ck
  | Expr_when (e,c,l) ->
      let ce = clock_standard_args env [e] in
      let c_loc = loc_of_cond expr.expr_loc c in
      let cr = clock_carrier env c c_loc ce in
      let ck = new_ck (Con (ce,cr,l)) true in
      let cr' = new_carrier (Carry_const c) ck.cscoped in
      let ck' = new_ck (Con (ce,cr',l)) true in
      expr.expr_clock <- ck';
      ck
  | Expr_merge (c,hl) ->
      let cvar = new_var true in
      let cr = clock_carrier env c expr.expr_loc cvar in
      List.iter (fun (t, h) -> clock_subtyping_arg env h (new_ck (Con (cvar,cr,t)) true)) hl;
      expr.expr_clock <- cvar;
      cvar
  in
  Log.report ~level:4 (fun fmt -> Format.fprintf fmt "Clock of expr %a: %a@." Printers.pp_expr expr Clocks.print_ck resulting_ck);
  resulting_ck

let clock_of_vlist vars =
  let ckl = List.map (fun v -> v.var_clock) vars in
  clock_of_clock_list ckl

(** [clock_eq env eq] performs the clock-calculus for equation [eq] in
    environment [env] *)
let clock_eq env eq =
  let expr_lhs = expr_of_expr_list eq.eq_loc (List.map (fun v -> expr_of_ident v eq.eq_loc) eq.eq_lhs) in
  let ck_rhs = clock_expr env eq.eq_rhs in
  clock_subtyping_arg env expr_lhs ck_rhs


(* [clock_coreclock cck] returns the clock_expr corresponding to clock
    declaration [cck] *)
let clock_coreclock env cck id loc scoped =
  match cck.ck_dec_desc with
  | Ckdec_any -> new_var scoped
  | Ckdec_pclock (n,(a,b)) ->
      let ck = new_ck (Pck_const (n,(a,b))) scoped in
      if n mod b <> 0 then raise (Error (loc,Invalid_const ck));
      ck
  | Ckdec_bool cl ->
      let temp_env = Env.add_value env id (new_var true) in
      (* We just want the id to be present in the environment *)
      let dummy_id_expr = expr_of_ident id loc in
      let when_expr =
        List.fold_left
          (fun expr (x,l) ->
                {expr_tag = new_tag ();
                 expr_desc = Expr_when (expr,x,l);
                 expr_type = Types.new_var ();
                 expr_clock = new_var scoped;
                 expr_delay = Delay.new_var ();
                 expr_loc = loc;
		 expr_annot = None})
          dummy_id_expr cl
      in
      clock_expr temp_env when_expr

(* Clocks a variable declaration *)
let clock_var_decl scoped env vdecl =
  let ck = clock_coreclock env vdecl.var_dec_clock vdecl.var_id vdecl.var_loc scoped in
  let ck =
(* WTF ????
    if vdecl.var_dec_const
    then
      (try_generalize ck vdecl.var_loc; ck)
    else
 *)
      if Types.get_clock_base_type vdecl.var_type <> None
      then new_ck (Ccarrying ((new_carrier Carry_name scoped),ck)) scoped
      else ck in
  vdecl.var_clock <- ck;
  Env.add_value env vdecl.var_id ck

(* Clocks a variable declaration list *)
let clock_var_decl_list env scoped l =
  List.fold_left (clock_var_decl scoped) env l

(** [clock_node env nd] performs the clock-calculus for node [nd] in
    environment [env].
    Generalization of clocks wrt scopes follows this rule:
    - generalize inputs (unscoped).
    - generalize outputs. As they are scoped, only clocks coming from inputs
      are allowed to be generalized.
    - generalize locals. As outputs don't depend on them (checked the step before),
      they can be generalized. 
 *)
let clock_node env loc nd =
  (* let is_main = nd.node_id = !Options.main_node in *)
  let new_env = clock_var_decl_list env false nd.node_inputs in
  let new_env = clock_var_decl_list new_env true nd.node_outputs in
  let new_env = clock_var_decl_list new_env true nd.node_locals in
  List.iter (clock_eq new_env) nd.node_eqs;
  let ck_ins = clock_of_vlist nd.node_inputs in
  let ck_outs = clock_of_vlist nd.node_outputs in
  let ck_node = new_ck (Carrow (ck_ins,ck_outs)) false in
  unify_imported_clock None ck_node;
  Log.report ~level:3 (fun fmt -> print_ck fmt ck_node);
  (* Local variables may contain first-order carrier variables that should be generalized.
     That's not the case for types. *)
  try_generalize ck_node loc;
(*
  List.iter (fun vdecl -> try_generalize vdecl.var_clock vdecl.var_loc) nd.node_inputs;
  List.iter (fun vdecl -> try_generalize vdecl.var_clock vdecl.var_loc) nd.node_outputs;*)
  (*List.iter (fun vdecl -> try_generalize vdecl.var_clock vdecl.var_loc) nd.node_locals;*)
  (* TODO : Xavier pourquoi ai je cette erreur ? *)
(*  if (is_main && is_polymorphic ck_node) then
    raise (Error (loc,(Cannot_be_polymorphic ck_node)));
*)
  Log.report ~level:3 (fun fmt -> print_ck fmt ck_node);
  nd.node_clock <- ck_node;
  Env.add_value env nd.node_id ck_node


let check_imported_pclocks loc ck_node =
  let pck = ref None in
  let rec aux ck =
    match ck.cdesc with
    | Carrow (ck1,ck2) -> aux ck1; aux ck2
    | Ctuple cl -> List.iter aux cl
    | Con (ck',_,_) -> aux ck'
    | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_) -> 
        raise (Error (loc, (Invalid_imported_clock ck_node)))
    | Pck_const (n,p) ->
        begin
          match !pck with
          | None -> pck := Some (n,p)
          | Some (n',p') ->
              if (n,p) <> (n',p') then
                raise (Error (loc, (Invalid_imported_clock ck_node)))
        end
    | Clink ck' -> aux ck'
    | Ccarrying (_,ck') -> aux ck'
    | Cvar _ | Cunivar _ ->
        match !pck with
        | None -> ()
        | Some (_,_) ->
            raise (Error (loc, (Invalid_imported_clock ck_node)))
  in
  aux ck_node

let clock_imported_node env loc nd =
  let new_env = clock_var_decl_list env false nd.nodei_inputs in
  ignore(clock_var_decl_list new_env false nd.nodei_outputs);
  let ck_ins = clock_of_vlist nd.nodei_inputs in
  let ck_outs = clock_of_vlist nd.nodei_outputs in
  let ck_node = new_ck (Carrow (ck_ins,ck_outs)) false in
  unify_imported_clock None ck_node;
  check_imported_pclocks loc ck_node;
  try_generalize ck_node loc;
  nd.nodei_clock <- ck_node;
  Env.add_value env nd.nodei_id ck_node

let clock_top_consts env clist =
  List.fold_left (fun env cdecl ->
    let ck = new_var false in
    try_generalize ck cdecl.const_loc;
    Env.add_value env cdecl.const_id ck) env clist

let clock_top_decl env decl =
  match decl.top_decl_desc with
  | Node nd ->
    clock_node env decl.top_decl_loc nd
  | ImportedNode nd ->
    clock_imported_node env decl.top_decl_loc nd
  | Consts clist ->
    clock_top_consts env clist
  | Open _ ->
    env

let clock_prog env decls =
  List.fold_left (fun e decl -> clock_top_decl e decl) env decls

(* Once the Lustre program is fully clocked,
   we must get back to the original description of clocks,
   with constant parameters, instead of unifiable internal variables. *)

(* The following functions aims at 'unevaluating' carriers occuring in clock expressions,
   i.e. replacing unifiable second_order variables with the original carrier names.
   Once restored in this formulation, clocks may be meaningfully printed.
*)
let uneval_vdecl_generics vdecl =
 (*Format.eprintf "Clock_calculus.uneval_vdecl_generics %a@." Printers.pp_node_var vdecl;*)
 if Types.get_clock_base_type vdecl.var_type <> None
 then
   match get_carrier_name vdecl.var_clock with
   | None    -> (Format.eprintf "internal error: %a@." print_ck vdecl.var_clock; assert false)
   | Some cr -> Clocks.uneval vdecl.var_id cr

let uneval_node_generics vdecls =
  List.iter uneval_vdecl_generics vdecls

let uneval_top_generics decl =
  match decl.top_decl_desc with
  | Node nd ->
      (* A node could contain first-order carrier variable in local vars. This is not the case for types. *)
      uneval_node_generics (nd.node_inputs @ nd.node_locals @ nd.node_outputs)
  | ImportedNode nd ->
      uneval_node_generics (nd.nodei_inputs @ nd.nodei_outputs)
  | Consts clist -> ()
  | Open _  -> ()

let uneval_prog_generics prog =
 List.iter uneval_top_generics prog

let check_env_compat header declared computed =
  uneval_prog_generics header;
  Env.iter declared (fun k decl_clock_k -> 
    let computed_c = instantiate (ref []) (ref []) (Env.lookup_value computed k) in
    try_semi_unify decl_clock_k computed_c Location.dummy_loc
  ) 
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
