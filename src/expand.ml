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

open Clocks
open Corelang

(* Applies variable substitutions *)
let subst_var var_substs vid =
  try Hashtbl.find var_substs vid with Not_found -> vid

(* Applies clock substitutions *)
let rec subst_ck ck_substs var_substs ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      {ck with cdesc =
       Carrow (subst_ck ck_substs var_substs ck1,
               subst_ck ck_substs var_substs ck2)}
  | Ctuple cklist ->
      {ck with cdesc =
       Ctuple (List.map (subst_ck ck_substs var_substs) cklist)}
  | Con (ck',c) ->
      {ck with cdesc =
       Con (subst_ck ck_substs var_substs ck',c)}
  | Connot (ck',c) ->
      {ck with cdesc =
       Connot (subst_ck ck_substs var_substs ck',c)}
  | Pck_up (ck',k) ->
      {ck with cdesc = 
       Pck_up (subst_ck ck_substs var_substs ck', k)}
  | Pck_down (ck',k) ->
      {ck with cdesc = 
       Pck_down (subst_ck ck_substs var_substs ck', k)}
  | Pck_phase (ck',q) ->
      {ck with cdesc = 
       Pck_phase (subst_ck ck_substs var_substs ck', q)}
  | Pck_const _ ->
      ck
  | Cvar _ | Cunivar _ ->
      begin
        try Hashtbl.find ck_substs ck with Not_found -> ck
      end
  | Clink ck' ->
      subst_ck ck_substs var_substs ck'
  | Ccarrying (_,ck') ->
      subst_ck ck_substs var_substs ck'

(* [new_expr_instance ck_substs var_substs e edesc] returns a new
   "instance" of expressions [e] of expression [e], where [edesc] is the
   expanded version of [e]. *)
let new_expr_instance ck_substs var_substs e edesc =
  {expr_tag = Utils.new_tag ();
   expr_desc = edesc;
   expr_type = e.expr_type;
   expr_clock = subst_ck ck_substs var_substs e.expr_clock;
   expr_delay = Delay.new_var ();
   expr_loc = e.expr_loc;
   expr_annot = e.expr_annot}
  
let locals_cpt = ref 0

(* Returns a new local variable (for the main node) *)
let new_local vtyp vck vdd vloc =
  let vid = "_"^(string_of_int !locals_cpt) in
  locals_cpt := !locals_cpt+1;
  let ty_dec = {ty_dec_desc = Tydec_any; ty_dec_loc = vloc} in (* dummy *)
  let ck_dec = {ck_dec_desc = Ckdec_any; ck_dec_loc = vloc} in (* dummy *)
  {var_id = vid;
   var_dec_type = ty_dec;
   var_dec_clock = ck_dec;
   var_dec_deadline = vdd;
   var_type = vtyp;
   var_clock = vck;
   var_loc = vloc}

(* Returns an expression correponding to variable v *)
let expr_of_var v =
  {expr_tag = Utils.new_tag ();
   expr_desc = Expr_ident v.var_id;
   expr_type = v.var_type;
   expr_clock = v.var_clock;
   expr_delay = Delay.new_var ();
   expr_loc = v.var_loc;
   expr_annot = None}

(* [build_ck_substs ck for_ck] computes the variable substitutions
   corresponding to the substitution of [ck] for [for_ck] *)
let build_ck_substs ck for_ck =
  let substs = Hashtbl.create 10 in
  let rec aux ck for_ck =
    let ck, for_ck = Clocks.repr ck, Clocks.repr for_ck in
    match ck.Clocks.cdesc, for_ck.Clocks.cdesc with
    | Clocks.Ctuple cklist1, Clocks.Ctuple cklist2 ->
        List.iter2 aux cklist1 cklist2
    | _, Clocks.Cunivar _ ->
        Hashtbl.add substs for_ck ck
    | _,_ ->
        ()
  in
  aux ck for_ck;
  substs

(* Expands a list of expressions *)
let rec expand_list ck_substs var_substs elist =
  List.fold_right
    (fun e (eqs,locs,elist) ->
      let eqs',locs',e' = expand_expr ck_substs var_substs e in
      eqs'@eqs,locs'@locs,e'::elist)
    elist ([],[],[])

(* Expands the node instance [nd(args)]. *)
and expand_nodeinst parent_ck_substs parent_vsubsts nd args =
  (* Expand arguments *)
  let args_eqs, args_locs, args' =
    expand_expr parent_ck_substs parent_vsubsts args in
  (* Compute clock substitutions to apply inside node's body *)
  let ck_ins = args'.expr_clock in
  let for_ck_ins,_ = Clocks.split_arrow nd.node_clock in
  let ck_substs = build_ck_substs ck_ins for_ck_ins in
  (* Compute variable substitutions to apply inside node's body, due
     to the transformation of node variables into local variables of the
     main node. *)
  let var_substs = Hashtbl.create 10 in
  (* Add an equation in=arg for each node input + transform node input
     into a local variable of the main node *)
  let eq_ins, loc_ins =
    List.split
      (List.map2
         (fun i e ->
           let i' =
             new_local i.var_type i.var_clock i.var_dec_deadline i.var_loc in
           Hashtbl.add var_substs i.var_id i'.var_id;
           {eq_lhs = [i'.var_id];
            eq_rhs = e;
            eq_loc = i.var_loc}, i')
         nd.node_inputs (expr_list_of_expr args'))
  in
  (* Transform node local variables into local variables of the main
     node *)
  let loc_sub =
    List.map
      (fun v ->
        let v' = new_local v.var_type v.var_clock v.var_dec_deadline v.var_loc in
        Hashtbl.add var_substs v.var_id v'.var_id;
        v')
      nd.node_locals
  in  
  (* Same for outputs *)
  let loc_outs =
    List.map
      (fun o ->
        let o' = new_local o.var_type o.var_clock o.var_dec_deadline o.var_loc in
        Hashtbl.add var_substs o.var_id o'.var_id;
        o')
      nd.node_outputs
  in  
  (* A tuple made of the node outputs will replace the node call in the parent
     node *)
  let eout = Expr_tuple (List.map expr_of_var loc_outs) in
  let new_eqs, new_locals = expand_eqs ck_substs var_substs nd.node_eqs in
  args_eqs@eq_ins@new_eqs,
  args_locs@loc_ins@loc_outs@loc_sub@new_locals,
  eout

(* Expands an expression *)
and expand_expr ck_substs var_substs expr =
  match expr.expr_desc with
  | Expr_const cst ->
      [],[],new_expr_instance ck_substs var_substs expr expr.expr_desc
  | Expr_ident id ->
      let id' = subst_var var_substs id in
      let edesc = Expr_ident id' in
      [],[],new_expr_instance ck_substs var_substs expr edesc
  | Expr_tuple elist ->
      let new_eqs,new_locals,exp_elist =
        expand_list ck_substs var_substs elist in
      new_eqs, new_locals,
      new_expr_instance ck_substs var_substs expr (Expr_tuple exp_elist)
  | Expr_fby (cst,e) ->
      let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_fby (cst, e') in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_concat (cst,e) ->
      let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_concat (cst, e') in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_tail e ->
      let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_tail e' in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_when (e,c) ->
      let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_when (e',c) in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_whennot (e,c) ->
      let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_whennot (e',c) in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_merge (c,e1,e2) ->
      let new_eqs1,new_locals1, e1' = expand_expr ck_substs var_substs e1 in
      let new_eqs2,new_locals2, e2' = expand_expr ck_substs var_substs e2 in
      let edesc = Expr_merge (c,e1',e2') in
      new_eqs1@new_eqs2,
      new_locals1@new_locals2,
      new_expr_instance ck_substs var_substs expr edesc
  | Expr_appl (id, e, r) ->
      let decl = Hashtbl.find node_table id in
      begin
        match decl.top_decl_desc with
        | ImportedNode _ ->
            let new_eqs,new_locals, e' = expand_expr ck_substs var_substs e in
            let edesc = Expr_appl (id, e', r) in
            new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
        | Node nd ->
            let new_eqs, new_locals, outs =
              expand_nodeinst ck_substs var_substs nd e in
            new_eqs, new_locals, new_expr_instance ck_substs var_substs expr outs
        | Include _ | Consts _ | SensorDecl _ | ActuatorDecl _ -> failwith "Internal error expand_expr"
      end
  | Expr_uclock (e,k) ->
      let new_eqs, new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_uclock (e',k) in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_dclock (e,k) ->
      let new_eqs, new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_dclock (e',k) in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_phclock (e,q) ->
      let new_eqs, new_locals, e' = expand_expr ck_substs var_substs e in
      let edesc = Expr_phclock (e',q) in
      new_eqs, new_locals, new_expr_instance ck_substs var_substs expr edesc
  | Expr_pre _ | Expr_arrow _ -> assert false (* Not used in the Prelude part of the code *)

(* Expands an equation *)
and expand_eq ck_substs var_substs eq =
  let new_eqs, new_locals, expr = expand_expr ck_substs var_substs eq.eq_rhs in
  let lhs' = List.map (subst_var var_substs) eq.eq_lhs in
  let eq' = {eq_lhs = lhs'; eq_rhs = expr; eq_loc = eq.eq_loc} in
  new_eqs, new_locals, eq'

(* Expands a set of equations *)
and expand_eqs ck_substs var_substs eqs =
  List.fold_left
    (fun (acc_eqs,acc_locals) eq ->
      let new_eqs, new_locals, eq' = expand_eq ck_substs var_substs eq in
      (eq'::(new_eqs@acc_eqs)), (new_locals@acc_locals))
    ([],[]) eqs

(* Expands the body of a node, replacing recursively all the node calls
   it contains by the body of the corresponding node. *)
let expand_node nd =
  let new_eqs, new_locals =
    expand_eqs (Hashtbl.create 10) (Hashtbl.create 10) nd.node_eqs in
  {node_id = nd.node_id;
   node_type = nd.node_type;
   node_clock = nd.node_clock;
   node_inputs = nd.node_inputs;
   node_outputs = nd.node_outputs;
   node_locals = new_locals@nd.node_locals;
   node_asserts = nd.node_asserts;
   node_eqs = new_eqs;
   node_spec = nd.node_spec;
   node_annot = nd.node_annot}

let expand_program () =
  if !Options.main_node = "" then
    raise (Corelang.Error No_main_specified);
  let main =
    try
      Hashtbl.find node_table !Options.main_node
    with Not_found ->
      raise (Corelang.Error Main_not_found)
  in
  match main.top_decl_desc with
  | Include _ | Consts _ | ImportedNode _ | SensorDecl _ | ActuatorDecl _ ->
      raise (Corelang.Error Main_wrong_kind)
  | Node nd ->
      expand_node nd

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
