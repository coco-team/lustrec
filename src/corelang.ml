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
open Format
open LustreSpec
open Dimension

(** The core language and its ast. Every element of the ast contains its
    location in the program text. The type and clock of an ast element
    is mutable (and initialized to dummy values). This avoids to have to
    duplicate ast structures (e.g. ast, typed_ast, clocked_ast). *)

type ident = Utils.ident
type label = Utils.ident
type rat = Utils.rat
type tag = Utils.tag

type constant =
  | Const_int of int
  | Const_real of string
  | Const_float of float
  | Const_array of constant list
  | Const_tag of label
  | Const_struct of (label * constant) list

type type_dec = LustreSpec.type_dec

let dummy_type_dec = {ty_dec_desc=Tydec_any; ty_dec_loc=Location.dummy_loc}


type clock_dec = LustreSpec.clock_dec

let dummy_clock_dec = {ck_dec_desc=Ckdec_any; ck_dec_loc=Location.dummy_loc}

type var_decl = LustreSpec.var_decl

(* The tag of an expression is a unique identifier used to distinguish
   different instances of the same node *)
type expr =
    {expr_tag: tag;
     expr_desc: expr_desc;
     mutable expr_type: Types.type_expr;
     mutable expr_clock: Clocks.clock_expr;
     mutable expr_delay: Delay.delay_expr;
     mutable expr_annot: LustreSpec.expr_annot option;
     expr_loc: Location.t}

and expr_desc =
  | Expr_const of constant
  | Expr_ident of ident
  | Expr_tuple of expr list
  | Expr_ite   of expr * expr * expr
  | Expr_arrow of expr * expr
  | Expr_fby of expr * expr
  | Expr_array of expr list
  | Expr_access of expr * Dimension.dim_expr
  | Expr_power of expr * Dimension.dim_expr
  | Expr_pre of expr
  | Expr_when of expr * ident * label
  | Expr_merge of ident * (label * expr) list
  | Expr_appl of call_t
  | Expr_uclock of expr * int
  | Expr_dclock of expr * int
  | Expr_phclock of expr * rat
 and call_t = ident * expr * (ident * label) option (* The third part denotes the reseting clock label and value *)

type eq =
    {eq_lhs: ident list;
     eq_rhs: expr;
     eq_loc: Location.t}

type assert_t = 
    {
      assert_expr: expr;
      assert_loc: Location.t
    } 

type node_desc =
    {node_id: ident;
     mutable node_type: Types.type_expr;
     mutable node_clock: Clocks.clock_expr;
     node_inputs: var_decl list;
     node_outputs: var_decl list;
     node_locals: var_decl list;
     mutable node_gencalls: expr list;
     mutable node_checks: Dimension.dim_expr list;
     node_asserts: assert_t list; 
     node_eqs: eq list;
     mutable node_dec_stateless: bool;
     mutable node_stateless: bool option;
     node_spec: LustreSpec.node_annot option;
     node_annot: LustreSpec.expr_annot option;
    }

type imported_node_desc =
    {nodei_id: ident;
     mutable nodei_type: Types.type_expr;
     mutable nodei_clock: Clocks.clock_expr;
     nodei_inputs: var_decl list;
     nodei_outputs: var_decl list;
     nodei_stateless: bool;
     nodei_spec: LustreSpec.node_annot option;
     nodei_prototype: string option;
     nodei_in_lib: string option;
    }

type const_desc = 
    {const_id: ident; 
     const_loc: Location.t; 
     const_value: constant;      
     mutable const_type: Types.type_expr;
    }

type top_decl_desc =
| Node of node_desc
| Consts of const_desc list
| ImportedNode of imported_node_desc
| Open of bool * string (* the boolean set to true denotes a local 
			   lusi vs a lusi installed at system level *)

type top_decl =
    {top_decl_desc: top_decl_desc;
     top_decl_loc: Location.t}

type program = top_decl list

type error =
    Main_not_found
  | Main_wrong_kind
  | No_main_specified
  | Unbound_symbol of ident
  | Already_bound_symbol of ident

exception Error of Location.t * error

module VDeclModule =
struct (* Node module *)
  type t = var_decl
  let compare v1 v2 = compare v1 v2
  let hash n = Hashtbl.hash n
  let equal n1 n2 = n1 = n2
end

module VMap = Map.Make(VDeclModule)

module VSet = Set.Make(VDeclModule)

(************************************************************)
(* *)

let mktyp loc d =
  { ty_dec_desc = d; ty_dec_loc = loc }

let mkclock loc d =
  { ck_dec_desc = d; ck_dec_loc = loc }

let mkvar_decl loc (id, ty_dec, ck_dec, is_const) =
  { var_id = id;
    var_dec_type = ty_dec;
    var_dec_clock = ck_dec;
    var_dec_const = is_const;
    var_type = Types.new_var ();
    var_clock = Clocks.new_var true;
    var_loc = loc }

let mkexpr loc d =
  { expr_tag = Utils.new_tag ();
    expr_desc = d;
    expr_type = Types.new_var ();
    expr_clock = Clocks.new_var true;
    expr_delay = Delay.new_var ();
    expr_annot = None;
    expr_loc = loc }

let var_decl_of_const c =
  { var_id = c.const_id;
    var_dec_type = { ty_dec_loc = c.const_loc; ty_dec_desc = Tydec_any };
    var_dec_clock = { ck_dec_loc = c.const_loc; ck_dec_desc = Ckdec_any };
    var_dec_const = true;
    var_type = c.const_type;
    var_clock = Clocks.new_var false;
    var_loc = c.const_loc }

let mk_new_name vdecl_list id =
  let rec new_name name cpt =
    if List.exists (fun v -> v.var_id = name) vdecl_list
    then new_name (sprintf "_%s_%i" id cpt) (cpt+1)
    else name
  in new_name id 1

let update_expr_annot e annot =
  { e with expr_annot = LustreSpec.merge_expr_annot e.expr_annot (Some annot) }

let mkeq loc (lhs, rhs) =
  { eq_lhs = lhs;
    eq_rhs = rhs;
    eq_loc = loc }

let mkassert loc expr =
  { assert_loc = loc;
    assert_expr = expr
  }

let mktop_decl loc d =
  { top_decl_desc = d; top_decl_loc = loc }

let mkpredef_call loc funname args =
  mkexpr loc (Expr_appl (funname, mkexpr loc (Expr_tuple args), None))

(***********************************************************)
(* Fast access to nodes, by name *)
let (node_table : (ident, top_decl) Hashtbl.t) = Hashtbl.create 30
let consts_table = Hashtbl.create 30

let node_name td =
    match td.top_decl_desc with 
    | Node nd         -> nd.node_id
    | ImportedNode nd -> nd.nodei_id
    | _ -> assert false

let is_generic_node td =
  match td.top_decl_desc with 
  | Node nd         -> List.exists (fun v -> v.var_dec_const) nd.node_inputs
  | ImportedNode nd -> List.exists (fun v -> v.var_dec_const) nd.nodei_inputs
  | _ -> assert false

let node_inputs td =
  match td.top_decl_desc with 
  | Node nd         -> nd.node_inputs
  | ImportedNode nd -> nd.nodei_inputs
  | _ -> assert false

let node_from_name id =
  try
    Hashtbl.find node_table id
  with Not_found -> (Format.eprintf "Unable to find any node named %s@ @?" id;
		     assert false)

let is_imported_node td =
  match td.top_decl_desc with 
  | Node nd         -> false
  | ImportedNode nd -> true
  | _ -> assert false


(* alias and type definition table *)
let type_table =
  Utils.create_hashtable 20 [
    Tydec_int  , Tydec_int;
    Tydec_bool , Tydec_bool;
    Tydec_float, Tydec_float;
    Tydec_real , Tydec_real
  ]

let rec is_user_type typ =
  match typ with
  | Tydec_int | Tydec_bool | Tydec_real 
  | Tydec_float | Tydec_any | Tydec_const _ -> false
  | Tydec_clock typ' -> is_user_type typ'
  | _ -> true

let get_repr_type typ =
  let typ_def = Hashtbl.find type_table typ in
  if is_user_type typ_def then typ else typ_def

let tag_true = "true"
let tag_false = "false"

let const_is_bool c =
 match c with
 | Const_tag t -> t = tag_true || t = tag_false
 | _           -> false

(* Computes the negation of a boolean constant *)
let const_negation c =
  assert (const_is_bool c);
  match c with
  | Const_tag t when t = tag_true  -> Const_tag tag_false
  | _                              -> Const_tag tag_true

let const_or c1 c2 =
  assert (const_is_bool c1 && const_is_bool c2);
  match c1, c2 with
  | Const_tag t1, _            when t1 = tag_true -> c1
  | _           , Const_tag t2 when t2 = tag_true -> c2
  | _                                             -> Const_tag tag_false

let const_and c1 c2 =
  assert (const_is_bool c1 && const_is_bool c2);
  match c1, c2 with
  | Const_tag t1, _            when t1 = tag_false -> c1
  | _           , Const_tag t2 when t2 = tag_false -> c2
  | _                                              -> Const_tag tag_true

let const_xor c1 c2 =
  assert (const_is_bool c1 && const_is_bool c2);
   match c1, c2 with
  | Const_tag t1, Const_tag t2 when t1 <> t2  -> Const_tag tag_true
  | _                                         -> Const_tag tag_false

let const_impl c1 c2 =
  assert (const_is_bool c1 && const_is_bool c2);
  match c1, c2 with
  | Const_tag t1, _ when t1 = tag_false           -> Const_tag tag_true
  | _           , Const_tag t2 when t2 = tag_true -> Const_tag tag_true
  | _                                             -> Const_tag tag_false

(* To guarantee uniqueness of tags in enum types *)
let tag_table =
  Utils.create_hashtable 20 [
   tag_true, Tydec_bool;
   tag_false, Tydec_bool
  ]

(* To guarantee uniqueness of fields in struct types *)
let field_table =
  Utils.create_hashtable 20 [
  ]

let get_enum_type_tags cty =
 match cty with
 | Tydec_bool    -> [tag_true; tag_false]
 | Tydec_const _ -> (match Hashtbl.find type_table cty with
                     | Tydec_enum tl -> tl
                     | _             -> assert false)
 | _            -> assert false

let get_struct_type_fields cty =
 match cty with
 | Tydec_const _ -> (match Hashtbl.find type_table cty with
                     | Tydec_struct fl -> fl
                     | _               -> assert false)
 | _            -> assert false

let const_of_bool b =
 Const_tag (if b then tag_true else tag_false)

(* let get_const c = snd (Hashtbl.find consts_table c) *)

let ident_of_expr expr =
 match expr.expr_desc with
 | Expr_ident id -> id
 | _             -> assert false

(* Caution, returns an untyped and unclocked expression *)
let expr_of_ident id loc =
  {expr_tag = Utils.new_tag ();
   expr_desc = Expr_ident id;
   expr_type = Types.new_var ();
   expr_clock = Clocks.new_var true;
   expr_delay = Delay.new_var ();
   expr_loc = loc;
   expr_annot = None}

let is_tuple_expr expr =
 match expr.expr_desc with
  | Expr_tuple _ -> true
  | _            -> false

let expr_list_of_expr expr =
  match expr.expr_desc with
  | Expr_tuple elist ->
      elist
  | _ -> [expr]

let expr_of_expr_list loc elist =
 match elist with
 | [t]  -> { t with expr_loc = loc }
 | t::_ -> { t with expr_desc = Expr_tuple elist; expr_loc = loc }
 | _    -> assert false

let call_of_expr expr =
 match expr.expr_desc with
 | Expr_appl (f, args, r) -> (f, expr_list_of_expr args, r)
 | _                      -> assert false

(* Conversion from dimension expr to standard expr, for the purpose of printing, typing, etc... *)
let rec expr_of_dimension dim =
 match dim.dim_desc with
 | Dbool b        ->
     mkexpr dim.dim_loc (Expr_const (const_of_bool b))
 | Dint i         ->
     mkexpr dim.dim_loc (Expr_const (Const_int i))
 | Dident id      ->
     mkexpr dim.dim_loc (Expr_ident id)
 | Dite (c, t, e) ->
     mkexpr dim.dim_loc (Expr_ite (expr_of_dimension c, expr_of_dimension t, expr_of_dimension e))
 | Dappl (id, args) ->
     mkexpr dim.dim_loc (Expr_appl (id, expr_of_expr_list dim.dim_loc (List.map expr_of_dimension args), None))
 | Dlink dim'       -> expr_of_dimension dim'
 | Dvar
 | Dunivar          -> (Format.eprintf "internal error: expr_of_dimension %a@." Dimension.pp_dimension dim;
			assert false)

let dimension_of_const loc const =
 match const with
 | Const_int i                                    -> mkdim_int loc i
 | Const_tag t when t = tag_true || t = tag_false -> mkdim_bool loc (t = tag_true)
 | _                                              -> raise InvalidDimension

(* Conversion from standard expr to dimension expr, for the purpose of injecting static call arguments 
   into dimension expressions *)
let rec dimension_of_expr expr =
  match expr.expr_desc with
  | Expr_const c  -> dimension_of_const expr.expr_loc c
  | Expr_ident id -> mkdim_ident expr.expr_loc id
  | Expr_appl (f, args, None) when Basic_library.is_internal_fun f ->
      let k = Types.get_static_value (Env.lookup_value Basic_library.type_env f) in
      if k = None then raise InvalidDimension;
      mkdim_appl expr.expr_loc f (List.map dimension_of_expr (expr_list_of_expr args))
  | Expr_ite (i, t, e)        ->
      mkdim_ite expr.expr_loc (dimension_of_expr i) (dimension_of_expr t) (dimension_of_expr e)
  | _ -> raise InvalidDimension (* not a simple dimension expression *)


let sort_handlers hl =
 List.sort (fun (t, _) (t', _) -> compare t t') hl

let rec is_eq_expr e1 e2 = match e1.expr_desc, e2.expr_desc with
  | Expr_const c1, Expr_const c2 -> c1 = c2
  | Expr_ident i1, Expr_ident i2 -> i1 = i2
  | Expr_array el1, Expr_array el2 
  | Expr_tuple el1, Expr_tuple el2 -> 
    List.length el1 = List.length el2 && List.for_all2 is_eq_expr el1 el2 
  | Expr_arrow (e1, e2), Expr_arrow (e1', e2') -> is_eq_expr e1 e1' && is_eq_expr e2 e2'
  | Expr_fby (e1,e2), Expr_fby (e1',e2') -> is_eq_expr e1 e1' && is_eq_expr e2 e2'
  | Expr_ite (i1, t1, e1), Expr_ite (i2, t2, e2) -> is_eq_expr i1 i2 && is_eq_expr t1 t2 && is_eq_expr e1 e2
  (* | Expr_concat (e1,e2), Expr_concat (e1',e2') -> is_eq_expr e1 e1' && is_eq_expr e2 e2' *)
  (* | Expr_tail e, Expr_tail e' -> is_eq_expr e e' *)
  | Expr_pre e, Expr_pre e' -> is_eq_expr e e'
  | Expr_when (e, i, l), Expr_when (e', i', l') -> l=l' && i=i' && is_eq_expr e e'
  | Expr_merge(i, hl), Expr_merge(i', hl') -> i=i' && List.for_all2 (fun (t, h) (t', h') -> t=t' && is_eq_expr h h') (sort_handlers hl) (sort_handlers hl')
  | Expr_appl (i, e, r), Expr_appl (i', e', r') -> i=i' && r=r' && is_eq_expr e e'
  | Expr_uclock(e, i), Expr_uclock(e', i') -> i=i' && is_eq_expr e e'
  | Expr_dclock(e, i), Expr_dclock(e', i') -> i=i' && is_eq_expr e e'
  | Expr_phclock(e, r), Expr_phclock(e', r') -> r=r' && is_eq_expr e e'
  | Expr_power (e1, i1), Expr_power (e2, i2)
  | Expr_access (e1, i1), Expr_access (e2, i2) -> is_eq_expr e1 e2 && is_eq_expr (expr_of_dimension i1) (expr_of_dimension i2)
  | _ -> false

let node_vars nd =
  nd.node_inputs @ nd.node_locals @ nd.node_outputs

let node_var id node =
 List.find (fun v -> v.var_id = id) (node_vars node)

let node_eq id node =
 List.find (fun eq -> List.mem id eq.eq_lhs) node.node_eqs

let get_nodes prog = 
  List.fold_left (
    fun nodes decl ->
      match decl.top_decl_desc with
	| Node nd -> nd::nodes
	| Consts _ | ImportedNode _ | Open _ -> nodes  
  ) [] prog

let get_consts prog = 
  List.fold_left (
    fun consts decl ->
      match decl.top_decl_desc with
	| Consts clist -> clist@consts
	| Node _ | ImportedNode _ | Open _ -> consts  
  ) [] prog



(************************************************************************)
(*        Renaming                                                      *)

(* applies the renaming function [fvar] to all variables of expression [expr] *)
 let rec expr_replace_var fvar expr =
  { expr with expr_desc = expr_desc_replace_var fvar expr.expr_desc }

 and expr_desc_replace_var fvar expr_desc =
   match expr_desc with
   | Expr_const _ -> expr_desc
   | Expr_ident i -> Expr_ident (fvar i)
   | Expr_array el -> Expr_array (List.map (expr_replace_var fvar) el)
   | Expr_access (e1, d) -> Expr_access (expr_replace_var fvar e1, d)
   | Expr_power (e1, d) -> Expr_power (expr_replace_var fvar e1, d)
   | Expr_tuple el -> Expr_tuple (List.map (expr_replace_var fvar) el)
   | Expr_ite (c, t, e) -> Expr_ite (expr_replace_var fvar c, expr_replace_var fvar t, expr_replace_var fvar e)
   | Expr_arrow (e1, e2)-> Expr_arrow (expr_replace_var fvar e1, expr_replace_var fvar e2) 
   | Expr_fby (e1, e2) -> Expr_fby (expr_replace_var fvar e1, expr_replace_var fvar e2)
   | Expr_pre e' -> Expr_pre (expr_replace_var fvar e')
   | Expr_when (e', i, l)-> Expr_when (expr_replace_var fvar e', fvar i, l)
   | Expr_merge (i, hl) -> Expr_merge (fvar i, List.map (fun (t, h) -> (t, expr_replace_var fvar h)) hl)
   | Expr_appl (i, e', i') -> Expr_appl (i, expr_replace_var fvar e', Utils.option_map (fun (x, l) -> fvar x, l) i')
   | _ -> assert false

(* Applies the renaming function [fvar] to every rhs
   only when the corresponding lhs satisfies predicate [pvar] *)
 let eq_replace_rhs_var pvar fvar eq =
   let pvar l = List.exists pvar l in
   let rec replace lhs rhs =
     { rhs with expr_desc = replace_desc lhs rhs.expr_desc }
   and replace_desc lhs rhs_desc =
     match lhs with
     | []  -> assert false
     | [_] -> if pvar lhs then expr_desc_replace_var fvar rhs_desc else rhs_desc
     | _   ->
       (match rhs_desc with
       | Expr_tuple tl ->
	 Expr_tuple (List.map2 (fun v e -> replace [v] e) lhs tl)
       | Expr_appl (f, arg, None) when Basic_library.is_internal_fun f ->
	 let args = expr_list_of_expr arg in
	 Expr_appl (f, expr_of_expr_list arg.expr_loc (List.map (replace lhs) args), None)
       | Expr_array _
       | Expr_access _
       | Expr_power _
       | Expr_const _
       | Expr_ident _
       | Expr_appl _   ->
	 if pvar lhs
	 then expr_desc_replace_var fvar rhs_desc
	 else rhs_desc
       | Expr_ite (c, t, e)   -> Expr_ite (replace lhs c, replace lhs t, replace lhs e)
       | Expr_arrow (e1, e2)  -> Expr_arrow (replace lhs e1, replace lhs e2) 
       | Expr_fby (e1, e2)    -> Expr_fby (replace lhs e1, replace lhs e2)
       | Expr_pre e'          -> Expr_pre (replace lhs e')
       | Expr_when (e', i, l) -> let i' = if pvar lhs then fvar i else i
				 in Expr_when (replace lhs e', i', l)
       | Expr_merge (i, hl)   -> let i' = if pvar lhs then fvar i else i
				 in Expr_merge (i', List.map (fun (t, h) -> (t, replace lhs h)) hl)
       | _                    -> assert false)
   in { eq with eq_rhs = replace eq.eq_lhs eq.eq_rhs }


 let rec rename_expr  f_node f_var f_const expr =
   { expr with expr_desc = rename_expr_desc f_node f_var f_const expr.expr_desc }
 and rename_expr_desc f_node f_var f_const expr_desc =
   let re = rename_expr  f_node f_var f_const in
   match expr_desc with
   | Expr_const _ -> expr_desc
   | Expr_ident i -> Expr_ident (f_var i)
   | Expr_array el -> Expr_array (List.map re el)
   | Expr_access (e1, d) -> Expr_access (re e1, d)
   | Expr_power (e1, d) -> Expr_power (re e1, d)
   | Expr_tuple el -> Expr_tuple (List.map re el)
   | Expr_ite (c, t, e) -> Expr_ite (re c, re t, re e)
   | Expr_arrow (e1, e2)-> Expr_arrow (re e1, re e2) 
   | Expr_fby (e1, e2) -> Expr_fby (re e1, re e2)
   | Expr_pre e' -> Expr_pre (re e')
   | Expr_when (e', i, l)-> Expr_when (re e', f_var i, l)
   | Expr_merge (i, hl) -> 
     Expr_merge (f_var i, List.map (fun (t, h) -> (t, re h)) hl)
   | Expr_appl (i, e', i') -> 
     Expr_appl (f_node i, re e', Utils.option_map (fun (x, l) -> f_var x, l) i')
   | _ -> assert false

 let rename_node_annot f_node f_var f_const expr  =
   expr
 (* TODO assert false *)

 let rename_expr_annot f_node f_var f_const annot =
   annot
 (* TODO assert false *)

let rename_node f_node f_var f_const nd =
  let rename_var v = { v with var_id = f_var v.var_id } in
  let inputs = List.map rename_var nd.node_inputs in
  let outputs = List.map rename_var nd.node_outputs in
  let locals = List.map rename_var nd.node_locals in
  let gen_calls = List.map (rename_expr f_node f_var f_const) nd.node_gencalls in
  let node_checks = List.map (Dimension.expr_replace_var f_var)  nd.node_checks in
  let node_asserts = List.map 
    (fun a -> 
      { a with assert_expr = rename_expr f_node f_var f_const a.assert_expr } 
    ) nd.node_asserts
  in
  let eqs = List.map 
    (fun eq -> { eq with
      eq_lhs = List.map f_var eq.eq_lhs; 
      eq_rhs = rename_expr f_node f_var f_const eq.eq_rhs
    } ) nd.node_eqs
  in
  let spec = 
    Utils.option_map 
      (fun s -> rename_node_annot f_node f_var f_const s) 
      nd.node_spec 
  in
  let annot =
    Utils.option_map
      (fun s -> rename_expr_annot f_node f_var f_const s) 
      nd.node_annot
  in
  {
    node_id = f_node nd.node_id;
    node_type = nd.node_type;
    node_clock = nd.node_clock;
    node_inputs = inputs;
    node_outputs = outputs;
    node_locals = locals;
    node_gencalls = gen_calls;
    node_checks = node_checks;
    node_asserts = node_asserts;
    node_eqs = eqs;
    node_dec_stateless = nd.node_dec_stateless;
    node_stateless = nd.node_stateless;
    node_spec = spec;
    node_annot = annot;
  }


let rename_const f_const c =
  { c with const_id = f_const c.const_id }
    
let rename_prog f_node f_var f_const prog =
  List.rev (
    List.fold_left (fun accu top ->
      (match top.top_decl_desc with
      | Node nd -> 
	{ top with top_decl_desc = Node (rename_node f_node f_var f_const nd) }
      | Consts c -> 
	{ top with top_decl_desc = Consts (List.map (rename_const f_const) c) }
      | ImportedNode _
      | Open _ -> top)
      ::accu
) [] prog
  )

(**********************************************************************)
(* Pretty printers *)

let pp_decl_type fmt tdecl =
  match tdecl.top_decl_desc with
  | Node nd ->
    fprintf fmt "%s: " nd.node_id;
    Utils.reset_names ();
    fprintf fmt "%a@ " Types.print_ty nd.node_type
  | ImportedNode ind ->
    fprintf fmt "%s: " ind.nodei_id;
    Utils.reset_names ();
    fprintf fmt "%a@ " Types.print_ty ind.nodei_type
  | Consts _ | Open _ -> ()

let pp_prog_type fmt tdecl_list =
  Utils.fprintf_list ~sep:"" pp_decl_type fmt tdecl_list

let pp_decl_clock fmt cdecl =
  match cdecl.top_decl_desc with
  | Node nd ->
    fprintf fmt "%s: " nd.node_id;
    Utils.reset_names ();
    fprintf fmt "%a@ " Clocks.print_ck nd.node_clock
  | ImportedNode ind ->
    fprintf fmt "%s: " ind.nodei_id;
    Utils.reset_names ();
    fprintf fmt "%a@ " Clocks.print_ck ind.nodei_clock
  | Consts _ | Open _ -> ()

let pp_prog_clock fmt prog =
  Utils.fprintf_list ~sep:"" pp_decl_clock fmt prog

let pp_error fmt = function
    Main_not_found ->
      fprintf fmt "Cannot compile node %s: could not find the node definition.@."
	!Options.main_node
  | Main_wrong_kind ->
    fprintf fmt
      "Name %s does not correspond to a (non-imported) node definition.@." 
      !Options.main_node
  | No_main_specified ->
    fprintf fmt "No main node specified@."
  | Unbound_symbol sym ->
    fprintf fmt
      "%s is undefined.@."
      sym
  | Already_bound_symbol sym -> 
    fprintf fmt
      "%s is already defined.@."
      sym

(* filling node table with internal functions *)
let vdecls_of_typ_ck cpt ty =
  let loc = Location.dummy_loc in
  List.map
    (fun _ -> incr cpt;
              let name = sprintf "_var_%d" !cpt in
              mkvar_decl loc (name, mktyp loc Tydec_any, mkclock loc Ckdec_any, false))
    (Types.type_list_of_type ty)

let mk_internal_node id =
  let spec = None in
  let ty = Env.lookup_value Basic_library.type_env id in
  let ck = Env.lookup_value Basic_library.clock_env id in
  let (tin, tout) = Types.split_arrow ty in
  (*eprintf "internal fun %s: %d -> %d@." id (List.length (Types.type_list_of_type tin)) (List.length (Types.type_list_of_type tout));*)
  let cpt = ref (-1) in
  mktop_decl Location.dummy_loc
    (ImportedNode
       {nodei_id = id;
	nodei_type = ty;
	nodei_clock = ck;
	nodei_inputs = vdecls_of_typ_ck cpt tin;
	nodei_outputs = vdecls_of_typ_ck cpt tout;
	nodei_stateless = Types.get_static_value ty <> None;
	nodei_spec = spec;
	nodei_prototype = None;
       	nodei_in_lib = None;
       })

let add_internal_funs () =
  List.iter
    (fun id -> let nd = mk_internal_node id in Hashtbl.add node_table id nd)
    Basic_library.internal_funs

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
