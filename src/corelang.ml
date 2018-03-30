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

open Format
open Lustre_types
open Machine_code_types
(*open Dimension*)


exception Error of Location.t * Error.error_kind

module VDeclModule =
struct (* Node module *)
  type t = var_decl
  let compare v1 v2 = compare v1.var_id v2.var_id
end

module VMap = Map.Make(VDeclModule)

module VSet : Set.S with type elt = var_decl = Set.Make(VDeclModule)

let dummy_type_dec = {ty_dec_desc=Tydec_any; ty_dec_loc=Location.dummy_loc}

let dummy_clock_dec = {ck_dec_desc=Ckdec_any; ck_dec_loc=Location.dummy_loc}



(************************************************************)
(* *)

let mktyp loc d =
  { ty_dec_desc = d; ty_dec_loc = loc }

let mkclock loc d =
  { ck_dec_desc = d; ck_dec_loc = loc }

let mkvar_decl loc ?(orig=false) (id, ty_dec, ck_dec, is_const, value, parentid) =
  assert (value = None || is_const);
  { var_id = id;
    var_orig = orig;
    var_dec_type = ty_dec;
    var_dec_clock = ck_dec;
    var_dec_const = is_const;
    var_dec_value = value;
    var_parent_nodeid = parentid;
    var_type = Types.new_var ();
    var_clock = Clocks.new_var true;
    var_loc = loc }

let dummy_var_decl name typ =
  {
    var_id = name;
    var_orig = false;
    var_dec_type = dummy_type_dec;
    var_dec_clock = dummy_clock_dec;
    var_dec_const = false;
    var_dec_value = None;
    var_parent_nodeid = None;
    var_type =  typ;
    var_clock = Clocks.new_ck Clocks.Cvar true;
    var_loc = Location.dummy_loc
  }

let mkexpr loc d =
  { expr_tag = Utils.new_tag ();
    expr_desc = d;
    expr_type = Types.new_var ();
    expr_clock = Clocks.new_var true;
    expr_delay = Delay.new_var ();
    expr_annot = None;
    expr_loc = loc }

let var_decl_of_const ?(parentid=None) c =
  { var_id = c.const_id;
    var_orig = true;
    var_dec_type = { ty_dec_loc = c.const_loc; ty_dec_desc = Tydec_any };
    var_dec_clock = { ck_dec_loc = c.const_loc; ck_dec_desc = Ckdec_any };
    var_dec_const = true;
    var_dec_value = None;
    var_parent_nodeid = parentid;
    var_type = c.const_type;
    var_clock = Clocks.new_var false;
    var_loc = c.const_loc }

let mk_new_name used id =
  let rec new_name name cpt =
    if used name
    then new_name (sprintf "_%s_%i" id cpt) (cpt+1)
    else name
  in new_name id 1

let mkeq loc (lhs, rhs) =
  { eq_lhs = lhs;
    eq_rhs = rhs;
    eq_loc = loc }

let mkassert loc expr =
  { assert_loc = loc;
    assert_expr = expr
  }

let mktop_decl loc own itf d =
  { top_decl_desc = d; top_decl_loc = loc; top_decl_owner = own; top_decl_itf = itf }

let mkpredef_call loc funname args =
  mkexpr loc (Expr_appl (funname, mkexpr loc (Expr_tuple args), None))

let is_clock_dec_type cty =
  match cty with
  | Tydec_clock _ -> true
  | _             -> false

let const_of_top top_decl =
  match top_decl.top_decl_desc with
  | Const c -> c
  | _ -> assert false

let node_of_top top_decl =
  match top_decl.top_decl_desc with
  | Node nd -> nd
  | _ -> raise Not_found

let imported_node_of_top top_decl =
  match top_decl.top_decl_desc with
  | ImportedNode ind -> ind
  | _ -> assert false

let typedef_of_top top_decl =
  match top_decl.top_decl_desc with
  | TypeDef tdef -> tdef
  | _ -> assert false

let dependency_of_top top_decl =
  match top_decl.top_decl_desc with
  | Open (local, dep) -> (local, dep)
  | _ -> assert false

let consts_of_enum_type top_decl =
  match top_decl.top_decl_desc with
  | TypeDef tdef ->
    (match tdef.tydef_desc with
    | Tydec_enum tags ->
       List.map
	 (fun tag ->
	   let cdecl = {
	     const_id = tag;
	     const_loc = top_decl.top_decl_loc;
	     const_value = Const_tag tag;
	     const_type = Type_predef.type_const tdef.tydef_id
	   } in
	   { top_decl with top_decl_desc = Const cdecl })
	 tags
     | _               -> [])
  | _ -> assert false

(************************************************************)
(*   Eexpr functions *)
(************************************************************)

let merge_node_annot ann1 ann2 =
  { requires = ann1.requires @ ann2.requires;
    ensures = ann1.ensures @ ann2.ensures;
    behaviors = ann1.behaviors @ ann2.behaviors;
    spec_loc = ann1.spec_loc
  }

let mkeexpr loc expr =
  { eexpr_tag = Utils.new_tag ();
    eexpr_qfexpr = expr;
    eexpr_quantifiers = [];
    eexpr_type = Types.new_var ();
    eexpr_clock = Clocks.new_var true;
    eexpr_normalized = None;
    eexpr_loc = loc }

let extend_eexpr q e = { e with eexpr_quantifiers = q@e.eexpr_quantifiers }

(*
let mkepredef_call loc funname args =
  mkeexpr loc (EExpr_appl (funname, mkeexpr loc (EExpr_tuple args), None))

let mkepredef_unary_call loc funname arg =
  mkeexpr loc (EExpr_appl (funname, arg, None))
*)

let merge_expr_annot ann1 ann2 =
  match ann1, ann2 with
    | None, None -> assert false
    | Some _, None -> ann1
    | None, Some _ -> ann2
    | Some ann1, Some ann2 -> Some {
      annots = ann1.annots @ ann2.annots;
      annot_loc = ann1.annot_loc
    }

let update_expr_annot node_id e annot =
  List.iter (fun (key, _) -> 
    Annotations.add_expr_ann node_id e.expr_tag key
  ) annot.annots;
  e.expr_annot <- merge_expr_annot e.expr_annot (Some annot);
  e


let mkinstr ?lustre_expr ?lustre_eq i =
  {
    instr_desc = i;
    (* lustre_expr = lustre_expr; *)
    lustre_eq = lustre_eq;
  }

let get_instr_desc i = i.instr_desc
let update_instr_desc i id = { i with instr_desc = id }

(***********************************************************)
(* Fast access to nodes, by name *)
let (node_table : (ident, top_decl) Hashtbl.t) = Hashtbl.create 30
let consts_table = Hashtbl.create 30

let print_node_table fmt () =
  begin
    Format.fprintf fmt "{ /* node table */@.";
    Hashtbl.iter (fun id nd ->
      Format.fprintf fmt "%s |-> %a"
	id
	Printers.pp_short_decl nd
    ) node_table;
    Format.fprintf fmt "}@."
  end

let print_consts_table fmt () =
  begin
    Format.fprintf fmt "{ /* consts table */@.";
    Hashtbl.iter (fun id const ->
      Format.fprintf fmt "%s |-> %a"
	id
	Printers.pp_const_decl (const_of_top const)
    ) consts_table;
    Format.fprintf fmt "}@."
  end

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

let mktop = mktop_decl Location.dummy_loc !Options.dest_dir false

let top_int_type = mktop (TypeDef {tydef_id = "int"; tydef_desc = Tydec_int})
let top_bool_type = mktop (TypeDef {tydef_id = "bool"; tydef_desc = Tydec_bool})
(* let top_float_type = mktop (TypeDef {tydef_id = "float"; tydef_desc = Tydec_float}) *)
let top_real_type = mktop (TypeDef {tydef_id = "real"; tydef_desc = Tydec_real})

let type_table =
  Utils.create_hashtable 20 [
    Tydec_int  , top_int_type;
    Tydec_bool , top_bool_type;
    (* Tydec_float, top_float_type; *)
    Tydec_real , top_real_type
  ]

let print_type_table fmt () =
  begin
    Format.fprintf fmt "{ /* type table */@.";
    Hashtbl.iter (fun tydec tdef ->
      Format.fprintf fmt "%a |-> %a"
	Printers.pp_var_type_dec_desc tydec
	Printers.pp_typedef (typedef_of_top tdef)
    ) type_table;
    Format.fprintf fmt "}@."
  end

let rec is_user_type typ =
  match typ with
  | Tydec_int | Tydec_bool | Tydec_real 
  (* | Tydec_float *) | Tydec_any | Tydec_const _ -> false
  | Tydec_clock typ' -> is_user_type typ'
  | _ -> true

let get_repr_type typ =
  let typ_def = (typedef_of_top (Hashtbl.find type_table typ)).tydef_desc in
  if is_user_type typ_def then typ else typ_def

let rec coretype_equal ty1 ty2 =
  let res =
  match ty1, ty2 with
  | Tydec_any           , _
  | _                   , Tydec_any             -> assert false
  | Tydec_const _       , Tydec_const _         -> get_repr_type ty1 = get_repr_type ty2
  | Tydec_const _       , _                     -> let ty1' = (typedef_of_top (Hashtbl.find type_table ty1)).tydef_desc
	       					   in (not (is_user_type ty1')) && coretype_equal ty1' ty2
  | _                   , Tydec_const _         -> coretype_equal ty2 ty1
  | Tydec_int           , Tydec_int
  | Tydec_real          , Tydec_real
  (* | Tydec_float         , Tydec_float *)
  | Tydec_bool          , Tydec_bool            -> true
  | Tydec_clock ty1     , Tydec_clock ty2       -> coretype_equal ty1 ty2
  | Tydec_array (d1,ty1), Tydec_array (d2, ty2) -> Dimension.is_eq_dimension d1 d2 && coretype_equal ty1 ty2
  | Tydec_enum tl1      , Tydec_enum tl2        -> List.sort compare tl1 = List.sort compare tl2
  | Tydec_struct fl1    , Tydec_struct fl2      ->
       List.length fl1 = List.length fl2
    && List.for_all2 (fun (f1, t1) (f2, t2) -> f1 = f2 && coretype_equal t1 t2)
      (List.sort (fun (f1,_) (f2,_) -> compare f1 f2) fl1)
      (List.sort (fun (f1,_) (f2,_) -> compare f1 f2) fl2)
  | _                                  -> false
  in ((*Format.eprintf "coretype_equal %a %a = %B@." Printers.pp_var_type_dec_desc ty1 Printers.pp_var_type_dec_desc ty2 res;*) res)

let tag_true = "true"
let tag_false = "false"
let tag_default = "default"

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
   tag_true, top_bool_type;
   tag_false, top_bool_type
  ]

(* To guarantee uniqueness of fields in struct types *)
let field_table =
  Utils.create_hashtable 20 [
  ]

let get_enum_type_tags cty =
(*Format.eprintf "get_enum_type_tags %a@." Printers.pp_var_type_dec_desc cty;*)
 match cty with
 | Tydec_bool    -> [tag_true; tag_false]
 | Tydec_const _ -> (match (typedef_of_top (Hashtbl.find type_table cty)).tydef_desc with
                     | Tydec_enum tl -> tl
                     | _             -> assert false)
 | _            -> assert false

let get_struct_type_fields cty =
 match cty with
 | Tydec_const _ -> (match (typedef_of_top (Hashtbl.find type_table cty)).tydef_desc with
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

(* Generate a new ident expression from a declared variable *)
let expr_of_vdecl v =
  { expr_tag = Utils.new_tag ();
    expr_desc = Expr_ident v.var_id;
    expr_type = v.var_type;
    expr_clock = v.var_clock;
    expr_delay = Delay.new_var ();
    expr_annot = None;
    expr_loc = v.var_loc }

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
  | Expr_tuple elist -> elist
  | _                -> [expr]

let expr_of_expr_list loc elist =
 match elist with
 | [t]  -> { t with expr_loc = loc }
 | t::_ ->
    let tlist = List.map (fun e -> e.expr_type) elist in
    let clist = List.map (fun e -> e.expr_clock) elist in
    { t with expr_desc = Expr_tuple elist;
	     expr_type = Type_predef.type_tuple tlist;
	     expr_clock = Clock_predef.ck_tuple clist;
	     expr_tag = Utils.new_tag ();
	     expr_loc = loc }
 | _    -> assert false

let call_of_expr expr =
 match expr.expr_desc with
 | Expr_appl (f, args, r) -> (f, expr_list_of_expr args, r)
 | _                      -> assert false

    
(* Conversion from dimension expr to standard expr, for the purpose of printing, typing, etc... *)
let rec expr_of_dimension dim =
  let open Dimension in
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
 | Dunivar          -> (Format.eprintf "internal error: Corelang.expr_of_dimension %a@." Dimension.pp_dimension dim;
			assert false)

let dimension_of_const loc const =
  let open Dimension in
 match const with
 | Const_int i                                    -> mkdim_int loc i
 | Const_tag t when t = tag_true || t = tag_false -> mkdim_bool loc (t = tag_true)
 | _                                              -> raise InvalidDimension

(* Conversion from standard expr to dimension expr, for the purpose of injecting static call arguments 
   into dimension expressions *)
let rec dimension_of_expr expr =
  let open Dimension in
  match expr.expr_desc with
  | Expr_const c  -> dimension_of_const expr.expr_loc c
  | Expr_ident id -> mkdim_ident expr.expr_loc id
  | Expr_appl (f, args, None) when Basic_library.is_expr_internal_fun expr ->
      let k = Types.get_static_value (Env.lookup_value Basic_library.type_env f) in
      if k = None then raise InvalidDimension;
      mkdim_appl expr.expr_loc f (List.map dimension_of_expr (expr_list_of_expr args))
  | Expr_ite (i, t, e)        ->
      mkdim_ite expr.expr_loc (dimension_of_expr i) (dimension_of_expr t) (dimension_of_expr e)
  | _ -> raise InvalidDimension (* not a simple dimension expression *)


let sort_handlers hl =
 List.sort (fun (t, _) (t', _) -> compare t t') hl

let num_10 = Num.num_of_int 10
  
let rec is_eq_const c1 c2 =
  match c1, c2 with
  | Const_real (n1, i1, _), Const_real (n2, i2, _)
    -> Num.(let n1 = n1 // (num_10 **/ (num_of_int i1)) in
	    let n2 = n2 // (num_10 **/ (num_of_int i2)) in
	    eq_num n1 n2)
  | Const_struct lcl1, Const_struct lcl2
    -> List.length lcl1 = List.length lcl2
    && List.for_all2 (fun (l1, c1) (l2, c2) -> l1 = l2 && is_eq_const c1 c2) lcl1 lcl2
  | _  -> c1 = c2

let rec is_eq_expr e1 e2 = match e1.expr_desc, e2.expr_desc with
  | Expr_const c1, Expr_const c2 -> is_eq_const c1 c2
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
  | Expr_power (e1, i1), Expr_power (e2, i2)
  | Expr_access (e1, i1), Expr_access (e2, i2) -> is_eq_expr e1 e2 && is_eq_expr (expr_of_dimension i1) (expr_of_dimension i2)
  | _ -> false

let get_node_vars nd =
  nd.node_inputs @ nd.node_locals @ nd.node_outputs

let mk_new_node_name nd id =
  let used_vars = get_node_vars nd in
  let used v = List.exists (fun vdecl -> vdecl.var_id = v) used_vars in
  mk_new_name used id

let get_var id var_list =
  List.find (fun v -> v.var_id = id) var_list

let get_node_var id node =
  try
    get_var id (get_node_vars node)
  with Not_found -> begin
    (* Format.eprintf "Unable to find variable %s in node %s@.@?" id node.node_id; *)
    raise Not_found
  end


let get_node_eqs =
  let get_eqs stmts =
    List.fold_right
      (fun stmt (res_eq, res_aut) ->
	match stmt with
	| Eq eq -> eq :: res_eq, res_aut
	| Aut aut -> res_eq, aut::res_aut)
      stmts
      ([], []) in
  let table_eqs = Hashtbl.create 23 in
  (fun nd ->
    try
      let (old, res) = Hashtbl.find table_eqs nd.node_id
      in if old == nd.node_stmts then res else raise Not_found
    with Not_found -> 
      let res = get_eqs nd.node_stmts in
      begin
	Hashtbl.replace table_eqs nd.node_id (nd.node_stmts, res);
	res
      end)

let get_node_eq id node =
  let eqs, auts = get_node_eqs node in
  try
    List.find (fun eq -> List.mem id eq.eq_lhs) eqs
  with
    Not_found -> (* Shall be defined in automata auts *) raise Not_found
      
let get_nodes prog = 
  List.fold_left (
    fun nodes decl ->
      match decl.top_decl_desc with
	| Node _ -> decl::nodes
	| Const _ | ImportedNode _ | Open _ | TypeDef _ -> nodes  
  ) [] prog

let get_imported_nodes prog = 
  List.fold_left (
    fun nodes decl ->
      match decl.top_decl_desc with
	| ImportedNode _ -> decl::nodes
	| Const _ | Node _ | Open _ | TypeDef _-> nodes  
  ) [] prog

let get_consts prog = 
  List.fold_right (
    fun decl consts ->
      match decl.top_decl_desc with
	| Const _ -> decl::consts
	| Node _ | ImportedNode _ | Open _ | TypeDef _ -> consts  
  ) prog []

let get_typedefs prog = 
  List.fold_right (
    fun decl types ->
      match decl.top_decl_desc with
	| TypeDef _ -> decl::types
	| Node _ | ImportedNode _ | Open _ | Const _ -> types  
  ) prog []

let get_dependencies prog =
  List.fold_right (
    fun decl deps ->
      match decl.top_decl_desc with
	| Open _ -> decl::deps
	| Node _ | ImportedNode _ | TypeDef _ | Const _ -> deps  
  ) prog []

let get_node_interface nd =
 {nodei_id = nd.node_id;
  nodei_type = nd.node_type;
  nodei_clock = nd.node_clock;
  nodei_inputs = nd.node_inputs;
  nodei_outputs = nd.node_outputs;
  nodei_stateless = nd.node_dec_stateless;
  nodei_spec = nd.node_spec;
  (* nodei_annot = nd.node_annot; *)
  nodei_prototype = None;
  nodei_in_lib = [];
 }

(************************************************************************)
(*        Renaming                                                      *)

let rec rename_static rename cty =
 match cty with
 | Tydec_array (d, cty') -> Tydec_array (Dimension.expr_replace_expr rename d, rename_static rename cty')
 | Tydec_clock cty       -> Tydec_clock (rename_static rename cty)
 | Tydec_struct fl       -> Tydec_struct (List.map (fun (f, cty) -> f, rename_static rename cty) fl)
 | _                      -> cty

let rec rename_carrier rename cck =
 match cck with
 | Ckdec_bool cl -> Ckdec_bool (List.map (fun (c, l) -> rename c, l) cl)
 | _             -> cck

 (*Format.eprintf "Types.rename_static %a = %a@." print_ty ty print_ty res; res*)

(* applies the renaming function [fvar] to all variables of expression [expr] *)
 (* let rec expr_replace_var fvar expr = *)
 (*  { expr with expr_desc = expr_desc_replace_var fvar expr.expr_desc } *)

 (* and expr_desc_replace_var fvar expr_desc = *)
 (*   match expr_desc with *)
 (*   | Expr_const _ -> expr_desc *)
 (*   | Expr_ident i -> Expr_ident (fvar i) *)
 (*   | Expr_array el -> Expr_array (List.map (expr_replace_var fvar) el) *)
 (*   | Expr_access (e1, d) -> Expr_access (expr_replace_var fvar e1, d) *)
 (*   | Expr_power (e1, d) -> Expr_power (expr_replace_var fvar e1, d) *)
 (*   | Expr_tuple el -> Expr_tuple (List.map (expr_replace_var fvar) el) *)
 (*   | Expr_ite (c, t, e) -> Expr_ite (expr_replace_var fvar c, expr_replace_var fvar t, expr_replace_var fvar e) *)
 (*   | Expr_arrow (e1, e2)-> Expr_arrow (expr_replace_var fvar e1, expr_replace_var fvar e2)  *)
 (*   | Expr_fby (e1, e2) -> Expr_fby (expr_replace_var fvar e1, expr_replace_var fvar e2) *)
 (*   | Expr_pre e' -> Expr_pre (expr_replace_var fvar e') *)
 (*   | Expr_when (e', i, l)-> Expr_when (expr_replace_var fvar e', fvar i, l) *)
 (*   | Expr_merge (i, hl) -> Expr_merge (fvar i, List.map (fun (t, h) -> (t, expr_replace_var fvar h)) hl) *)
 (*   | Expr_appl (i, e', i') -> Expr_appl (i, expr_replace_var fvar e', Utils.option_map (expr_replace_var fvar) i') *)



 let rec rename_expr  f_node f_var expr =
   { expr with expr_desc = rename_expr_desc f_node f_var expr.expr_desc }
 and rename_expr_desc f_node f_var expr_desc =
   let re = rename_expr  f_node f_var in
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
     Expr_appl (f_node i, re e', Utils.option_map re i')

 let rename_dec_type f_node f_var t = assert false (*
						     Types.rename_dim_type (Dimension.rename f_node f_var) t*)

 let rename_dec_clock f_node f_var c = assert false (* 
					  Clocks.rename_clock_expr f_var c*)
   
 let rename_var f_node f_var v = {
   v with
     var_id = f_var v.var_id;
     var_dec_type = rename_dec_type f_node f_var v.var_type;
     var_dec_clock = rename_dec_clock f_node f_var v.var_clock
 } 

 let rename_vars f_node f_var = List.map (rename_var f_node f_var) 

 let rec rename_eq f_node f_var eq = { eq with
   eq_lhs = List.map f_var eq.eq_lhs; 
   eq_rhs = rename_expr f_node f_var eq.eq_rhs
 } 
 and rename_handler f_node f_var  h = {h with
   hand_state = f_var h.hand_state;
   hand_unless = List.map (
     fun (l,e,b,id) -> l, rename_expr f_node f_var e, b, f_var id
   ) h.hand_unless;
   hand_until = List.map (
     fun (l,e,b,id) -> l, rename_expr f_node f_var e, b, f_var id
   ) h.hand_until;
   hand_locals = rename_vars f_node f_var h.hand_locals;
   hand_stmts = rename_stmts f_node f_var h.hand_stmts;
   hand_annots = rename_annots f_node f_var h.hand_annots;
   
 } 
 and rename_aut f_node f_var  aut = { aut with
   aut_id = f_var aut.aut_id;
   aut_handlers = List.map (rename_handler f_node f_var) aut.aut_handlers;
 }
 and rename_stmts f_node f_var stmts = List.map (fun stmt -> match stmt with
   | Eq eq -> Eq (rename_eq f_node f_var eq)
   | Aut at -> Aut (rename_aut f_node f_var at))
   stmts
 and rename_annotl f_node f_var  annots = 
   List.map 
     (fun (key, value) -> key, rename_eexpr f_node f_var value) 
     annots
 and rename_annot f_node f_var annot =
   { annot with annots = rename_annotl f_node f_var annot.annots }
 and rename_annots f_node f_var annots =
   List.map (rename_annot f_node f_var) annots
and rename_eexpr f_node f_var ee =
   { ee with
     eexpr_tag = Utils.new_tag ();
     eexpr_qfexpr = rename_expr f_node f_var ee.eexpr_qfexpr;
     eexpr_quantifiers = List.map (fun (typ,vdecls) -> typ, rename_vars f_node f_var vdecls) ee.eexpr_quantifiers;
     eexpr_normalized = Utils.option_map 
       (fun (vdecl, eqs, vdecls) ->
	 rename_var f_node f_var vdecl,
	 List.map (rename_eq f_node f_var) eqs,
	 rename_vars f_node f_var vdecls
       ) ee.eexpr_normalized;
     
   }
 
     
     
   
 let rename_node f_node f_var nd =
   let rename_var = rename_var f_node f_var in
   let rename_expr = rename_expr f_node f_var in
   let rename_stmts = rename_stmts f_node f_var in
   let inputs = List.map rename_var nd.node_inputs in
   let outputs = List.map rename_var nd.node_outputs in
   let locals = List.map rename_var nd.node_locals in
   let gen_calls = List.map rename_expr nd.node_gencalls in
   let node_checks = List.map (Dimension.rename f_node f_var)  nd.node_checks in
   let node_asserts = List.map 
     (fun a -> 
       {a with assert_expr = 
	   let expr = a.assert_expr in
	   rename_expr expr})
     nd.node_asserts
   in
   let node_stmts = rename_stmts nd.node_stmts

     
   in
   let spec = 
     Utils.option_map 
       (fun s -> assert false; (*rename_node_annot f_node f_var s*) ) (* TODO: implement! *) 
       nd.node_spec 
   in
   let annot = rename_annots f_node f_var nd.node_annot in
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
     node_stmts = node_stmts;
     node_dec_stateless = nd.node_dec_stateless;
     node_stateless = nd.node_stateless;
     node_spec = spec;
     node_annot = annot;
   }


let rename_const f_const c =
  { c with const_id = f_const c.const_id }

let rename_typedef f_var t =
  match t.tydef_desc with
  | Tydec_enum tags -> { t with tydef_desc = Tydec_enum (List.map f_var tags) }
  | _               -> t

let rename_prog f_node f_var f_const prog =
  List.rev (
    List.fold_left (fun accu top ->
      (match top.top_decl_desc with
      | Node nd -> 
	 { top with top_decl_desc = Node (rename_node f_node f_var nd) }
      | Const c -> 
	 { top with top_decl_desc = Const (rename_const f_const c) }
      | TypeDef tdef ->
	 { top with top_decl_desc = TypeDef (rename_typedef f_var tdef) }
      | ImportedNode _
      | Open _       -> top)
      ::accu
) [] prog
		   )

(* Applies the renaming function [fvar] to every rhs
   only when the corresponding lhs satisfies predicate [pvar] *)
 let eq_replace_rhs_var pvar fvar eq =
   let pvar l = List.exists pvar l in
   let rec replace lhs rhs =
     { rhs with expr_desc =
     match lhs with
     | []  -> assert false
     | [_] -> if pvar lhs then rename_expr_desc (fun x -> x) fvar rhs.expr_desc else rhs.expr_desc
     | _   ->
       (match rhs.expr_desc with
       | Expr_tuple tl ->
	 Expr_tuple (List.map2 (fun v e -> replace [v] e) lhs tl)
       | Expr_appl (f, arg, None) when Basic_library.is_expr_internal_fun rhs ->
	 let args = expr_list_of_expr arg in
	 Expr_appl (f, expr_of_expr_list arg.expr_loc (List.map (replace lhs) args), None)
       | Expr_array _
       | Expr_access _
       | Expr_power _
       | Expr_const _
       | Expr_ident _
       | Expr_appl _   ->
	 if pvar lhs
	 then rename_expr_desc (fun x -> x) fvar rhs.expr_desc
	 else rhs.expr_desc
       | Expr_ite (c, t, e)   -> Expr_ite (replace lhs c, replace lhs t, replace lhs e)
       | Expr_arrow (e1, e2)  -> Expr_arrow (replace lhs e1, replace lhs e2) 
       | Expr_fby (e1, e2)    -> Expr_fby (replace lhs e1, replace lhs e2)
       | Expr_pre e'          -> Expr_pre (replace lhs e')
       | Expr_when (e', i, l) -> let i' = if pvar lhs then fvar i else i
				 in Expr_when (replace lhs e', i', l)
       | Expr_merge (i, hl)   -> let i' = if pvar lhs then fvar i else i
				 in Expr_merge (i', List.map (fun (t, h) -> (t, replace lhs h)) hl)
       )
     }
   in { eq with eq_rhs = replace eq.eq_lhs eq.eq_rhs }

    
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
  | Const _ | Open _ | TypeDef _ -> ()

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
  | Const _ | Open _ | TypeDef _ -> ()

let pp_prog_clock fmt prog =
  Utils.fprintf_list ~sep:"" pp_decl_clock fmt prog


(* filling node table with internal functions *)
let vdecls_of_typ_ck cpt ty =
  let loc = Location.dummy_loc in
  List.map
    (fun _ -> incr cpt;
              let name = sprintf "_var_%d" !cpt in
              mkvar_decl loc (name, mktyp loc Tydec_any, mkclock loc Ckdec_any, false, None, None))
    (Types.type_list_of_type ty)

let mk_internal_node id =
  let spec = None in
  let ty = Env.lookup_value Basic_library.type_env id in
  let ck = Env.lookup_value Basic_library.clock_env id in
  let (tin, tout) = Types.split_arrow ty in
  (*eprintf "internal fun %s: %d -> %d@." id (List.length (Types.type_list_of_type tin)) (List.length (Types.type_list_of_type tout));*)
  let cpt = ref (-1) in
  mktop
    (ImportedNode
       {nodei_id = id;
	nodei_type = ty;
	nodei_clock = ck;
	nodei_inputs = vdecls_of_typ_ck cpt tin;
	nodei_outputs = vdecls_of_typ_ck cpt tout;
	nodei_stateless = Types.get_static_value ty <> None;
	nodei_spec = spec;
	(* nodei_annot = []; *)
	nodei_prototype = None;
       	nodei_in_lib = [];
       })

let add_internal_funs () =
  List.iter
    (fun id -> let nd = mk_internal_node id in Hashtbl.add node_table id nd)
    Basic_library.internal_funs



(* Replace any occurence of a var in vars_to_replace by its associated
   expression in defs until e does not contain any such variables *)
let rec substitute_expr vars_to_replace defs e =
  let se = substitute_expr vars_to_replace defs in
  { e with expr_desc = 
      let ed = e.expr_desc in
      match ed with
      | Expr_const _ -> ed
      | Expr_array el -> Expr_array (List.map se el)
      | Expr_access (e1, d) -> Expr_access (se e1, d)
      | Expr_power (e1, d) -> Expr_power (se e1, d)
      | Expr_tuple el -> Expr_tuple (List.map se el)
      | Expr_ite (c, t, e) -> Expr_ite (se c, se t, se e)
      | Expr_arrow (e1, e2)-> Expr_arrow (se e1, se e2) 
      | Expr_fby (e1, e2) -> Expr_fby (se e1, se e2)
      | Expr_pre e' -> Expr_pre (se e')
      | Expr_when (e', i, l)-> Expr_when (se e', i, l)
      | Expr_merge (i, hl) -> Expr_merge (i, List.map (fun (t, h) -> (t, se h)) hl)
      | Expr_appl (i, e', i') -> Expr_appl (i, se e', i')
      | Expr_ident i -> 
	if List.exists (fun v -> v.var_id = i) vars_to_replace then (
	  let eq_i eq = eq.eq_lhs = [i] in
	  if List.exists eq_i defs then
	    let sub = List.find eq_i defs in
	    let sub' = se sub.eq_rhs in
	    sub'.expr_desc
	  else 
	    assert false
	)
	else
	  ed

  }
  
 let rec expr_to_eexpr  expr =
   { eexpr_tag = expr.expr_tag;
     eexpr_qfexpr = expr;
     eexpr_quantifiers = [];
     eexpr_type = expr.expr_type;
     eexpr_clock = expr.expr_clock;
     eexpr_loc = expr.expr_loc;
     eexpr_normalized = None
   }
 (* and expr_desc_to_eexpr_desc expr_desc = *)
 (*   let conv = expr_to_eexpr in *)
 (*   match expr_desc with *)
 (*   | Expr_const c -> EExpr_const (match c with *)
 (*     | Const_int x -> EConst_int x  *)
 (*     | Const_real x -> EConst_real x  *)
 (*     | Const_float x -> EConst_float x  *)
 (*     | Const_tag x -> EConst_tag x  *)
 (*     | _ -> assert false *)

 (*   ) *)
 (*   | Expr_ident i -> EExpr_ident i *)
 (*   | Expr_tuple el -> EExpr_tuple (List.map conv el) *)

 (*   | Expr_arrow (e1, e2)-> EExpr_arrow (conv e1, conv e2)  *)
 (*   | Expr_fby (e1, e2) -> EExpr_fby (conv e1, conv e2) *)
 (*   | Expr_pre e' -> EExpr_pre (conv e') *)
 (*   | Expr_appl (i, e', i') ->  *)
 (*     EExpr_appl  *)
 (*       (i, conv e', match i' with None -> None | Some(id, _) -> Some id) *)

 (*   | Expr_when _ *)
 (*   | Expr_merge _ -> assert false *)
 (*   | Expr_array _  *)
 (*   | Expr_access _  *)
 (*   | Expr_power _  -> assert false *)
 (*   | Expr_ite (c, t, e) -> assert false  *)
 (*   | _ -> assert false *)
      
     
let rec get_expr_calls nodes e =
  let get_calls = get_expr_calls nodes in
  match e.expr_desc with
  | Expr_const _ 
   | Expr_ident _ -> Utils.ISet.empty
   | Expr_tuple el
   | Expr_array el -> List.fold_left (fun accu e -> Utils.ISet.union accu (get_calls e)) Utils.ISet.empty el
   | Expr_pre e1 
   | Expr_when (e1, _, _) 
   | Expr_access (e1, _) 
   | Expr_power (e1, _) -> get_calls e1
   | Expr_ite (c, t, e) -> Utils.ISet.union (Utils.ISet.union (get_calls c) (get_calls t)) (get_calls e) 
   | Expr_arrow (e1, e2) 
   | Expr_fby (e1, e2) -> Utils.ISet.union (get_calls e1) (get_calls e2)
   | Expr_merge (_, hl) -> List.fold_left (fun accu (_, h) -> Utils.ISet.union accu (get_calls h)) Utils.ISet.empty  hl
   | Expr_appl (i, e', i') -> 
     if Basic_library.is_expr_internal_fun e then 
       (get_calls e') 
     else
       let calls =  Utils.ISet.add i (get_calls e') in
       let test = (fun n -> match n.top_decl_desc with Node nd -> nd.node_id = i | _ -> false) in
       if List.exists test nodes then
	 match (List.find test nodes).top_decl_desc with
	 | Node nd -> Utils.ISet.union (get_node_calls nodes nd) calls
	 | _ -> assert false
       else 
	 calls

and get_eq_calls nodes eq =
  get_expr_calls nodes eq.eq_rhs
and get_aut_handler_calls nodes h =
  List.fold_left (fun accu stmt -> match stmt with
  | Eq eq -> Utils.ISet.union (get_eq_calls nodes eq) accu
  | Aut aut' ->  Utils.ISet.union (get_aut_calls nodes aut') accu
  ) Utils.ISet.empty h.hand_stmts 
and get_aut_calls nodes aut =
  List.fold_left (fun accu h -> Utils.ISet.union (get_aut_handler_calls nodes h) accu)
    Utils.ISet.empty aut.aut_handlers
and get_node_calls nodes node =
  let eqs, auts = get_node_eqs node in
  let aut_calls =
    List.fold_left
      (fun accu aut -> Utils.ISet.union (get_aut_calls nodes aut) accu)
      Utils.ISet.empty auts
  in
  List.fold_left
    (fun accu eq -> Utils.ISet.union (get_eq_calls nodes eq) accu)
    aut_calls eqs

let get_expr_vars e =
  let rec get_expr_vars vars e =
    get_expr_desc_vars vars e.expr_desc
  and get_expr_desc_vars vars expr_desc =
    (*Format.eprintf "get_expr_desc_vars expr=%a@." Printers.pp_expr (mkexpr Location.dummy_loc expr_desc);*)
  match expr_desc with
  | Expr_const _ -> vars
  | Expr_ident x -> Utils.ISet.add x vars
  | Expr_tuple el
  | Expr_array el -> List.fold_left get_expr_vars vars el
  | Expr_pre e1 -> get_expr_vars vars e1
  | Expr_when (e1, c, _) -> get_expr_vars (Utils.ISet.add c vars) e1 
  | Expr_access (e1, d) 
  | Expr_power (e1, d)   -> List.fold_left get_expr_vars vars [e1; expr_of_dimension d]
  | Expr_ite (c, t, e) -> List.fold_left get_expr_vars vars [c; t; e]
  | Expr_arrow (e1, e2) 
  | Expr_fby (e1, e2) -> List.fold_left get_expr_vars vars [e1; e2]
  | Expr_merge (c, hl) -> List.fold_left (fun vars (_, h) -> get_expr_vars vars h) (Utils.ISet.add c vars) hl
  | Expr_appl (_, arg, None)   -> get_expr_vars vars arg
  | Expr_appl (_, arg, Some r) -> List.fold_left get_expr_vars vars [arg; r]
  in
  get_expr_vars Utils.ISet.empty e 

let rec expr_has_arrows e =
  expr_desc_has_arrows e.expr_desc
and expr_desc_has_arrows expr_desc =
  match expr_desc with
  | Expr_const _ 
  | Expr_ident _ -> false
  | Expr_tuple el
  | Expr_array el -> List.exists expr_has_arrows el
  | Expr_pre e1 
  | Expr_when (e1, _, _) 
  | Expr_access (e1, _) 
  | Expr_power (e1, _) -> expr_has_arrows e1
  | Expr_ite (c, t, e) -> List.exists expr_has_arrows [c; t; e]
  | Expr_arrow (e1, e2) 
  | Expr_fby (e1, e2) -> true
  | Expr_merge (_, hl) -> List.exists (fun (_, h) -> expr_has_arrows h) hl
  | Expr_appl (i, e', i') -> expr_has_arrows e'

and eq_has_arrows eq =
  expr_has_arrows eq.eq_rhs
and aut_has_arrows aut = List.exists (fun h -> List.exists (fun stmt -> match stmt with Eq eq -> eq_has_arrows eq | Aut aut' -> aut_has_arrows aut') h.hand_stmts ) aut.aut_handlers 
and node_has_arrows node =
  let eqs, auts = get_node_eqs node in
  List.exists (fun eq -> eq_has_arrows eq) eqs || List.exists (fun aut -> aut_has_arrows aut) auts



let copy_var_decl vdecl =
  mkvar_decl vdecl.var_loc ~orig:vdecl.var_orig (vdecl.var_id, vdecl.var_dec_type, vdecl.var_dec_clock, vdecl.var_dec_const, vdecl.var_dec_value, vdecl.var_parent_nodeid)

let copy_const cdecl =
  { cdecl with const_type = Types.new_var () }

let copy_node nd =
  { nd with
    node_type     = Types.new_var ();
    node_clock    = Clocks.new_var true;
    node_inputs   = List.map copy_var_decl nd.node_inputs;
    node_outputs  = List.map copy_var_decl nd.node_outputs;
    node_locals   = List.map copy_var_decl nd.node_locals;
    node_gencalls = [];
    node_checks   = [];
    node_stateless = None;
  }

let copy_top top =
  match top.top_decl_desc with
  | Node nd -> { top with top_decl_desc = Node (copy_node nd)  }
  | Const c -> { top with top_decl_desc = Const (copy_const c) }
  | _       -> top

let copy_prog top_list =
  List.map copy_top top_list


let rec expr_contains_expr expr_tag expr  =
  let search = expr_contains_expr expr_tag in
  expr.expr_tag = expr_tag ||
      (
	match expr.expr_desc with
	| Expr_const _ -> false
	| Expr_array el -> List.exists search el
	| Expr_access (e1, _) 
	| Expr_power (e1, _) -> search e1
	| Expr_tuple el -> List.exists search el
	| Expr_ite (c, t, e) -> List.exists search [c;t;e]
	| Expr_arrow (e1, e2)
	| Expr_fby (e1, e2) -> List.exists search [e1; e2]
	| Expr_pre e' 
	| Expr_when (e', _, _) -> search e'
	| Expr_merge (_, hl) -> List.exists (fun (_, h) -> search h) hl
	| Expr_appl (_, e', None) -> search e' 
	| Expr_appl (_, e', Some e'') -> List.exists search [e'; e''] 
	| Expr_ident _ -> false
      )




(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
