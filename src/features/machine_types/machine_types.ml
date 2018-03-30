(* Extension du deal with machine types annotation

   In each node, node local annotations can specify the actual type of the
   implementation uintXX, intXX, floatXX ...

   The module provide utility functions to query the model:
   - get_var_machine_type varid nodeid returns the string denoting the actual type

   The actual type is used at different stages of the coompilation
   - early stage: limited typing, ie validity of operation are checked
      - a first version ensures that the actual type is a subtype of the declared/infered ones
        eg uint8 is a valid subtype of int
      - a future implementation could ensures that operations are valid
        - each standard or unspecified operation should be homogeneous : 
          op: real -> real -> real is valid for any same subtype t of real: op: t -> t -> t
        - specific nodes that explicitely defined subtypes could be used to perform casts
          eg. a int2uint8 (i: int) returns (j: int) with annotations specifying i as int and j as uint8
   - C backend: any print of a typed variable should rely on the actual machine type when provided
   - EMF backend: idem
   - Horn backend: an option could enforce the bounds provided by the machine
     type or implement the cycling behavior for integer subtypes 
   - Salsa plugin: the information should be propagated to the plugin. 
     One can also imagine that results of the analysis could specify or
     substitute a type by a subtype. Eg. the analysis detects that a float32 is enough for variable z and
     the annotation is added to the node.


A posisble behavior could be 
- an option to  ensure type checking
- dedicated  conversion functions that, in C, would generate cast or calls to simple identity functions (to be inlined)



TODO
EMF: rajouter les memoires dans les caracteristiques du node
     gerer les types plus finement:
     propager les types machines aux variables fraiches creees par la normalisation

*)
open Lustre_types

let is_active = false
  
let keyword = ["machine_types"]

module MT =
struct

  type int_typ =
    | Tint8_t
    | Tint16_t
    | Tint32_t
    | Tint64_t
    | Tuint8_t
    | Tuint16_t
    | Tuint32_t
    | Tuint64_t

  let pp_int fmt t =
    match t with
    | Tint8_t -> Format.fprintf fmt "int8"
    | Tint16_t -> Format.fprintf fmt "int16"
    | Tint32_t -> Format.fprintf fmt "int32"
    | Tint64_t -> Format.fprintf fmt "int64"
    | Tuint8_t -> Format.fprintf fmt "uint8"
    | Tuint16_t -> Format.fprintf fmt "uint16"
    | Tuint32_t -> Format.fprintf fmt "uint32"
    | Tuint64_t -> Format.fprintf fmt "uint64"

  let pp_c_int fmt t =
    match t with
    | Tint8_t -> Format.fprintf fmt "int8_t"
    | Tint16_t -> Format.fprintf fmt "int16_t"
    | Tint32_t -> Format.fprintf fmt "int32_t"
    | Tint64_t -> Format.fprintf fmt "int64_t"
    | Tuint8_t -> Format.fprintf fmt "uint8_t"
    | Tuint16_t -> Format.fprintf fmt "uint16_t"
    | Tuint32_t -> Format.fprintf fmt "uint32_t"
    | Tuint64_t -> Format.fprintf fmt "uint64_t"

  type t =
    | MTint of int_typ option
    | MTreal of string option
    | MTbool
    | MTstring

  open Format
  let pp fmt t =
    match t with
    | MTint None ->
       fprintf fmt "int"
    | MTint (Some s) ->
       fprintf fmt "%a" pp_int s
    | MTreal None ->
       fprintf fmt "real"
    | MTreal (Some s) ->
       fprintf fmt "%s" s
    | MTbool ->
       fprintf fmt "bool"
    | MTstring ->
       fprintf fmt "string"

  let pp_c fmt t =
    match t with
    | MTint (Some s) ->
       fprintf fmt "%a" pp_c_int s
    | MTreal (Some s) ->
       fprintf fmt "%s" s
    | MTint None 
    | MTreal None 
    | MTbool 
    | MTstring -> assert false
	 
	 
  let is_scalar_type t =
    match t with
    | MTbool 
    | MTint _
    | MTreal _ -> true
    | _ -> false


  let is_numeric_type t =
    match t with
    | MTint _ 
    | MTreal _ -> true
    | _ -> false

  let is_int_type t = match t with MTint _ -> true | _ -> false
  let is_real_type t = match t with MTreal _ -> true | _ -> false
  let is_bool_type t = t = MTbool
    
  let is_dimension_type t =
    match t with
    | MTint _
    | MTbool -> true
 | _ -> false

  let type_int_builder = MTint None
  let type_real_builder = MTreal None
  let type_bool_builder = MTbool
  let type_string_builder = MTstring

  let unify _ _ = ()
  let is_unifiable b1 b2 =
    match b1, b2 with
    | MTint _ , MTint _
    | MTreal _, MTreal _
    | MTstring, MTstring
    | MTbool, MTbool -> true
    | _ -> false

  let is_exportable b =
    match b with
    | MTstring
    | MTbool
    | MTreal None
    | MTint None -> false
    | _ -> true
end

module MTypes = Types.Make (MT)

let type_uint8 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tuint8_t)))
let type_uint16 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tuint16_t)))
let type_uint32 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tuint32_t)))
let type_uint64 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tuint64_t)))
let type_int8 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tint8_t)))
let type_int16 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tint16_t)))
let type_int32 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tint32_t)))
let type_int64 = MTypes.new_ty (MTypes.Tbasic (MT.MTint (Some MT.Tint64_t)))

  
module ConvTypes =
    struct
      type type_expr = MTypes.type_expr

      let map_type_basic f_basic  =
	let rec map_type_basic e =
	  { MTypes.tid = e.Types.tid;
	    MTypes.tdesc = map_type_basic_desc (Types.type_desc e)
	  }
	and map_type_basic_desc td =
	  let mape = map_type_basic in
	  match td with
	  | Types.Tbasic b -> f_basic b
	  | Types.Tconst c -> MTypes.Tconst c
	  | Types.Tenum e -> MTypes.Tenum e
	  | Types.Tvar -> MTypes.Tvar
	  | Types.Tunivar -> MTypes.Tunivar
	     
	  | Types.Tclock te -> MTypes.Tclock (mape te)
	  | Types.Tarrow (te1, te2) -> MTypes.Tarrow (mape te1, mape te2)
	  | Types.Ttuple tel -> MTypes.Ttuple (List.map mape tel)
	  | Types.Tstruct id_te_l -> MTypes.Tstruct (List.map (fun (id, te) -> id, mape te) id_te_l) 
	  | Types.Tarray (de, te) -> MTypes.Tarray (de, mape te)
	  | Types.Tstatic (de, te) -> MTypes.Tstatic (de, mape te)
	  | Types.Tlink te -> MTypes.Tlink (mape te)
	in
	map_type_basic
	  
      let import main_typ =
	let import_basic b =
	  if Types.BasicT.is_int_type b then MTypes.type_int else
	    if Types.BasicT.is_real_type b then MTypes.type_real else
	      if Types.BasicT.is_bool_type b then MTypes.type_bool else
		(Format.eprintf "importing %a with issues!@.@?" Types.print_ty main_typ; assert false)
	in
	map_type_basic import_basic main_typ

	  
      let map_mtype_basic f_basic  =
	let rec map_mtype_basic e =
	  { Types.tid = e.MTypes.tid;
	    Types.tdesc = map_mtype_basic_desc (MTypes.type_desc e)
	  }
	and map_mtype_basic_desc td =
	  let mape = map_mtype_basic in
	  match td with
	  | MTypes.Tbasic b ->
	     (* Format.eprintf "supposely basic mtype: %a@." MTypes.BasicT.pp b; *)
	    f_basic b 
	  | MTypes.Tconst c -> Types.Tconst c
	  | MTypes.Tenum e -> Types.Tenum e
	  | MTypes.Tvar -> Types.Tvar
	  | MTypes.Tunivar -> Types.Tunivar
	     
	  | MTypes.Tclock te -> Types.Tclock (mape te)
	  | MTypes.Tarrow (te1, te2) -> Types.Tarrow (mape te1, mape te2)
	  | MTypes.Ttuple tel -> Types.Ttuple (List.map mape tel)
	  | MTypes.Tstruct id_te_l -> Types.Tstruct (List.map (fun (id, te) -> id, mape te) id_te_l) 
	  | MTypes.Tarray (de, te) -> Types.Tarray (de, mape te)
	  | MTypes.Tstatic (de, te) -> Types.Tstatic (de, mape te)
	  | MTypes.Tlink te -> Types.Tlink (mape te)
	in
	map_mtype_basic
	  
      let export machine_type =
	let export_basic b =
	if MTypes.BasicT.is_int_type b then Types.type_int else
	if MTypes.BasicT.is_real_type b then Types.type_real else
	if MTypes.BasicT.is_bool_type b then Types.type_bool else
          (
	    Format.eprintf "unhandled basic mtype is %a. Issues while dealing with basic type %a@.@?" MTypes.print_ty machine_type MTypes.BasicT.pp b;
	    assert false
	  )
	in
	map_mtype_basic export_basic machine_type

    end

module Typing = Typing.Make (MTypes) (ConvTypes)
				  
(* Associate to each (node_id, var_id) its machine type *)
let machine_type_table : (var_decl, MTypes.type_expr) Hashtbl.t = Hashtbl.create 13

(* Store the node signatures, with machine types when available *)
let typing_env = ref Env.initial
    
let is_specified v =
  (* Format.eprintf "looking for var %a@." Printers.pp_var v; *)
  Hashtbl.mem machine_type_table v

let pp_table fmt =
  Format.fprintf fmt "@[<v 0>[";
  Hashtbl.iter
    (fun v typ -> Format.fprintf fmt "%a -> %a,@ " Printers.pp_var v MTypes.print_ty typ )
    machine_type_table;
  Format.fprintf fmt "@]"

    
let get_specified_type v =
  (* Format.eprintf "Looking for variable %a in table [%t]@.@?" *)
  (*   Printers.pp_var v *)
  (*   pp_table; *)
    Hashtbl.find machine_type_table v

let is_exportable v =
  is_specified v && (
    let typ = get_specified_type v in
    match (MTypes.dynamic_type typ).MTypes.tdesc with
    | MTypes.Tbasic b -> MT.is_exportable b
    | MTypes.Tconst _ -> false (* Enumerated types are not "machine type" customizeable *)
    | _ -> assert false  (* TODO deal with other constructs *)
  )      
(* could depend on the actual computed type *)

let type_name typ = 
  MTypes.print_ty Format.str_formatter typ;
  Format.flush_str_formatter () 

let pp_var_type fmt v =
  let typ = get_specified_type v in
  MTypes.print_ty fmt typ

let pp_c_var_type fmt v =
  let typ = get_specified_type v in
  MTypes.print_ty_param MT.pp_c fmt typ
    
(************** Checking types ******************)
  
let erroneous_annotation loc =
  Format.eprintf "Invalid annotation for machine_type at loc %a@."
    Location.pp_loc loc;
  assert false


let valid_subtype subtype typ =
  let mismatch subtyp typ =
    Format.eprintf "Subtype mismatch %a vs %a@." MTypes.print_ty subtyp Types.print_ty typ; false
  in
  match (MTypes.dynamic_type subtype).MTypes.tdesc with
  | MTypes.Tconst c -> Types.is_const_type typ c
  | MTypes.Tbasic MT.MTint _ -> Types.is_int_type typ
  | MTypes.Tbasic MT.MTreal _ -> Types.is_real_type typ
  | MTypes.Tbasic MT.MTbool -> Types.is_bool_type typ
  | _ -> mismatch subtype typ
     
let type_of_name name =
  match name with
  | "uint8" -> type_uint8
  | "uint16" -> type_uint16
  | "uint32" -> type_uint32
  | "uint64" -> type_uint64
  | "int8" -> type_int8
  | "int16" -> type_int16
  | "int32" -> type_int32
  | "int64" -> type_int64
  | _ -> assert false (* unknown custom machine type *)
     
let register_var var typ =
  (* let typ = type_of_name type_name in *)
  if valid_subtype typ var.var_type then (
    Hashtbl.add machine_type_table var typ
  )
  else
    erroneous_annotation var.var_loc
    
(* let register_var_opt var type_name_opt = *)
(*   match type_name_opt with *)
(*   | None -> () *)
(*   | Some type_name -> register_var var type_name *)
     
(************** Registering annotations ******************)

    
let register_node node_id vars annots =
  List.fold_left (fun accu annot ->
    let annl = annot.annots in
    List.fold_left (fun accu (kwd, value) ->
      if kwd = keyword then
	let expr = value.eexpr_qfexpr in
	match Corelang.expr_list_of_expr expr with
	| [var_id; type_name] -> (
	  match var_id.expr_desc, type_name.expr_desc with
	  | Expr_ident var_id, Expr_const (Const_string type_name) ->
	     let var = List.find (fun v -> v.var_id = var_id) vars in
	     Log.report ~level:2 (fun fmt ->
	       Format.fprintf fmt "Recorded type %s for variable %a (parent node is %s)@ "
		 type_name
		 Printers.pp_var var
		 (match var.var_parent_nodeid with Some id -> id | None -> "unknown")
	     );
	     let typ = type_of_name type_name in
	     register_var var typ;
	     var::accu
	  | _ -> erroneous_annotation expr.expr_loc
	)
	| _ -> erroneous_annotation expr.expr_loc
      else
	accu
    ) accu annl
  ) [] annots


let check_node nd vars =
(* TODO check that all access to vars are valid *)
  ()
  
let type_of_vlist vars =
  let tyl = List.map (fun v -> if is_specified v then get_specified_type v else
      ConvTypes.import v.var_type
  ) vars in
  MTypes.type_of_type_list tyl

  
let load prog =
  let init_env =
    Env.fold (fun id typ env ->
      Env.add_value env id (ConvTypes.import typ)
    ) 
    Basic_library.type_env Env.initial in
  let env =
    List.fold_left (fun type_env top ->
      match top.top_decl_desc with
      | Node nd ->
	 (* Format.eprintf "Registeing node %s@." nd.node_id; *)
	 let vars = nd.node_inputs @ nd.node_outputs @ nd.node_locals in
	 let constrained_vars = register_node nd.node_id vars nd.node_annot in
	 check_node nd constrained_vars;

	 (* Computing the node type *)
	 let ty_ins = type_of_vlist nd.node_inputs in
	 let ty_outs = type_of_vlist nd.node_outputs in
	 let ty_node = MTypes.new_ty (MTypes.Tarrow (ty_ins,ty_outs)) in
	 Typing.generalize ty_node;
	 let env = Env.add_value type_env nd.node_id ty_node in
	 (* Format.eprintf "Env: %a" (Env.pp_env MTypes.print_ty) env; *)
	 env

      | _ -> type_env 
    (* | ImportedNode ind -> *)
    (*    let vars = ind.nodei_inputs @ ind.nodei_outputs in *)
    (*    register_node ind.nodei_id vars ind.nodei_annot *)
(*      | _ -> () TODO: shall we load something for Open statements? *)
    ) init_env prog
  in
  typing_env := env

let type_expr nd expr =
  let init_env = !typing_env in
  (* Format.eprintf "Init env: %a@." (Env.pp_env MTypes.print_ty) init_env; *)
  let init_vars = nd.node_inputs @ nd.node_outputs @ nd.node_locals in
  (* Rebuilding the variables environment from accumulated knowledge *)
  let env,vars = (* First, we add non specified variables *)
    List.fold_left (fun (env, vars) v ->
      if not (is_specified v) then
  	let env = Env.add_value env v.var_id (ConvTypes.import v.var_type) in
  	env, v::vars
      else
	env, vars
    ) (init_env, []) init_vars 
  in
  
  (* Then declared ones *)
  let env, vars =
    Hashtbl.fold (fun vdecl machine_type (env, vds) ->
      if vdecl.var_parent_nodeid = Some nd.node_id then (
	 (* Format.eprintf "Adding variable %a to the environement@.@?" Printers.pp_var vdecl;  *)
	let env = Env.add_value env vdecl.var_id machine_type in
	env, vdecl::vds
      )
      else
	env, vds
    ) machine_type_table (env, vars)
  in

  
  (* Format.eprintf "env with local vars: %a@." (Env.pp_env MTypes.print_ty) env; *)
  (* Format.eprintf "expr = %a@." Printers.pp_expr expr; *)
  (* let res = *)
    Typing.type_expr
      (env,vars)
      false (* not in main node *)
      false (* no a constant *)
      expr
  (* in *)
  (* Format.eprintf "typing ok = %a@." MTypes.print_ty res; *)
  (* res *)
  
(* Typing the expression (vars = expr) in node 
   
*)    
let type_def node vars expr =
  (* Format.eprintf "Typing def %a = %a@.@." *)
  (*   (Utils.fprintf_list ~sep:", " Printers.pp_var) vars *)
  (*   Printers.pp_expr expr *)
  (* ; *)
    let typ = type_expr node expr in
    (* Format.eprintf "Type is %a. Saving stuff@.@." MTypes.print_ty typ; *)
    let typ = MTypes.type_list_of_type typ  in
    List.iter2 register_var vars typ 

let has_machine_type () =
  let annl = Annotations.get_expr_annotations keyword in
  (* Format.eprintf "has _mchine _type annotations: %i@." (List.length annl); *)
  List.length annl > 0
      
(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)

