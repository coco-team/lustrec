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
open Corelang
open Machine_code_common
open C_backend_common

module type MODIFIERS_SRC =
sig
end

module EmptyMod =
struct
end

module Main = functor (Mod: MODIFIERS_SRC) -> 
struct

(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)


(* Computes the depth to which multi-dimension array assignments should be expanded.
   It equals the maximum number of nested static array constructions accessible from root [v].
*)
  let rec expansion_depth v =
    match v.value_desc with
    | Cst cst -> expansion_depth_cst cst
    | LocalVar _
    | StateVar _  -> 0
    | Fun (_, vl) -> List.fold_right (fun v -> max (expansion_depth v)) vl 0
    | Array vl    -> 1 + List.fold_right (fun v -> max (expansion_depth v)) vl 0
    | Access (v, i) -> max 0 (expansion_depth v - 1)
    | Power (v, n)  -> 0 (*1 + expansion_depth v*)
  and expansion_depth_cst c = 
    match c with
      Const_array cl -> 1 + List.fold_right (fun c -> max (expansion_depth_cst c)) cl 0
    | _ -> 0
  
  let rec merge_static_loop_profiles lp1 lp2 =
    match lp1, lp2 with
    | []      , _        -> lp2
    | _       , []       -> lp1
    | p1 :: q1, p2 :: q2 -> (p1 || p2) :: merge_static_loop_profiles q1 q2

(* Returns a list of bool values, indicating whether the indices must be static or not *)
  let rec static_loop_profile v =
    match v.value_desc with
    | Cst cst  -> static_loop_profile_cst cst
    | LocalVar _
    | StateVar _  -> []
    | Fun (_, vl) -> List.fold_right (fun v lp -> merge_static_loop_profiles lp (static_loop_profile v)) vl []
    | Array vl    -> true :: List.fold_right (fun v lp -> merge_static_loop_profiles lp (static_loop_profile v)) vl []
    | Access (v, i) -> (match (static_loop_profile v) with [] -> [] | _ :: q -> q)
    | Power (v, n)  -> false :: static_loop_profile v
  and static_loop_profile_cst cst =
    match cst with
      Const_array cl -> List.fold_right 
	(fun c lp -> merge_static_loop_profiles lp (static_loop_profile_cst c))
	cl 
	[]
    | _ -> [] 
  
  
let rec is_const_index v =
  match v.value_desc with
  | Cst (Const_int _) -> true
  | Fun (_, vl)       -> List.for_all is_const_index vl
  | _                 -> false

type loop_index = LVar of ident | LInt of int ref | LAcc of value_t
(*
let rec value_offsets v offsets =
 match v, offsets with
 | _                        , []          -> v
 | Power (v, n)             , _ :: q      -> value_offsets v q
 | Array vl                 , LInt r :: q -> value_offsets (List.nth vl !r) q
 | Cst (Const_array cl)     , LInt r :: q -> value_offsets (Cst (List.nth cl !r)) q
 | Fun (f, vl)              , _           -> Fun (f, List.map (fun v -> value_offsets v offsets) vl)
 | _                        , LInt r :: q -> value_offsets (Access (v, Cst (Const_int !r))) q
 | _                        , LVar i :: q -> value_offsets (Access (v, LocalVar i)) q
*)
(* Computes the list of nested loop variables together with their dimension bounds.
   - LInt r stands for loop expansion (no loop variable, but int loop index)
   - LVar v stands for loop variable v
*)
let rec mk_loop_variables m ty depth =
 match (Types.repr ty).Types.tdesc, depth with
 | Types.Tarray (d, ty'), 0       ->
   let v = mk_loop_var m () in
   (d, LVar v) :: mk_loop_variables m ty' 0
 | Types.Tarray (d, ty'), _       ->
   let r = ref (-1) in
   (d, LInt r) :: mk_loop_variables m ty' (depth - 1)
 | _                    , 0       -> []
 | _                              -> assert false

let reorder_loop_variables loop_vars =
  let (int_loops, var_loops) = 
    List.partition (function (d, LInt _) -> true | _ -> false) loop_vars 
  in
  var_loops @ int_loops

(* Prints a one loop variable suffix for arrays *)
let pp_loop_var fmt lv =
 match snd lv with
 | LVar v -> fprintf fmt "[%s]" v
 | LInt r -> fprintf fmt "[%d]" !r
 | LAcc i -> fprintf fmt "[%a]" pp_val i

(* Prints a suffix of loop variables for arrays *)
let pp_suffix fmt loop_vars =
 Utils.fprintf_list ~sep:"" pp_loop_var fmt loop_vars

(* Prints a value expression [v], with internal function calls only.
   [pp_var] is a printer for variables (typically [pp_c_var_read]),
   but an offset suffix may be added for array variables
*)
(* Prints a constant value before a suffix (needs casting) *)
let rec pp_c_const_suffix var_type fmt c =
  match c with
    | Const_int i          -> pp_print_int fmt i
    | Const_real (_, _, s) -> pp_print_string fmt s
    | Const_tag t          -> pp_c_tag fmt t
    | Const_array ca       -> let var_type = Types.array_element_type var_type in
                              fprintf fmt "(%a[]){%a }" (pp_c_type "") var_type (Utils.fprintf_list ~sep:", " (pp_c_const_suffix var_type)) ca
    | Const_struct fl       -> fprintf fmt "{%a }" (Utils.fprintf_list ~sep:", " (fun fmt (f, c) -> (pp_c_const_suffix (Types.struct_field_type var_type f)) fmt c)) fl
    | Const_string _        -> assert false (* string occurs in annotations not in C *)


(* Prints a [value] of type [var_type] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self var_type loop_vars pp_value fmt value =
  (*Format.eprintf "pp_value_suffix: %a %a %a@." Types.print_ty var_type Machine_code.pp_val value pp_suffix loop_vars;*)
  (
    match loop_vars, value.value_desc with
    | (x, LAcc i) :: q, _ when is_const_index i ->
       let r = ref (Dimension.size_const_dimension (dimension_of_value i)) in
       pp_value_suffix self var_type ((x, LInt r)::q) pp_value fmt value
    | (_, LInt r) :: q, Cst (Const_array cl) ->
       let var_type = Types.array_element_type var_type in
       pp_value_suffix self var_type q pp_value fmt (mk_val (Cst (List.nth cl !r)) Type_predef.type_int)
    | (_, LInt r) :: q, Array vl      ->
       let var_type = Types.array_element_type var_type in
       pp_value_suffix self var_type q pp_value fmt (List.nth vl !r)
    | loop_var    :: q, Array vl      ->
       let var_type = Types.array_element_type var_type in
       Format.fprintf fmt "(%a[]){%a }%a" (pp_c_type "") var_type (Utils.fprintf_list ~sep:", " (pp_value_suffix self var_type q pp_value)) vl pp_suffix [loop_var]
    | []              , Array vl      ->
       let var_type = Types.array_element_type var_type in
       Format.fprintf fmt "(%a[]){%a }" (pp_c_type "") var_type (Utils.fprintf_list ~sep:", " (pp_value_suffix self var_type [] pp_value)) vl
    | _           :: q, Power (v, n)  ->
       pp_value_suffix self var_type q pp_value fmt v
    | _               , Fun (n, vl)   ->
       pp_basic_lib_fun n (pp_value_suffix self var_type loop_vars pp_value) fmt vl
    | _               , Access (v, i) ->
       let var_type = Type_predef.type_array (Dimension.mkdim_var ()) var_type in
       pp_value_suffix self var_type ((Dimension.mkdim_var (), LAcc i) :: loop_vars) pp_value fmt v
    | _               , LocalVar v    -> Format.fprintf fmt "%a%a" pp_value v pp_suffix loop_vars
    | _               , StateVar v    ->
       (* array memory vars are represented by an indirection to a local var with the right type,
	  in order to avoid casting everywhere. *)
       if Types.is_array_type v.var_type
       then Format.fprintf fmt "%a%a" pp_value v pp_suffix loop_vars
       else Format.fprintf fmt "%s->_reg.%a%a" self pp_value v pp_suffix loop_vars
    | _               , Cst cst       -> pp_c_const_suffix var_type fmt cst
    | _               , _             -> (Format.eprintf "internal error: C_backend_src.pp_value_suffix %a %a %a@." Types.print_ty var_type pp_val value pp_suffix loop_vars; assert false)
  )
   
(* Subsumes C_backend_common.pp_c_val to cope with aggressive substitution
   which may yield constant arrays in expressions.
   Type is needed to correctly print constant arrays.
 *)
let pp_c_val self pp_var fmt v =
  pp_value_suffix self v.value_type [] pp_var fmt v

let pp_basic_assign pp_var fmt typ var_name value =
  if Types.is_real_type typ && !Options.mpfr
  then
    Mpfr.pp_inject_assign pp_var fmt var_name value
  else
    fprintf fmt "%a = %a;" 
      pp_var var_name
      pp_var value

(* type_directed assignment: array vs. statically sized type
   - [var_type]: type of variable to be assigned
   - [var_name]: name of variable to be assigned
   - [value]: assigned value
   - [pp_var]: printer for variables
*)
let pp_assign m self pp_var fmt var_type var_name value =
  let depth = expansion_depth value in
  (*Format.eprintf "pp_assign %a %a %a %d@." Types.print_ty var_type pp_val var_name pp_val value depth;*)
  let loop_vars = mk_loop_variables m var_type depth in
  let reordered_loop_vars = reorder_loop_variables loop_vars in
  let rec aux typ fmt vars =
    match vars with
    | [] ->
       pp_basic_assign (pp_value_suffix self var_type loop_vars pp_var) fmt typ var_name value
    | (d, LVar i) :: q ->
       let typ' = Types.array_element_type typ in
      (*eprintf "pp_aux %a %s@." Dimension.pp_dimension d i;*)
      fprintf fmt "@[<v 2>{@,int %s;@,for(%s=0;%s<%a;%s++)@,%a @]@,}"
	i i i pp_c_dimension d i
	(aux typ') q
    | (d, LInt r) :: q ->
       (*eprintf "pp_aux %a %d@." Dimension.pp_dimension d (!r);*)
       let typ' = Types.array_element_type typ in
       let szl = Utils.enumerate (Dimension.size_const_dimension d) in
       fprintf fmt "@[<v 2>{@,%a@]@,}"
	       (Utils.fprintf_list ~sep:"@," (fun fmt i -> r := i; aux typ' fmt q)) szl
    | _ -> assert false
  in
  begin
    reset_loop_counter ();
    (*reset_addr_counter ();*)
    aux var_type fmt reordered_loop_vars;
    (*Format.eprintf "end pp_assign@.";*)
  end

let pp_machine_reset (m: machine_t) self fmt inst =
  let (node, static) =
    try
      List.assoc inst m.minstances
    with Not_found -> (Format.eprintf "internal error: pp_machine_reset %s %s %s:@." m.mname.node_id self inst; raise Not_found) in
  fprintf fmt "%a(%a%t%s->%s);"
    pp_machine_reset_name (node_name node)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty ", " static)
    self inst

let pp_machine_init (m: machine_t) self fmt inst =
  let (node, static) =
    try
      List.assoc inst m.minstances
    with Not_found -> (Format.eprintf "internal error: pp_machine_init %s %s %s@." m.mname.node_id self inst; raise Not_found) in
  fprintf fmt "%a(%a%t%s->%s);"
    pp_machine_init_name (node_name node)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty ", " static)
    self inst

let pp_machine_clear (m: machine_t) self fmt inst =
  let (node, static) =
    try
      List.assoc inst m.minstances
    with Not_found -> (Format.eprintf "internal error: pp_machine_clear %s %s %s@." m.mname.node_id self inst; raise Not_found) in
  fprintf fmt "%a(%a%t%s->%s);"
    pp_machine_clear_name (node_name node)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty ", " static)
    self inst

let has_c_prototype funname dependencies =
  let imported_node_opt = (* We select the last imported node with the name funname.
			       The order of evaluation of dependencies should be
			       compatible with overloading. (Not checked yet) *) 
      List.fold_left
	(fun res (Dep (_, _, decls, _)) -> 
	  match res with
	  | Some _ -> res
	  | None -> 
	    let matched = fun t -> match t.top_decl_desc with 
	      | ImportedNode nd -> nd.nodei_id = funname 
	      | _ -> false
	    in
	    if List.exists matched decls then (
	      match (List.find matched decls).top_decl_desc with
	      | ImportedNode nd -> Some nd
	      | _ -> assert false
	    )
	    else
	      None
	) None dependencies in
    match imported_node_opt with
    | None -> false
    | Some nd -> (match nd.nodei_prototype with Some "C" -> true | _ -> false)
(*
let pp_instance_call dependencies m self fmt i (inputs: value_t list) (outputs: var_decl list) =
  try (* stateful node instance *)
    let (n,_) = List.assoc i m.minstances in
    let (input_types, _) = Typing.get_type_of_call n in
    let inputs = List.combine input_types inputs in
    fprintf fmt "%a (%a%t%a%t%s->%s);"
      pp_machine_step_name (node_name n)
      (Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) inputs
      (Utils.pp_final_char_if_non_empty ", " inputs) 
      (Utils.fprintf_list ~sep:", " (pp_c_var_write m)) outputs
      (Utils.pp_final_char_if_non_empty ", " outputs)
      self
      i
  with Not_found -> (* stateless node instance *)
    let (n,_) = List.assoc i m.mcalls in
    let (input_types, output_types) = Typing.get_type_of_call n in
    let inputs = List.combine input_types inputs in
    if has_c_prototype i dependencies
    then (* external C function *)
      let outputs = List.map2 (fun t v -> t, LocalVar v) output_types outputs in
      fprintf fmt "%a = %s(%a);"
	(Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) outputs
	i
	(Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) inputs
    else
      fprintf fmt "%a (%a%t%a);"
	pp_machine_step_name (node_name n)
	(Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) inputs
	(Utils.pp_final_char_if_non_empty ", " inputs) 
	(Utils.fprintf_list ~sep:", " (pp_c_var_write m)) outputs 
*)
let rec pp_conditional dependencies (m: machine_t) self fmt c tl el =
  fprintf fmt "@[<v 2>if (%a) {%t%a@]@,@[<v 2>} else {%t%a@]@,}"
    (pp_c_val self (pp_c_var_read m)) c
    (Utils.pp_newline_if_non_empty tl)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) tl
    (Utils.pp_newline_if_non_empty el)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) el

and pp_machine_instr dependencies (m: machine_t) self fmt instr =
  match get_instr_desc instr with 
  | MNoReset _ -> ()
  | MReset i ->
    pp_machine_reset m self fmt i
  | MLocalAssign (i,v) ->
    pp_assign
      m self (pp_c_var_read m) fmt
      i.var_type (mk_val (LocalVar i) i.var_type) v
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_c_var_read m) fmt
      i.var_type (mk_val (StateVar i) i.var_type) v
  | MStep ([i0], i, vl) when Basic_library.is_value_internal_fun (mk_val (Fun (i, vl)) i0.var_type)  ->
    pp_machine_instr dependencies m self fmt 
      (update_instr_desc instr (MLocalAssign (i0, mk_val (Fun (i, vl)) i0.var_type)))
  | MStep ([i0], i, vl) when has_c_prototype i dependencies -> 
    fprintf fmt "%a = %s(%a);" 
      (pp_c_val self (pp_c_var_read m)) (mk_val (LocalVar i0) i0.var_type)
      i
      (Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) vl
  | MStep (il, i, vl) when Mpfr.is_homomorphic_fun i ->
    pp_instance_call m self fmt i vl il
  | MStep (il, i, vl) ->
    pp_basic_instance_call m self fmt i vl il
  | MBranch (_, []) -> (Format.eprintf "internal error: C_backend_src.pp_machine_instr %a@." pp_instr instr; assert false)
  | MBranch (g, hl) ->
    if let t = fst (List.hd hl) in t = tag_true || t = tag_false
    then (* boolean case, needs special treatment in C because truth value is not unique *)
	 (* may disappear if we optimize code by replacing last branch test with default *)
      let tl = try List.assoc tag_true  hl with Not_found -> [] in
      let el = try List.assoc tag_false hl with Not_found -> [] in
      pp_conditional dependencies m self fmt g tl el
    else (* enum type case *)
      (*let g_typ = Typing.type_const Location.dummy_loc (Const_tag (fst (List.hd hl))) in*)
      fprintf fmt "@[<v 2>switch(%a) {@,%a@,}@]"
	(pp_c_val self (pp_c_var_read m)) g
	(Utils.fprintf_list ~sep:"@," (pp_machine_branch dependencies m self)) hl
  | MComment s  -> 
      fprintf fmt "/*%s*/@ " s


and pp_machine_branch dependencies m self fmt (t, h) =
  fprintf fmt "@[<v 2>case %a:@,%a@,break;@]"
    pp_c_tag t
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) h


(********************************************************************************************)
(*                         C file Printing functions                                        *)
(********************************************************************************************)

let print_const_def fmt cdecl =
  if !Options.mpfr && Types.is_real_type (Types.array_base_type cdecl.const_type)
  then
    fprintf fmt "%a;@." 
      (pp_c_type cdecl.const_id) (Types.dynamic_type cdecl.const_type) 
  else
    fprintf fmt "%a = %a;@." 
      (pp_c_type cdecl.const_id) cdecl.const_type
      pp_c_const cdecl.const_value 


let print_alloc_instance fmt (i, (m, static)) =
  fprintf fmt "_alloc->%s = %a (%a);@,"
    i
    pp_machine_alloc_name (node_name m)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static

let print_dealloc_instance fmt (i, (m, _)) =
  fprintf fmt "%a (_alloc->%s);@,"
    pp_machine_dealloc_name (node_name m)
    i

let print_alloc_const fmt m =
  let const_locals = List.filter (fun vdecl -> vdecl.var_dec_const) m.mstep.step_locals in
  fprintf fmt "%a%t"
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) const_locals
    (Utils.pp_final_char_if_non_empty ";@," const_locals)

let print_alloc_array fmt vdecl =
  let base_type = Types.array_base_type vdecl.var_type in
  let size_types = Types.array_type_multi_dimension vdecl.var_type in
  let size_type = Dimension.multi_dimension_product vdecl.var_loc size_types in
  fprintf fmt "_alloc->_reg.%s = (%a*) malloc((%a)*sizeof(%a));@,assert(_alloc->%s);@,"
    vdecl.var_id
    (pp_c_type "") base_type
    Dimension.pp_dimension size_type
    (pp_c_type "") base_type
    vdecl.var_id

let print_dealloc_array fmt vdecl =
  fprintf fmt "free (_alloc->_reg.%s);@,"
    vdecl.var_id

let print_alloc_code fmt m =
  let array_mem = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "%a *_alloc;@,_alloc = (%a *) malloc(sizeof(%a));@,assert(_alloc);@,%a%areturn _alloc;"
    pp_machine_memtype_name m.mname.node_id
    pp_machine_memtype_name m.mname.node_id
    pp_machine_memtype_name m.mname.node_id
    (Utils.fprintf_list ~sep:"" print_alloc_array) array_mem
    (Utils.fprintf_list ~sep:"" print_alloc_instance) m.minstances

let print_dealloc_code fmt m =
  let array_mem = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "%a%afree (_alloc);@,return;"
    (Utils.fprintf_list ~sep:"" print_dealloc_array) array_mem
    (Utils.fprintf_list ~sep:"" print_dealloc_instance) m.minstances

let print_stateless_init_code dependencies fmt m self =
  let minit = List.map (fun i -> match get_instr_desc i with MReset i -> i | _ -> assert false) m.minit in
  let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "@[<v 2>%a {@,%a%t%a%t%a%treturn;@]@,}@.@."
    (print_init_prototype self) (m.mname.node_id, m.mstatic)
    (* array mems *) 
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
    (Utils.pp_final_char_if_non_empty ";@," array_mems)
    (* memory initialization *)
    (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mmemory
    (Utils.pp_newline_if_non_empty m.mmemory)
    (* sub-machines initialization *)
    (Utils.fprintf_list ~sep:"@," (pp_machine_init m self)) minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_stateless_clear_code dependencies fmt m self =
  let minit = List.map (fun i -> match get_instr_desc i with MReset i -> i | _ -> assert false) m.minit in
  let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "@[<v 2>%a {@,%a%t%a%t%a%treturn;@]@,}@.@."
    (print_clear_prototype self) (m.mname.node_id, m.mstatic)
    (* array mems *)
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
    (Utils.pp_final_char_if_non_empty ";@," array_mems)
    (* memory clear *)
    (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mmemory
    (Utils.pp_newline_if_non_empty m.mmemory)
    (* sub-machines clear*)
    (Utils.fprintf_list ~sep:"@," (pp_machine_clear m self)) minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_stateless_code dependencies fmt m =
  let self = "__ERROR__" in
  if not (!Options.ansi && is_generic_node { top_decl_desc = Node m.mname; top_decl_loc = Location.dummy_loc; top_decl_owner = ""; top_decl_itf = false })
  then
    (* C99 code *)
    fprintf fmt "@[<v 2>%a {@,%a%t%a%t@,%a%a%t%a%t%t@]@,}@.@."
      print_stateless_prototype (m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) m.mstep.step_locals
      (Utils.pp_final_char_if_non_empty ";@," m.mstep.step_locals)
      (* locals initialization *)
      (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (* locals clear *)
      (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (fun fmt -> fprintf fmt "return;")
  else
    (* C90 code *)
    let (gen_locals, base_locals) = List.partition (fun v -> Types.is_generic_type v.var_type) m.mstep.step_locals in
    let gen_calls = List.map (fun e -> let (id, _, _) = call_of_expr e in mk_call_var_decl e.expr_loc id) m.mname.node_gencalls in
    fprintf fmt "@[<v 2>%a {@,%a%t%a%t@,%a%a%t%a%t%t@]@,}@.@."
      print_stateless_prototype (m.mname.node_id, (m.mstep.step_inputs@gen_locals@gen_calls), m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) base_locals
      (Utils.pp_final_char_if_non_empty ";" base_locals)
      (* locals initialization *)
      (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (* locals clear *)
      (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (fun fmt -> fprintf fmt "return;")

let print_reset_code dependencies fmt m self =
  let const_locals = List.filter (fun vdecl -> vdecl.var_dec_const) m.mstep.step_locals in
  fprintf fmt "@[<v 2>%a {@,%a%t@,%a%treturn;@]@,}@.@."
    (print_reset_prototype self) (m.mname.node_id, m.mstatic)
    (* constant locals decl *)
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) const_locals
    (Utils.pp_final_char_if_non_empty ";" const_locals)
    (* instrs *)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_init_code dependencies fmt m self =
  let minit = List.map (fun i -> match get_instr_desc i with MReset i -> i | _ -> assert false) m.minit in
  let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "@[<v 2>%a {@,%a%t%a%t%a%treturn;@]@,}@.@."
    (print_init_prototype self) (m.mname.node_id, m.mstatic)
    (* array mems *) 
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
    (Utils.pp_final_char_if_non_empty ";@," array_mems)
    (* memory initialization *)
    (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mmemory
    (Utils.pp_newline_if_non_empty m.mmemory)
    (* sub-machines initialization *)
    (Utils.fprintf_list ~sep:"@," (pp_machine_init m self)) minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_clear_code dependencies fmt m self =
  let minit = List.map (fun i -> match get_instr_desc i with MReset i -> i | _ -> assert false) m.minit in
  let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "@[<v 2>%a {@,%a%t%a%t%a%treturn;@]@,}@.@."
    (print_clear_prototype self) (m.mname.node_id, m.mstatic)
    (* array mems *)
    (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
    (Utils.pp_final_char_if_non_empty ";@," array_mems)
    (* memory clear *)
    (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mmemory
    (Utils.pp_newline_if_non_empty m.mmemory)
    (* sub-machines clear*)
    (Utils.fprintf_list ~sep:"@," (pp_machine_clear m self)) minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_step_code dependencies fmt m self =
  if not (!Options.ansi && is_generic_node { top_decl_desc = Node m.mname; top_decl_loc = Location.dummy_loc; top_decl_owner = ""; top_decl_itf = false })
  then
    (* C99 code *)
    let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
    fprintf fmt "@[<v 2>%a {@,%a%t%a%t%a%t@,%a%a%t%a%t%t@]@,}@.@."
      (print_step_prototype self) (m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) m.mstep.step_locals
      (Utils.pp_final_char_if_non_empty ";@," m.mstep.step_locals)
      (* array mems *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
      (Utils.pp_final_char_if_non_empty ";@," array_mems)
      (* locals initialization *)
      (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (* locals clear *)
      (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (fun fmt -> fprintf fmt "return;")
  else
    (* C90 code *)
    let (gen_locals, base_locals) = List.partition (fun v -> Types.is_generic_type v.var_type) m.mstep.step_locals in
    let gen_calls = List.map (fun e -> let (id, _, _) = call_of_expr e in mk_call_var_decl e.expr_loc id) m.mname.node_gencalls in
    fprintf fmt "@[<v 2>%a {@,%a%t%a%t@,%a%a%t%a%t%t@]@,}@.@."
      (print_step_prototype self) (m.mname.node_id, (m.mstep.step_inputs@gen_locals@gen_calls), m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_local_var m)) base_locals
      (Utils.pp_final_char_if_non_empty ";" base_locals)
      (* locals initialization *)
      (Utils.fprintf_list ~sep:"@," (pp_initialize m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (* locals clear *)
      (Utils.fprintf_list ~sep:"@," (pp_clear m self (pp_c_var_read m))) m.mstep.step_locals
      (Utils.pp_newline_if_non_empty m.mstep.step_locals)
      (fun fmt -> fprintf fmt "return;")


(********************************************************************************************)
(*                     MAIN C file Printing functions                                       *)
(********************************************************************************************)

let print_global_init_code fmt basename prog dependencies =
  let baseNAME = file_to_module_name basename in
  let constants = List.map const_of_top (get_consts prog) in
  fprintf fmt "@[<v 2>%a {@,static %s init = 0;@,@[<v 2>if (!init) { @,init = 1;@,%a%t%a@]@,}@,return;@]@,}@.@."
    print_global_init_prototype baseNAME
    (pp_c_basic_type_desc Type_predef.type_bool)
    (* constants *) 
    (Utils.fprintf_list ~sep:"@," (pp_const_initialize (pp_c_var_read empty_machine))) constants
    (Utils.pp_final_char_if_non_empty "@," dependencies)
    (* dependencies initialization *)
    (Utils.fprintf_list ~sep:"@," print_import_init) dependencies

let print_global_clear_code  fmt basename prog dependencies =
  let baseNAME = file_to_module_name basename in
  let constants = List.map const_of_top (get_consts prog) in
  fprintf fmt "@[<v 2>%a {@,static %s clear = 0;@,@[<v 2>if (!clear) { @,clear = 1;@,%a%t%a@]@,}@,return;@]@,}@.@."
    print_global_clear_prototype baseNAME
    (pp_c_basic_type_desc Type_predef.type_bool)
    (* constants *) 
    (Utils.fprintf_list ~sep:"@," (pp_const_clear (pp_c_var_read empty_machine))) constants
    (Utils.pp_final_char_if_non_empty "@," dependencies)
    (* dependencies initialization *)
    (Utils.fprintf_list ~sep:"@," print_import_clear) dependencies

let print_machine dependencies fmt m =
  if fst (get_stateless_status m) then
    begin
      (* Step function *)
      print_stateless_code dependencies fmt m
    end
  else
    begin
      (* Alloc functions, only if non static mode *)
      if (not !Options.static_mem) then  
	begin
	  fprintf fmt "@[<v 2>%a {@,%a%a@]@,}@.@."
	    print_alloc_prototype (m.mname.node_id, m.mstatic)
	    print_alloc_const m
	    print_alloc_code m;

	  fprintf fmt "@[<v 2>%a {@,%a%a@]@,}@.@."
	    print_dealloc_prototype m.mname.node_id
	    print_alloc_const m
	    print_dealloc_code m;
	end;
      let self = mk_self m in
      (* Reset function *)
      print_reset_code dependencies fmt m self;
      (* Step function *)
      print_step_code dependencies fmt m self;
      
      if !Options.mpfr then
	begin
          (* Init function *)
	  print_init_code dependencies fmt m self;
          (* Clear function *)
	  print_clear_code dependencies fmt m self;
	end
    end

let print_import_standard source_fmt =
  begin
    fprintf source_fmt "#include <assert.h>@.";
    if Machine_types.has_machine_type () then
      begin
	fprintf source_fmt "#include <stdint.h>@."
      end;
    if not !Options.static_mem then
      begin
	fprintf source_fmt "#include <stdlib.h>@.";
      end;
    if !Options.mpfr then
      begin
	fprintf source_fmt "#include <mpfr.h>@.";
      end
  end

let print_lib_c source_fmt basename prog machines dependencies =
  print_import_standard source_fmt;
  print_import_prototype source_fmt (Dep (true, basename, [], true (* assuming it is stateful *)));
  pp_print_newline source_fmt ();
  (* Print the svn version number and the supported C standard (C90 or C99) *)
  print_version source_fmt;
  (* Print the prototype of imported nodes *)
  fprintf source_fmt "/* Import dependencies */@.";
  fprintf source_fmt "@[<v>";
  List.iter (print_import_prototype source_fmt) dependencies;
  fprintf source_fmt "@]@.";
  (* Print consts *)
  fprintf source_fmt "/* Global constants (definitions) */@.";
  fprintf source_fmt "@[<v>";
  List.iter (fun c -> print_const_def source_fmt (const_of_top c)) (get_consts prog);
  fprintf source_fmt "@]@.";
  if !Options.mpfr then
    begin
      fprintf source_fmt "/* Global constants initialization */@.";
      print_global_init_code source_fmt basename prog dependencies;
      fprintf source_fmt "/* Global constants clearing */@.";
      print_global_clear_code source_fmt basename prog dependencies;
    end;
  if not !Options.static_mem then
    begin
      fprintf source_fmt "/* External allocation function prototypes */@.";
      fprintf source_fmt "@[<v>";
      List.iter (print_extern_alloc_prototypes source_fmt) dependencies;
      fprintf source_fmt "@]@.";
      fprintf source_fmt "/* Node allocation function prototypes */@.";
      fprintf source_fmt "@[<v>";
      List.iter
	(fun m -> fprintf source_fmt "%a;@.@.%a;@.@."
	  print_alloc_prototype (m.mname.node_id, m.mstatic)
	  print_dealloc_prototype m.mname.node_id
	)
	machines;
      fprintf source_fmt "@]@.";
    end;

  (* Print the struct definitions of all machines. *)
  fprintf source_fmt "/* Struct definitions */@.";
  fprintf source_fmt "@[<v>";
  List.iter (print_machine_struct source_fmt) machines;
  fprintf source_fmt "@]@.";
  pp_print_newline source_fmt ();
  (* Print nodes one by one (in the previous order) *)
  List.iter (print_machine dependencies source_fmt) machines;
 end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
