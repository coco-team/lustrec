open Format
open LustreSpec
open Corelang
open Machine_code
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
 match v with
 | Cst (Const_array cl) -> 1 + List.fold_right (fun c -> max (expansion_depth (Cst c))) cl 0
 | Cst _
 | LocalVar _
 | StateVar _  -> 0
 | Fun (_, vl) -> List.fold_right (fun v -> max (expansion_depth v)) vl 0
 | Array vl    -> 1 + List.fold_right (fun v -> max (expansion_depth v)) vl 0
 | Access (v, i) -> max 0 (expansion_depth v - 1)
 | Power (v, n)  -> 0 (*1 + expansion_depth v*)

type loop_index = LVar of ident | LInt of int ref

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

(* Prints a suffix of loop variables for arrays *)
let pp_suffix fmt loop_vars =
 Utils.fprintf_list ~sep:"" pp_loop_var fmt loop_vars

(* Prints a [value] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self loop_vars pp_value fmt value =
 match loop_vars, value with
 | (_, LInt r) :: q, Array vl     ->
   pp_value_suffix self q pp_value fmt (List.nth vl !r)
 | _           :: q, Power (v, n) ->
   pp_value_suffix self loop_vars pp_value fmt v
 | _               , Fun (n, vl)  ->
   Basic_library.pp_c n (pp_value_suffix self loop_vars pp_value) fmt vl
 | _               , _            ->
   let pp_var_suffix fmt v = fprintf fmt "%a%a" pp_value v pp_suffix loop_vars in
   pp_c_val self pp_var_suffix fmt value

(* type_directed assignment: array vs. statically sized type
   - [var_type]: type of variable to be assigned
   - [var_name]: name of variable to be assigned
   - [value]: assigned value
   - [pp_var]: printer for variables
*)
let pp_assign m self pp_var fmt var_type var_name value =
  let depth = expansion_depth value in
(*eprintf "pp_assign %a %a %d@." Types.print_ty var_type pp_val value depth;*)
  let loop_vars = mk_loop_variables m var_type depth in
  let reordered_loop_vars = reorder_loop_variables loop_vars in
  let rec aux fmt vars =
    match vars with
    | [] ->
      fprintf fmt "%a = %a;" 
	(pp_value_suffix self loop_vars pp_var) var_name
	(pp_value_suffix self loop_vars pp_var) value
    | (d, LVar i) :: q ->
(*eprintf "pp_aux %a %s@." Dimension.pp_dimension d i;*)
      fprintf fmt "@[<v 2>{@,int %s;@,for(%s=0;%s<%a;%s++)@,%a @]@,}"
	i i i Dimension.pp_dimension d i
	aux q
    | (d, LInt r) :: q ->
(*eprintf "pp_aux %a %d@." Dimension.pp_dimension d (!r);*)
      let szl = Utils.enumerate (Dimension.size_const_dimension d) in
      fprintf fmt "@[<v 2>{@,%a@]@,}"
	(Utils.fprintf_list ~sep:"@," (fun fmt i -> r := i; aux fmt q)) szl
  in
  begin
    reset_loop_counter ();
    (*reset_addr_counter ();*)
    aux fmt reordered_loop_vars
  end

let pp_instance_call m self fmt i (inputs: value_t list) (outputs: var_decl list) =
 try (* stateful node instance *)
   let (n,_) = List.assoc i m.minstances in
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
   fprintf fmt "%a (%a%t%a);"
     pp_machine_step_name (node_name n)
     (Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) inputs
     (Utils.pp_final_char_if_non_empty ", " inputs) 
     (Utils.fprintf_list ~sep:", " (pp_c_var_write m)) outputs 

let pp_machine_reset (m: machine_t) self fmt inst =
  let (node, static) = List.assoc inst m.minstances in
  fprintf fmt "%a(%a%t%s->%s);"
    pp_machine_reset_name (node_name node)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty ", " static)
    self inst

let has_c_prototype funname dependencies =
  let imported_node_opt = (* We select the last imported node with the name funname.
			       The order of evaluation of dependencies should be
			       compatible with overloading. (Not checked yet) *) 
      List.fold_left
	(fun res (_, _, decls) -> 
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

let rec pp_conditional dependencies (m: machine_t) self fmt c tl el =
  fprintf fmt "@[<v 2>if (%a) {%t%a@]@,@[<v 2>} else {%t%a@]@,}"
    (pp_c_val self (pp_c_var_read m)) c
    (Utils.pp_newline_if_non_empty tl)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) tl
    (Utils.pp_newline_if_non_empty el)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) el

and pp_machine_instr dependencies (m: machine_t) self fmt instr =
  match instr with 
  | MReset i ->
    pp_machine_reset m self fmt i
  | MLocalAssign (i,v) ->
    pp_assign
      m self (pp_c_var_read m) fmt
      i.var_type (LocalVar i) v
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_c_var_read m) fmt
      i.var_type (StateVar i) v
  | MStep ([i0], i, vl) when Basic_library.is_internal_fun i  ->
    pp_machine_instr dependencies m self fmt (MLocalAssign (i0, Fun (i, vl)))
  | MStep ([i0], i, vl) when has_c_prototype i dependencies -> 
    fprintf fmt "%a = %s(%a);" 
      (pp_c_val self (pp_c_var_read m)) (LocalVar i0) 
      i
      (Utils.fprintf_list ~sep:", " (pp_c_val self (pp_c_var_read m))) vl
  | MStep (il, i, vl) ->
    pp_instance_call m self fmt i vl il
  | MBranch (g,hl) ->
    if hl <> [] && let t = fst (List.hd hl) in t = tag_true || t = tag_false
    then (* boolean case, needs special treatment in C because truth value is not unique *)
	 (* may disappear if we optimize code by replacing last branch test with default *)
      let tl = try List.assoc tag_true  hl with Not_found -> [] in
      let el = try List.assoc tag_false hl with Not_found -> [] in
      pp_conditional dependencies m self fmt g tl el
    else (* enum type case *)
      fprintf fmt "@[<v 2>switch(%a) {@,%a@,}@]"
	(pp_c_val self (pp_c_var_read m)) g
	(Utils.fprintf_list ~sep:"@," (pp_machine_branch dependencies m self)) hl

and pp_machine_branch dependencies m self fmt (t, h) =
  fprintf fmt "@[<v 2>case %a:@,%a@,break;@]" pp_c_tag t (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) h


(********************************************************************************************)
(*                         C file Printing functions                                        *)
(********************************************************************************************)

let print_const_def fmt cdecl =
  fprintf fmt "%a = %a;@." 
    (pp_c_type cdecl.const_id) cdecl.const_type
    pp_c_const cdecl.const_value 


let print_alloc_instance fmt (i, (m, static)) =
  fprintf fmt "_alloc->%s = %a (%a);@,"
    i
    pp_machine_alloc_name (node_name m)
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static

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

let print_alloc_code fmt m =
  let array_mem = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "%a *_alloc;@,_alloc = (%a *) malloc(sizeof(%a));@,assert(_alloc);@,%a%areturn _alloc;"
    pp_machine_memtype_name m.mname.node_id
    pp_machine_memtype_name m.mname.node_id
    pp_machine_memtype_name m.mname.node_id
    (Utils.fprintf_list ~sep:"" print_alloc_array) array_mem
    (Utils.fprintf_list ~sep:"" print_alloc_instance) m.minstances

let print_stateless_code dependencies fmt m =
  let self = "__ERROR__" in
  if not (!Options.ansi && is_generic_node { top_decl_desc = Node m.mname; top_decl_loc = Location.dummy_loc })
  then
    (* C99 code *)
    fprintf fmt "@[<v 2>%a {@,%a%t@,%a%a%t%t@]@,}@.@."
      print_stateless_prototype (m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," pp_c_decl_local_var) m.mstep.step_locals
      (Utils.pp_final_char_if_non_empty ";@," m.mstep.step_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (fun fmt -> fprintf fmt "return;")
  else
    (* C90 code *)
    let (gen_locals, base_locals) = List.partition (fun v -> Types.is_generic_type v.var_type) m.mstep.step_locals in
    let gen_calls = List.map (fun e -> let (id, _, _) = call_of_expr e in mk_call_var_decl e.expr_loc id) m.mname.node_gencalls in
    fprintf fmt "@[<v 2>%a {@,%a%t@,%a%a%t%t@]@,}@.@."
      print_stateless_prototype (m.mname.node_id, (m.mstep.step_inputs@gen_locals@gen_calls), m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," pp_c_decl_local_var) base_locals
      (Utils.pp_final_char_if_non_empty ";" base_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (fun fmt -> fprintf fmt "return;")

let print_reset_code dependencies fmt m self =
  fprintf fmt "@[<v 2>%a {@,%a%treturn;@]@,}@.@."
    (print_reset_prototype self) (m.mname.node_id, m.mstatic)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.minit
    (Utils.pp_newline_if_non_empty m.minit)

let print_step_code dependencies fmt m self =
  if not (!Options.ansi && is_generic_node { top_decl_desc = Node m.mname; top_decl_loc = Location.dummy_loc })
  then
    (* C99 code *)
    let array_mems = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
    fprintf fmt "@[<v 2>%a {@,%a%t%a%t@,%a%a%t%t@]@,}@.@."
      (print_step_prototype self) (m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," pp_c_decl_local_var) m.mstep.step_locals
      (Utils.pp_final_char_if_non_empty ";@," m.mstep.step_locals)
      (* array mems *)
      (Utils.fprintf_list ~sep:";@," (pp_c_decl_array_mem self)) array_mems
      (Utils.pp_final_char_if_non_empty ";@," array_mems)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (fun fmt -> fprintf fmt "return;")
  else
    (* C90 code *)
    let (gen_locals, base_locals) = List.partition (fun v -> Types.is_generic_type v.var_type) m.mstep.step_locals in
    let gen_calls = List.map (fun e -> let (id, _, _) = call_of_expr e in mk_call_var_decl e.expr_loc id) m.mname.node_gencalls in
    fprintf fmt "@[<v 2>%a {@,%a%t@,%a%a%t%t@]@,}@.@."
      (print_step_prototype self) (m.mname.node_id, (m.mstep.step_inputs@gen_locals@gen_calls), m.mstep.step_outputs)
      (* locals *)
      (Utils.fprintf_list ~sep:";@," pp_c_decl_local_var) base_locals
      (Utils.pp_final_char_if_non_empty ";" base_locals)
      (* check assertions *)
      (pp_c_checks self) m
      (* instrs *)
      (Utils.fprintf_list ~sep:"@," (pp_machine_instr dependencies m self)) m.mstep.step_instrs
      (Utils.pp_newline_if_non_empty m.mstep.step_instrs)
      (fun fmt -> fprintf fmt "return;")


(********************************************************************************************)
(*                     MAIN C file Printing functions                                       *)
(********************************************************************************************)

let print_machine dependencies fmt m =
  if fst (get_stateless_status m) then
    begin
      (* Step function *)
      print_stateless_code dependencies fmt m
    end
  else
    begin
      (* Alloc function, only if non static mode *)
      if (not !Options.static_mem) then  
	(
	  fprintf fmt "@[<v 2>%a {@,%a@]@,}@.@."
	    print_alloc_prototype (m.mname.node_id, m.mstatic)
	    print_alloc_code m;
	);
      let self = mk_self m in
      (* Reset function *)
      print_reset_code dependencies fmt m self;
      (* Step function *)
      print_step_code dependencies fmt m self
    end


let print_lib_c source_fmt basename prog machines dependencies =

  fprintf source_fmt "#include <stdlib.h>@.#include <assert.h>@.#include \"%s\"@.@." (basename^".h");
  (* Print the svn version number and the supported C standard (C90 or C99) *)
  print_version source_fmt;
  (* Print the prototype of imported nodes *)
  fprintf source_fmt "/* Imported nodes declarations */@.";
  fprintf source_fmt "@[<v>";
  List.iter (print_import_prototype source_fmt) dependencies;
  fprintf source_fmt "@]@.";
  (* Print consts *)
  fprintf source_fmt "/* Global constants (definitions) */@.";
  List.iter (fun c -> print_const_def source_fmt c) (get_consts prog);
  pp_print_newline source_fmt ();
  (* Print nodes one by one (in the previous order) *)
  List.iter (print_machine dependencies source_fmt) machines;
 end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
