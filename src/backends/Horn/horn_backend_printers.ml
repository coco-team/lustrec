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

(* The compilation presented here was first defined in Garoche, Gurfinkel,
   Kahsai, HCSV'14.

   This is a modified version that handle reset
*)

open Format
open Lustre_types
open Machine_code_types
open Corelang
open Machine_code_common

open Horn_backend_common
  
(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)

let pp_horn_var m fmt id =
  (*if Types.is_array_type id.var_type
  then
    assert false (* no arrays in Horn output *)
  else*)
    fprintf fmt "%s" id.var_id

(* Used to print boolean constants *)
let pp_horn_tag fmt t =
  pp_print_string fmt (if t = tag_true then "true" else if t = tag_false then "false" else t)
    
(* Prints a constant value *)
let rec pp_horn_const fmt c =
  match c with
    | Const_int i    -> pp_print_int fmt i
    | Const_real (_,_,s)   -> pp_print_string fmt s
    | Const_tag t    -> pp_horn_tag fmt t
    | _              -> assert false

(* PL comment 2017/01/03: Useless code, the function existed before in typing.ml *)
(* let rec get_type_cst c = *)
(*   match c with *)
(*   | Const_int(n) -> new_ty Tint *)
(*   | Const_real _ -> new_ty Treal *)
(*   (\* | Const_float _ -> new_ty Treal *\) *)
(*   | Const_array(l) -> new_ty (Tarray(Dimension.mkdim_int (Location.dummy_loc) (List.length l), *)
(* 				     get_type_cst (List.hd l))) *)
(*   | Const_tag(tag) -> new_ty Tbool *)
(*   | Const_string(str) ->  assert false(\* used only for annotations *\) *)
(*   | Const_struct(l) -> new_ty (Tstruct(List.map (fun (label, t) -> (label, get_type_cst t)) l)) *)

(* PL comment 2017/01/03: the following function get_type seems useless to me: it looks like computing the type of a machine code expression v while v.value_type should contain this information. The code is kept for the moment in case I missed something *)

(*
let rec get_type v =
  match v with
  | Cst c -> Typing.type_const Location.dummy_loc c (* get_type_cst c*)
  | Access(tab, index) -> begin
      let rec remove_link ltype =
        match (dynamic_type ltype).tdesc with
        | Tlink t -> t
        | _ -> ltype
      in
      match (dynamic_type (remove_link (get_type tab))).tdesc with
      | Tarray(size, t) -> remove_link t
      | Tvar -> Format.eprintf "Type of access is a variable... "; assert false
      | Tunivar -> Format.eprintf "Type of access is a variable... "; assert false
      | _ -> Format.eprintf "Type of access is not an array "; assert false
                          end
  | Power(v, n) -> assert false
  | LocalVar v -> v.var_type
  | StateVar v -> v.var_type
  | Fun(n, vl) -> begin match n with
                  | "+"
                  | "-"
                  | "*" -> get_type (List.hd vl)
                  | _ -> Format.eprintf "Function undealt with : %s" n ;assert false
                  end
  | Array(l) -> new_ty (Tarray(Dimension.mkdim_int
                                 (Location.dummy_loc)
                                 (List.length l),
                               get_type (List.hd l)))
  | _ -> assert false
*)

(* Default value for each type, used when building arrays. Eg integer array
   [2;7] is defined as (store (store (0) 1 7) 0 2) where 0 is this default value
   for the type integer (arrays).
*)
let rec pp_default_val fmt t =
  let t = Types.dynamic_type t in
  if Types.is_bool_type t  then fprintf fmt "true" else
  if Types.is_int_type t then fprintf fmt "0" else 
  if Types.is_real_type t then fprintf fmt "0" else 
  match (Types.dynamic_type t).Types.tdesc with
  | Types.Tarray(dim, l) -> (* TODO PL: this strange code has to be (heavily) checked *)
     let valt = Types.array_element_type t in
     fprintf fmt "((as const (Array Int %a)) %a)"
       pp_type valt 
       pp_default_val valt
  | Types.Tstruct(l) -> assert false
  | Types.Ttuple(l) -> assert false
  |_ -> assert false


let pp_basic_lib_fun i pp_val fmt vl =
  match i, vl with
  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(@[<hov 2>ite %a@ %a@ %a@])" pp_val v1 pp_val v2 pp_val v3

  | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
  | "not", [v] -> Format.fprintf fmt "(not %a)" pp_val v
  | "=", [v1; v2] -> Format.fprintf fmt "(= %a %a)" pp_val v1 pp_val v2
  | "&&", [v1; v2] -> Format.fprintf fmt "(and %a %a)" pp_val v1 pp_val v2
  | "||", [v1; v2] -> Format.fprintf fmt "(or %a %a)" pp_val v1 pp_val v2
  | "impl", [v1; v2] -> Format.fprintf fmt "(=> %a %a)" pp_val v1 pp_val v2
  | "mod", [v1; v2] -> Format.fprintf fmt "(mod %a %a)" pp_val v1 pp_val v2
  | "equi", [v1; v2] -> Format.fprintf fmt "(%a = %a)" pp_val v1 pp_val v2
  | "xor", [v1; v2] -> Format.fprintf fmt "(%a xor %a)" pp_val v1 pp_val v2
  | "!=", [v1; v2] -> Format.fprintf fmt "(not (= %a %a))" pp_val v1 pp_val v2
  | "/", [v1; v2] -> Format.fprintf fmt "(div %a %a)" pp_val v1 pp_val v2
  | _, [v1; v2] -> Format.fprintf fmt "(%s %a %a)" i pp_val v1 pp_val v2
  | _ -> (Format.eprintf "internal error: Basic_library.pp_horn %s@." i; assert false)
(*  | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2

*)


(* Prints a value expression [v], with internal function calls only.
   [pp_var] is a printer for variables (typically [pp_c_var_read]),
   but an offset suffix may be added for array variables
*)
let rec pp_horn_val ?(is_lhs=false) self pp_var fmt v =
  match v.value_desc with
  | Cst c       -> pp_horn_const fmt c

  (* Code specific for arrays *)
  | Array il    ->
     (* An array definition: 
	(store (
	  ...
 	    (store (
	       store (
	          default_val
	       ) 
	       idx_n val_n
	    ) 
	    idx_n-1 val_n-1)
	  ... 
	  idx_1 val_1
	) *)
     let rec print fmt (tab, x) =
       match tab with
       | [] -> pp_default_val fmt v.value_type(* (get_type v) *)
       | h::t ->
	  fprintf fmt "(store %a %i %a)"
	    print (t, (x+1))
	    x
	    (pp_horn_val ~is_lhs:is_lhs self pp_var) h
     in
     print fmt (il, 0)
       
  | Access(tab,index) ->
     fprintf fmt "(select %a %a)"
       (pp_horn_val ~is_lhs:is_lhs self pp_var) tab
       (pp_horn_val ~is_lhs:is_lhs self pp_var) index

  (* Code specific for arrays *)
    
  | Power (v, n)  -> assert false
  | LocalVar v    -> pp_var fmt (rename_machine self v)
  | StateVar v    ->
     if Types.is_array_type v.var_type
     then assert false
     else pp_var fmt (rename_machine self ((if is_lhs then rename_next else rename_current) (* self *) v))
  | Fun (n, vl)   -> fprintf fmt "%a" (pp_basic_lib_fun n (pp_horn_val self pp_var)) vl

(* Prints a [value] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self pp_value fmt value =
 match value.value_desc with
 | Fun (n, vl)  ->
   pp_basic_lib_fun n (pp_value_suffix self pp_value) fmt vl
 |  _            ->
   pp_horn_val self pp_value fmt value

(* type_directed assignment: array vs. statically sized type
   - [var_type]: type of variable to be assigned
   - [var_name]: name of variable to be assigned
   - [value]: assigned value
   - [pp_var]: printer for variables
*)
let pp_assign m pp_var fmt var_name value =
  let self = m.mname.node_id in
  fprintf fmt "(= %a %a)" 
    (pp_horn_val ~is_lhs:true self pp_var) var_name
    (pp_value_suffix self pp_var) value
    

(* In case of no reset call, we define mid_mem = current_mem *)
let pp_no_reset machines m fmt i =
  let (n,_) = List.assoc i m.minstances in
  let target_machine = List.find (fun m  -> m.mname.node_id = (node_name n)) machines in

  let m_list = 
    rename_machine_list
      (concat m.mname.node_id i)
      (rename_mid_list (full_memory_vars machines target_machine))
  in
  let c_list =
    rename_machine_list
      (concat m.mname.node_id i)
      (rename_current_list (full_memory_vars machines target_machine))
  in
  match c_list, m_list with
  | [chd], [mhd] ->
    fprintf fmt "(= %a %a)"
      (pp_horn_var m) mhd
      (pp_horn_var m) chd
  
  | _ -> (
    fprintf fmt "@[<v 0>(and @[<v 0>";
    List.iter2 (fun mhd chd -> 
      fprintf fmt "(= %a %a)@ "
      (pp_horn_var m) mhd
      (pp_horn_var m) chd
    )
      m_list
      c_list      ;
    fprintf fmt ")@]@ @]"
  )

let pp_instance_reset machines m fmt i =
  let (n,_) = List.assoc i m.minstances in
  let target_machine = List.find (fun m  -> m.mname.node_id = (node_name n)) machines in
  
  fprintf fmt "(%a @[<v 0>%a)@]"
    pp_machine_reset_name (node_name n)
    (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) 
    (
      (rename_machine_list
	 (concat m.mname.node_id i)
	 (rename_current_list (full_memory_vars machines target_machine))
      ) 
      @
	(rename_machine_list
	   (concat m.mname.node_id i)
	   (rename_mid_list (full_memory_vars machines target_machine))
	)
    )

let pp_instance_call machines reset_instances m fmt i inputs outputs =
  let self = m.mname.node_id in
  try (* stateful node instance *)
    begin
      let (n,_) = List.assoc i m.minstances in
      let target_machine = List.find (fun m  -> m.mname.node_id = node_name n) machines in
      (* Checking whether this specific instances has been reset yet *)
      if not (List.mem i reset_instances) then
	(* If not, declare mem_m = mem_c *)
	pp_no_reset machines m fmt i;
      
      let mems = full_memory_vars machines target_machine in
      let rename_mems f = rename_machine_list (concat m.mname.node_id i) (f mems) in
      let mid_mems = rename_mems rename_mid_list in
      let next_mems = rename_mems rename_next_list in

      match node_name n, inputs, outputs, mid_mems, next_mems with
      | "_arrow", [i1; i2], [o], [mem_m], [mem_x] -> begin
	fprintf fmt "@[<v 5>(and ";
	fprintf fmt "(= %a (ite %a %a %a))"
	  (pp_horn_val ~is_lhs:true self (pp_horn_var m)) (mk_val (LocalVar o) o.var_type) (* output var *)
	  (pp_horn_var m) mem_m 
	  (pp_horn_val self (pp_horn_var m)) i1
	  (pp_horn_val self (pp_horn_var m)) i2
	;
	fprintf fmt "@ ";
	fprintf fmt "(= %a false)" (pp_horn_var m) mem_x;
	fprintf fmt ")@]"
      end

      | node_name_n -> begin
	fprintf fmt "(%a @[<v 0>%a%t%a%t%a)@]"
	  pp_machine_step_name (node_name n)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m))) inputs
	  (Utils.pp_final_char_if_non_empty "@ " inputs)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
	  (List.map (fun v -> mk_val (LocalVar v) v.var_type) outputs)
	  (Utils.pp_final_char_if_non_empty "@ " outputs)
	  (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (mid_mems@next_mems)
	
      end
    end
  with Not_found -> ( (* stateless node instance *)
    let (n,_) = List.assoc i m.mcalls in
    fprintf fmt "(%a @[<v 0>%a%t%a)@]"
      pp_machine_stateless_name (node_name n)
      (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
      inputs
      (Utils.pp_final_char_if_non_empty "@ " inputs)
      (Utils.fprintf_list ~sep:"@ " (pp_horn_val self (pp_horn_var m)))
      (List.map (fun v -> mk_val (LocalVar v) v.var_type) outputs)
  )
    
    
(* Print the instruction and update the set of reset instances *)
let rec pp_machine_instr machines reset_instances (m: machine_t) fmt instr : ident list =
  match get_instr_desc instr with
  | MComment _ -> reset_instances
  | MNoReset i -> (* we assign middle_mem with mem_m. And declare i as reset *)
    pp_no_reset machines m fmt i;
    i::reset_instances
  | MReset i -> (* we assign middle_mem with reset: reset(mem_m) *)
    pp_instance_reset machines m fmt i;
    i::reset_instances
  | MLocalAssign (i,v) ->
    pp_assign
      m (pp_horn_var m) fmt
      (mk_val (LocalVar i) i.var_type) v;
    reset_instances
  | MStateAssign (i,v) ->
    pp_assign
      m (pp_horn_var m) fmt
      (mk_val (StateVar i) i.var_type) v;
    reset_instances
  | MStep ([i0], i, vl) when Basic_library.is_internal_fun i (List.map (fun v -> v.value_type) vl) ->
    assert false (* This should not happen anymore *)
  | MStep (il, i, vl) ->
    (* if reset instance, just print the call over mem_m , otherwise declare mem_m =
       mem_c and print the call to mem_m *)
    pp_instance_call machines reset_instances m fmt i vl il;
    reset_instances (* Since this instance call will only happen once, we
		       don't have to update reset_instances *)

  | MBranch (g,hl) -> (* (g = tag1 => expr1) and (g = tag2 => expr2) ...
			 should not be produced yet. Later, we will have to
			 compare the reset_instances of each branch and
			 introduced the mem_m = mem_c for branches to do not
			 address it while other did. Am I clear ? *)
    (* For each branch we obtain the logical encoding, and the information
       whether a sub node has been reset or not. If a node has been reset in one
       of the branch, then all others have to have the mem_m = mem_c
       statement. *)
    let self = m.mname.node_id in
    let pp_branch fmt (tag, instrs) =
      fprintf fmt 
	"@[<v 3>(or (not (= %a %a))@ " 
	(*"@[<v 3>(=> (= %a %s)@ "*)  (* Issues with some versions of Z3. It
					  seems that => within Horn predicate
					  may cause trouble. I have hard time
					  producing a MWE, so I'll just keep the
					  fix here as (not a) or b *)
	(pp_horn_val self (pp_horn_var m)) g
	pp_horn_tag tag;
      let _ (* rs *) = pp_machine_instrs machines reset_instances m fmt instrs in 
      fprintf fmt "@])";
      () (* rs *)
    in
    pp_conj pp_branch fmt hl;
    reset_instances 

and pp_machine_instrs machines reset_instances m fmt instrs = 
  let ppi rs fmt i = pp_machine_instr machines rs m fmt i in
  match instrs with
  | [x] -> ppi reset_instances fmt x 
  | _::_ ->
    fprintf fmt "(and @[<v 0>";
    let rs = List.fold_left (fun rs i -> 
      let rs = ppi rs fmt i in
      fprintf fmt "@ ";
      rs
    )
      reset_instances instrs 
    in
    fprintf fmt "@])";
    rs

  | [] -> fprintf fmt "true"; reset_instances

let pp_machine_reset machines fmt m =
  let locals = local_memory_vars machines m in
  fprintf fmt "@[<v 5>(and @ ";

  (* print "x_m = x_c" for each local memory *)
  (Utils.fprintf_list ~sep:"@ " (fun fmt v -> 
    fprintf fmt "(= %a %a)"
      (pp_horn_var m) (rename_mid v)
      (pp_horn_var m) (rename_current v)
   )) fmt locals;
  fprintf fmt "@ ";

  (* print "child_reset ( associated vars _ {c,m} )" for each subnode.
     Special treatment for _arrow: _first = true
  *)
  (Utils.fprintf_list ~sep:"@ " (fun fmt (id, (n, _)) ->
    let name = node_name n in
    if name = "_arrow" then ( 
      fprintf fmt "(= %s._arrow._first_m true)"
	(concat m.mname.node_id id)  
    ) else (
      let machine_n = get_machine machines name in 
      fprintf fmt "(%s_reset @[<hov 0>%a@])" 
	name
	(Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) 
	(rename_machine_list (concat m.mname.node_id id) (reset_vars machines machine_n))
    )
   )) fmt m.minstances;

  fprintf fmt "@]@ )"



(**************************************************************)

let is_stateless m = m.minstances = [] && m.mmemory = []

(* Print the machine m:
   two functions: m_init and m_step
   - m_init is a predicate over m memories
   - m_step is a predicate over old_memories, inputs, new_memories, outputs
   We first declare all variables then the two /rules/.
*)
let print_machine machines fmt m =
  if m.mname.node_id = Arrow.arrow_id then
    (* We don't print arrow function *)
    ()
  else
    begin
      fprintf fmt "; %s@." m.mname.node_id;
      
      (* Printing variables *)
      Utils.fprintf_list ~sep:"@." pp_decl_var fmt
	(
	  (inout_vars machines m)@
	    (rename_current_list (full_memory_vars machines m)) @
	    (rename_mid_list (full_memory_vars machines m)) @
	    (rename_next_list (full_memory_vars machines m)) @
	    (rename_machine_list m.mname.node_id m.mstep.step_locals)
	);
      pp_print_newline fmt ();

      if is_stateless m then
	begin
	  (* Declaring single predicate *)
	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_stateless_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (inout_vars machines m));

          match m.mstep.step_asserts with
	  | [] ->
	     begin

	       (* Rule for single predicate *)
	       fprintf fmt "; Stateless step rule @.";
	       fprintf fmt "@[<v 2>(rule (=> @ ";
	       ignore (pp_machine_instrs machines ([] (* No reset info for stateless nodes *) )  m fmt m.mstep.step_instrs);
	       fprintf fmt "@ (%a @[<v 0>%a)@]@]@.))@.@."
		 pp_machine_stateless_name m.mname.node_id
		 (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (inout_vars machines m);
	     end
	  | assertsl ->
	     begin
	       let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id (pp_horn_var m) in
	       
	       fprintf fmt "; Stateless step rule with Assertions @.";
	       (*Rule for step*)
	       fprintf fmt "@[<v 2>(rule (=> @ (and @ ";
	       ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	       fprintf fmt "@. %a)@ (%a @[<v 0>%a)@]@]@.))@.@." (pp_conj pp_val) assertsl
		 pp_machine_stateless_name m.mname.node_id
		 (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (step_vars machines m);
	  
	     end
	       
	end
      else
	begin
	  (* Declaring predicate *)
	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_reset_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (reset_vars machines m));

	  fprintf fmt "(declare-rel %a (%a))@."
	    pp_machine_step_name m.mname.node_id
	    (Utils.fprintf_list ~sep:" " pp_type)
	    (List.map (fun v -> v.var_type) (step_vars machines m));

	  pp_print_newline fmt ();

	  (* Rule for reset *)
	  fprintf fmt "@[<v 2>(rule (=> @ %a@ (%a @[<v 0>%a)@]@]@.))@.@."
	    (pp_machine_reset machines) m 
	    pp_machine_reset_name m.mname.node_id
	    (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (reset_vars machines m);

          match m.mstep.step_asserts with
	  | [] ->
	     begin
	       fprintf fmt "; Step rule @.";
	       (* Rule for step*)
	       fprintf fmt "@[<v 2>(rule (=> @ ";
	       ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	       fprintf fmt "@ (%a @[<v 0>%a)@]@]@.))@.@."
		 pp_machine_step_name m.mname.node_id
		 (Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (step_vars machines m);
	     end
	  | assertsl -> 
	     begin
	       let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id (pp_horn_var m) in
	       (* print_string pp_val; *)
	       fprintf fmt "; Step rule with Assertions @.";
	       
	       (*Rule for step*)
	       fprintf fmt "@[<v 2>(rule (=> @ (and @ ";
	       ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	       fprintf fmt "@. %a)@ (%a @[<v 0>%a)@]@]@.))@.@." (pp_conj pp_val) assertsl
		 pp_machine_step_name m.mname.node_id
		 (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (step_vars machines m);
	     end
	       
	       
	end
    end


let mk_flags arity =
  let b_range =
   let rec range i j =
     if i > arity then [] else i :: (range (i+1) j) in
   range 2 arity;
 in
 List.fold_left (fun acc x -> acc ^ " false") "true" b_range


  (*Get sfunction infos from command line*)
let get_sf_info() =
  let splitted = Str.split (Str.regexp "@") !Options.sfunction in
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. sfunction name: %s@," !Options.sfunction);
  let sf_name, flags, arity = match splitted with
      [h;flg;par] -> h, flg, par
    | _ -> failwith "Wrong Sfunction info"

  in
  Log.report ~level:1 (fun fmt -> fprintf fmt "... sf_name: %s@, .. flags: %s@ .. arity: %s@," sf_name flags arity);
  sf_name, flags, arity


    (*a function to print the rules in case we have an s-function*)
  let print_sfunction machines fmt m =
      if m.mname.node_id = Arrow.arrow_id then
        (* We don't print arrow function *)
        ()
      else
        begin
          Format.fprintf fmt "; SFUNCTION@.";
          Format.fprintf fmt "; %s@." m.mname.node_id;
          Format.fprintf fmt "; EndPoint Predicate %s." !Options.sfunction;

          (* Check if there is annotation for s-function *)
          if m.mannot != [] then(
              Format.fprintf fmt "; @[%a@]@]@\n" (Utils.fprintf_list ~sep:"@ " Printers.pp_s_function) m.mannot;
            );

       (* Printing variables *)
          Utils.fprintf_list ~sep:"@." pp_decl_var fmt
                             ((step_vars machines m)@
    	                        (rename_machine_list m.mname.node_id m.mstep.step_locals));
          Format.pp_print_newline fmt ();
          let sf_name, flags, arity = get_sf_info() in

       if is_stateless m then
         begin
           (* Declaring single predicate *)
           Format.fprintf fmt "(declare-rel %a (%a))@."
    	                  pp_machine_stateless_name m.mname.node_id
    	                  (Utils.fprintf_list ~sep:" " pp_type)
    	                  (List.map (fun v -> v.var_type) (reset_vars machines m));
           Format.pp_print_newline fmt ();
           (* Rule for single predicate *)
           let str_flags = sf_name ^ " " ^ mk_flags (int_of_string flags) in
           Format.fprintf fmt "@[<v 2>(rule (=> @ (%s %a) (%a %a)@]@.))@.@."
                          str_flags
                          (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (reset_vars machines m)
	                  pp_machine_stateless_name m.mname.node_id
	                  (Utils.fprintf_list ~sep:" " (pp_horn_var m)) (reset_vars machines m);
         end
      else
         begin
           (* Declaring predicate *)
           Format.fprintf fmt "(declare-rel %a (%a))@."
    	                  pp_machine_reset_name m.mname.node_id
    	                  (Utils.fprintf_list ~sep:" " pp_type)
    	                  (List.map (fun v -> v.var_type) (inout_vars machines m));

           Format.fprintf fmt "(declare-rel %a (%a))@."
    	                  pp_machine_step_name m.mname.node_id
    	                  (Utils.fprintf_list ~sep:" " pp_type)
    	                  (List.map (fun v -> v.var_type) (step_vars machines m));

           Format.pp_print_newline fmt ();
          (* Adding assertions *)
           match m.mstep.step_asserts with
	  | [] ->
	    begin

	      (* Rule for step*)
	      fprintf fmt "@[<v 2>(rule (=> @ ";
	      ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	      fprintf fmt "@ (%a @[<v 0>%a)@]@]@.))@.@."
		pp_machine_step_name m.mname.node_id
		(Utils.fprintf_list ~sep:"@ " (pp_horn_var m)) (step_vars machines m);
	    end
	  | assertsl ->
	    begin
	      let pp_val = pp_horn_val ~is_lhs:true m.mname.node_id (pp_horn_var m) in
	      (* print_string pp_val; *)
	      fprintf fmt "; with Assertions @.";

	      (*Rule for step*)
	      fprintf fmt "@[<v 2>(rule (=> @ (and @ ";
	      ignore (pp_machine_instrs machines [] m fmt m.mstep.step_instrs);
	      fprintf fmt "@. %a)(%a @[<v 0>%a)@]@]@.))@.@." (pp_conj pp_val) assertsl
		pp_machine_step_name m.mname.node_id
		(Utils.fprintf_list ~sep:" " (pp_horn_var m)) (step_vars machines m);
	    end

         end

        end


(**************** XML printing functions *************)

	  let rec pp_xml_expr fmt expr =
  (match expr.expr_annot with 
  | None -> fprintf fmt "%t" 
  | Some ann -> fprintf fmt "@[(%a %t)@]" pp_xml_expr_annot ann)
    (fun fmt -> 
      match expr.expr_desc with
    | Expr_const c -> Printers.pp_const fmt c
    | Expr_ident id -> fprintf fmt "%s" id
    | Expr_array a -> fprintf fmt "[%a]" pp_xml_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_xml_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_xml_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_xml_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "@[<hov 1>(if %a then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@])" pp_xml_expr c pp_xml_expr t pp_xml_expr e
    | Expr_arrow (e1, e2) -> fprintf fmt "(%a -> %a)" pp_xml_expr e1 pp_xml_expr e2
    | Expr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_xml_expr e1 pp_xml_expr e2
    | Expr_pre e -> fprintf fmt "pre %a" pp_xml_expr e
    | Expr_when (e, id, l) -> fprintf fmt "%a when %s(%s)" pp_xml_expr e l id
    | Expr_merge (id, hl) -> 
      fprintf fmt "merge %s %a" id pp_xml_handlers hl
    | Expr_appl (id, e, r) -> pp_xml_app fmt id e r
    )
and pp_xml_tuple fmt el =
 Utils.fprintf_list ~sep:"," pp_xml_expr fmt el

and pp_xml_handler fmt (t, h) =
 fprintf fmt "(%s -> %a)" t pp_xml_expr h

and pp_xml_handlers fmt hl =
 Utils.fprintf_list ~sep:" " pp_xml_handler fmt hl

and pp_xml_app fmt id e r =
  match r with
  | None -> pp_xml_call fmt id e
  | Some c -> fprintf fmt "%t every (%a)" (fun fmt -> pp_xml_call fmt id e) pp_xml_expr c 

and pp_xml_call fmt id e =
  match id, e.expr_desc with
  | "+", Expr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_xml_expr e1 pp_xml_expr e2
  | "uminus", _ -> fprintf fmt "(- %a)" pp_xml_expr e
  | "-", Expr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_xml_expr e1 pp_xml_expr e2
  | "*", Expr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_xml_expr e1 pp_xml_expr e2
  | "/", Expr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_xml_expr e1 pp_xml_expr e2
  | "mod", Expr_tuple([e1;e2]) -> fprintf fmt "(%a mod %a)" pp_xml_expr e1 pp_xml_expr e2
  | "&&", Expr_tuple([e1;e2]) -> fprintf fmt "(%a and %a)" pp_xml_expr e1 pp_xml_expr e2
  | "||", Expr_tuple([e1;e2]) -> fprintf fmt "(%a or %a)" pp_xml_expr e1 pp_xml_expr e2
  | "xor", Expr_tuple([e1;e2]) -> fprintf fmt "(%a xor %a)" pp_xml_expr e1 pp_xml_expr e2
  | "impl", Expr_tuple([e1;e2]) -> fprintf fmt "(%a => %a)" pp_xml_expr e1 pp_xml_expr e2
  | "<", Expr_tuple([e1;e2]) -> fprintf fmt "(%a &lt; %a)" pp_xml_expr e1 pp_xml_expr e2
  | "<=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a &lt;= %a)" pp_xml_expr e1 pp_xml_expr e2
  | ">", Expr_tuple([e1;e2]) -> fprintf fmt "(%a &gt; %a)" pp_xml_expr e1 pp_xml_expr e2
  | ">=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a &gt;= %a)" pp_xml_expr e1 pp_xml_expr e2
  | "!=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a != %a)" pp_xml_expr e1 pp_xml_expr e2
  | "=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a = %a)" pp_xml_expr e1 pp_xml_expr e2
  | "not", _ -> fprintf fmt "(not %a)" pp_xml_expr e
  | _, Expr_tuple _ -> fprintf fmt "%s %a" id pp_xml_expr e
  | _ -> fprintf fmt "%s (%a)" id pp_xml_expr e

and pp_xml_eexpr fmt e =
  fprintf fmt "%a%t %a"
    (Utils.fprintf_list ~sep:"; " Printers.pp_quantifiers) e.eexpr_quantifiers
    (fun fmt -> match e.eexpr_quantifiers with [] -> () | _ -> fprintf fmt ";")
    pp_xml_expr e.eexpr_qfexpr

and  pp_xml_sf_value fmt e =
   fprintf fmt "%a"
     (* (Utils.fprintf_list ~sep:"; " pp_xml_quantifiers) e.eexpr_quantifiers *)
     (* (fun fmt -> match e.eexpr_quantifiers *)
     (*             with [] -> () *)
     (*                | _ -> fprintf fmt ";") *)
     pp_xml_expr e.eexpr_qfexpr

and pp_xml_s_function fmt expr_ann =
  let pp_xml_annot fmt (kwds, ee) =
    Format.fprintf fmt " %t : %a"
                   (fun fmt -> match kwds with
                               | [] -> assert false
                               | [x] -> Format.pp_print_string fmt x
                               | _ -> Format.fprintf fmt "%a" (Utils.fprintf_list ~sep:"/" Format.pp_print_string) kwds)
                   pp_xml_sf_value ee
  in
  Utils.fprintf_list ~sep:"@ " pp_xml_annot fmt expr_ann.annots

and pp_xml_expr_annot fmt expr_ann =
  let pp_xml_annot fmt (kwds, ee) =
    Format.fprintf fmt "(*! %t: %a; *)"
      (fun fmt -> match kwds with | [] -> assert false | [x] -> Format.pp_print_string fmt x | _ -> Format.fprintf fmt "/%a/" (Utils.fprintf_list ~sep:"/" Format.pp_print_string) kwds)
      pp_xml_eexpr ee
  in
  Utils.fprintf_list ~sep:"@ " pp_xml_annot fmt expr_ann.annots


(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
