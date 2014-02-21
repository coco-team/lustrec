open Format
open LustreSpec
open Corelang
open Machine_code


let pp_machine_reset_name fmt id = fprintf fmt "%s_reset" id
let pp_machine_step_name fmt id = fprintf fmt "%s_step" id

let pp_type fmt t =
  match (Types.repr t).Types.tdesc with
  | Types.Tbool           -> Format.fprintf fmt "Bool"
  | Types.Tint            -> Format.fprintf fmt "Int"
  | Types.Tclock _
  | Types.Tarray _
  | Types.Tstatic _
  | Types.Tconst _
  | Types.Tarrow _
  | _                     -> Format.eprintf "internal error: pp_type %a@." 
                             Types.print_ty t; assert false
    

let pp_decl_var fmt id = 
  Format.fprintf fmt "(declare_var %s %a)"
    id.var_id
    pp_type id.var_type

let pp_var fmt id = Format.pp_print_string fmt id.var_id

let pp_instr machine_name fmt i =
  Format.fprintf fmt "(xxx)"

let rename f = (fun v -> {v with var_id = f v.var_id } )
let rename_current machine_name =  rename (fun n -> machine_name ^ "." ^ n ^ "_c")
let rename_current_list machine_name = List.map (rename_current machine_name)
let rename_next machine_name = rename (fun n -> machine_name ^ "." ^ n ^ "_x")
let rename_next_list machine_name = List.map (rename_next machine_name)
let rename_machine machine_name = rename (fun n -> machine_name ^ "." ^ n)
let rename_machine_list machine_name = List.map (rename_machine machine_name)

let full_memory_vars machine instance =
  (rename_current_list machine.mname.node_id machine.mmemory) @
    (rename_next_list machine.mname.node_id machine.mmemory)

let machine_vars m = 
    (rename_machine_list m.mname.node_id m.mstep.step_inputs)@
    (rename_machine_list m.mname.node_id m.mstep.step_outputs)@
  full_memory_vars m ()

  
(********************************************************************************************)
(*                    Instruction Printing functions                                        *)
(********************************************************************************************)

let pp_horn_var m fmt id =
  if Types.is_array_type id.var_type
  then
    assert false (* no arrays in Horn output *)
  else
    Format.fprintf fmt "%s" id.var_id


(* Used to print boolean constants *)
let pp_horn_tag fmt t =
  pp_print_string fmt (if t = tag_true then "1" else if t = tag_false then "0" else t)

(* Prints a constant value *)
let rec pp_horn_const fmt c =
  match c with
    | Const_int i    -> pp_print_int fmt i
    | Const_real r   -> pp_print_string fmt r
    | Const_float r  -> pp_print_float fmt r
    | Const_tag t    -> pp_horn_tag fmt t
    | Const_array ca -> assert false

(* Prints a value expression [v], with internal function calls only.
   [pp_var] is a printer for variables (typically [pp_c_var_read]),
   but an offset suffix may be added for array variables
*)
let rec pp_horn_val ?(is_lhs=false) self pp_var fmt v =
  match v with
    | Cst c         -> pp_horn_const fmt c
    | Array _      
    | Access _ -> assert false (* no arrays *)
    | Power (v, n)  -> assert false
    | LocalVar v    -> pp_var fmt (rename_machine self v)
    | StateVar v    ->
      if Types.is_array_type v.var_type
      then assert false 
      else pp_var fmt ((if is_lhs then rename_next else rename_current) self v)
    | Fun (n, vl)   -> Format.fprintf fmt "(%a)" (Basic_library.pp_horn n (pp_horn_val self pp_var)) vl

(* Prints a [value] indexed by the suffix list [loop_vars] *)
let rec pp_value_suffix self pp_value fmt value =
 match value with
 | Fun (n, vl)  ->
   Basic_library.pp_horn n (pp_value_suffix self pp_value) fmt vl
 |  _            ->
   pp_horn_val self pp_value fmt value

(* type_directed assignment: array vs. statically sized type
   - [var_type]: type of variable to be assigned
   - [var_name]: name of variable to be assigned
   - [value]: assigned value
   - [pp_var]: printer for variables
*)
let pp_assign m self pp_var fmt var_type var_name value =
  fprintf fmt "(= %a %a)" (pp_horn_val ~is_lhs:true self pp_var) var_name (pp_value_suffix self pp_var) value
  
let pp_instance_call 
    machines ?(init=false) m self fmt i (inputs: value_t list) (outputs: var_decl list) =
  try (* stateful node instance *) 
    begin
      let (n,_) = List.assoc i m.minstances in
      match node_name n, inputs, outputs with
      | "_arrow", [i1; i2], [o] -> begin
         if init then
           pp_assign
   	     m
   	     self
   	     (pp_horn_var m)
	     (* (pp_horn_val self (pp_horn_var m) fmt o) *)  fmt
   	     o.var_type (LocalVar o) i1
         else
           pp_assign
   	     m self (pp_horn_var m) fmt
   	     o.var_type (LocalVar o) i2
	     
      end
      | name, _, _ ->  
	begin
	  let target_machine = List.find (fun m  -> m.mname.node_id = name) machines in
	  Format.fprintf fmt "(%s_%s %a%t%a%t%a)"
	    (node_name n) 
	    (if init then "init" else "step")
	    (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) inputs
	    (Utils.pp_final_char_if_non_empty " " inputs) 
	    (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) (List.map (fun v -> LocalVar v) outputs)
	       (Utils.pp_final_char_if_non_empty " " outputs)
	    (Utils.fprintf_list ~sep:" " pp_var) (full_memory_vars target_machine i)
	    
	     end
    end
    with Not_found -> ( (* stateless node instance *)
      let (n,_) = List.assoc i m.mcalls in
   Format.fprintf fmt "(%s %a%t%a)"
     (node_name n)
     (Utils.fprintf_list ~sep:" " (pp_horn_val self (pp_horn_var m))) inputs
     (Utils.pp_final_char_if_non_empty " " inputs) 
     (Utils.fprintf_list ~sep:" " (pp_horn_var m)) outputs 
    )

let pp_machine_reset (m: machine_t) self fmt inst =
  let (node, static) = List.assoc inst m.minstances in
  fprintf fmt "(%a %a%t%s->%s)"
    pp_machine_reset_name (node_name node)
    (Utils.fprintf_list ~sep:" " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty " " static)
    self inst

(* TODO *)
let rec pp_conditional machines ?(init=false)  (m: machine_t) self fmt c tl el =
  fprintf fmt "@[<v 2>if (%a) {%t%a@]@,@[<v 2>} else {%t%a@]@,}"
    (pp_horn_val self (pp_horn_var m)) c
    (Utils.pp_newline_if_non_empty tl)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) tl
    (Utils.pp_newline_if_non_empty el)
    (Utils.fprintf_list ~sep:"@," (pp_machine_instr machines ~init:init  m self)) el

and pp_machine_instr machines ?(init=false) (m: machine_t) self fmt instr =
  match instr with 
  | MReset i ->
    pp_machine_reset m self fmt i
  | MLocalAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (LocalVar i) v
  | MStateAssign (i,v) ->
    pp_assign
      m self (pp_horn_var m) fmt
      i.var_type (StateVar i) v
  | MStep ([i0], i, vl) when Basic_library.is_internal_fun i  ->
    pp_machine_instr machines ~init:init m self fmt (MLocalAssign (i0, Fun (i, vl)))
  | MStep (il, i, vl) ->
    pp_instance_call machines ~init:init m self fmt i vl il
  | MBranch (g,hl) ->
    if hl <> [] && let t = fst (List.hd hl) in t = tag_true || t = tag_false
    then (* boolean case, needs special treatment in C because truth value is not unique *)
	 (* may disappear if we optimize code by replacing last branch test with default *)
      let tl = try List.assoc tag_true  hl with Not_found -> [] in
      let el = try List.assoc tag_false hl with Not_found -> [] in
      pp_conditional machines ~init:init m self fmt g tl el
    else assert false (* enum type case *)


(**************************************************************)
    
(* Print the machine m: 
   two functions: m_init and m_step
   - m_init is a predicate over m memories
   - m_step is a predicate over old_memories, inputs, new_memories, outputs
   We first declare all variables then the two /rules/.
*)
let print_machine machines fmt m = 
  let pp_instr init = pp_machine_instr machines ~init:init m in
  if m.mname.node_id = arrow_id then () 
  else 
    ( (* We don't print arrow function *)
   Format.fprintf fmt "; %s@." m.mname.node_id;
   (* Printing variables *)
   Utils.fprintf_list ~sep:"@." pp_decl_var fmt 
     ((machine_vars m)@(rename_machine_list m.mname.node_id m.mstep.step_locals));
   Format.pp_print_newline fmt ();
   (* Declaring predicate *)
   Format.fprintf fmt "(declare-rel %a (%a))@."
     pp_machine_reset_name m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_type) (List.map (fun v -> v.var_type) m.mmemory);
   
   Format.fprintf fmt "(declare-rel %a (%a))@."
     pp_machine_step_name m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_type) (List.map (fun v -> v.var_type) (machine_vars m));
   Format.pp_print_newline fmt ();

   Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>%a@]@ )@ (%s_init %a)@]@.))@.@."
     (Utils.fprintf_list ~sep:"@ " (pp_instr true m.mname.node_id)) m.mstep.step_instrs
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_var) (rename_next_list m.mname.node_id m.mmemory);

   Format.fprintf fmt "@[<v 2>(rule (=> @ (and @[<v 0>%a@]@ )@ (%s_step %a)@]@.))@.@."
     (Utils.fprintf_list ~sep:"@ " (pp_instr false m.mname.node_id)) m.mstep.step_instrs
     m.mname.node_id
     (Utils.fprintf_list ~sep:" " pp_var) (machine_vars m);
   
()
  )

let main_print fmt = ()

let translate fmt basename prog machines =
  List.iter (print_machine machines fmt) (List.rev machines);
  main_print fmt 


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
