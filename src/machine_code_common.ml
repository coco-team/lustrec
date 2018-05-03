open Lustre_types
open Machine_code_types
open Corelang
  
let print_statelocaltag = true

let rec pp_val fmt v =
  match v.value_desc with
    | Cst c         -> Printers.pp_const fmt c 
    | LocalVar v    ->
       if print_statelocaltag then
	 Format.fprintf fmt "%s(L)" v.var_id
       else
	 Format.pp_print_string fmt v.var_id
	   
    | StateVar v    ->
       if print_statelocaltag then
	 Format.fprintf fmt "%s(S)" v.var_id
       else
	 Format.pp_print_string fmt v.var_id
    | Array vl      -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:", " pp_val)  vl
    | Access (t, i) -> Format.fprintf fmt "%a[%a]" pp_val t pp_val i
    | Power (v, n)  -> Format.fprintf fmt "(%a^%a)" pp_val v pp_val n
    | Fun (n, vl)   -> Format.fprintf fmt "%s (%a)" n (Utils.fprintf_list ~sep:", " pp_val)  vl

let rec pp_instr fmt i =
  let _ =
    match i.instr_desc with
    | MLocalAssign (i,v) -> Format.fprintf fmt "%s<-l- %a" i.var_id pp_val v
    | MStateAssign (i,v) -> Format.fprintf fmt "%s<-s- %a" i.var_id pp_val v
    | MReset i           -> Format.fprintf fmt "reset %s" i
    | MNoReset i         -> Format.fprintf fmt "noreset %s" i
    | MStep (il, i, vl)  ->
       Format.fprintf fmt "%a = %s (%a)"
	 (Utils.fprintf_list ~sep:", " (fun fmt v -> Format.pp_print_string fmt v.var_id)) il
	 i
	 (Utils.fprintf_list ~sep:", " pp_val) vl
    | MBranch (g,hl)     ->
       Format.fprintf fmt "@[<v 2>case(%a) {@,%a@,}@]"
	 pp_val g
	 (Utils.fprintf_list ~sep:"@," pp_branch) hl
    | MComment s -> Format.pp_print_string fmt s
       
  in
  (* Annotation *)
  (* let _ = *)
  (*   match i.lustre_expr with None -> () | Some e -> Format.fprintf fmt " -- original expr: %a" Printers.pp_expr e *)
  (* in *)
  let _ = 
    match i.lustre_eq with None -> () | Some eq -> Format.fprintf fmt " -- original eq: %a" Printers.pp_node_eq eq
  in
  ()
    
and pp_branch fmt (t, h) =
  Format.fprintf fmt "@[<v 2>%s:@,%a@]" t (Utils.fprintf_list ~sep:"@," pp_instr) h

and pp_instrs fmt il =
  Format.fprintf fmt "@[<v 2>%a@]" (Utils.fprintf_list ~sep:"@," pp_instr) il


(* merge log: get_node_def was in c0f8 *)
(* Returns the node/machine associated to id in m calls *)
let get_node_def id m =
  try
    let (decl, _) = List.assoc id m.mcalls in
    Corelang.node_of_top decl
  with Not_found -> ( 
    (* Format.eprintf "Unable to find node %s in list [%a]@.@?" *)
    (*   id *)
    (*   (Utils.fprintf_list ~sep:", " (fun fmt (n,_) -> Format.fprintf fmt "%s" n)) m.mcalls *)
    (* ; *)
    raise Not_found
  )
    
(* merge log: machine_vars was in 44686 *)
let machine_vars m = m.mstep.step_inputs @ m.mstep.step_locals @ m.mstep.step_outputs @ m.mmemory

let pp_step fmt s =
  Format.fprintf fmt "@[<v>inputs : %a@ outputs: %a@ locals : %a@ checks : %a@ instrs : @[%a@]@ asserts : @[%a@]@]@ "
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_inputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_outputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_locals
    (Utils.fprintf_list ~sep:", " (fun fmt (_, c) -> pp_val fmt c)) s.step_checks
    (Utils.fprintf_list ~sep:"@ " pp_instr) s.step_instrs
    (Utils.fprintf_list ~sep:", " pp_val) s.step_asserts


let pp_static_call fmt (node, args) =
 Format.fprintf fmt "%s<%a>"
   (node_name node)
   (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) args

let pp_machine fmt m =
  Format.fprintf fmt
    "@[<v 2>machine %s@ mem      : %a@ instances: %a@ init     : %a@ const    : %a@ step     :@   @[<v 2>%a@]@ @  spec : @[%t@]@  annot : @[%a@]@]@ "
    m.mname.node_id
    (Utils.fprintf_list ~sep:", " Printers.pp_var) m.mmemory
    (Utils.fprintf_list ~sep:", " (fun fmt (o1, o2) -> Format.fprintf fmt "(%s, %a)" o1 pp_static_call o2)) m.minstances
    (Utils.fprintf_list ~sep:"@ " pp_instr) m.minit
    (Utils.fprintf_list ~sep:"@ " pp_instr) m.mconst
    pp_step m.mstep
    (fun fmt -> match m.mspec with | None -> () | Some spec -> Printers.pp_spec fmt spec)
    (Utils.fprintf_list ~sep:"@ " Printers.pp_expr_annot) m.mannot

let pp_machines fmt ml =
  Format.fprintf fmt "@[<v 0>%a@]" (Utils.fprintf_list ~sep:"@," pp_machine) ml

  
let rec is_const_value v =
  match v.value_desc with
  | Cst _          -> true
  | Fun (id, args) -> Basic_library.is_value_internal_fun v && List.for_all is_const_value args
  | _              -> false

(* Returns the declared stateless status and the computed one. *)
let get_stateless_status m =
 (m.mname.node_dec_stateless, try Utils.desome m.mname.node_stateless with _ -> failwith ("stateless status of machine " ^ m.mname.node_id ^ " not computed"))

let is_input m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_inputs

let is_output m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_outputs

let is_memory m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mmemory

let mk_conditional ?lustre_eq c t e =
  mkinstr ?lustre_eq:lustre_eq  (MBranch(c, [ (tag_true, t); (tag_false, e) ]))



let mk_val v t =
  { value_desc = v; 
    value_type = t; 
    value_annot = None }
    
let arrow_machine =
  let state = "_first" in
  let var_state = dummy_var_decl state Type_predef.type_bool(* (Types.new_ty Types.Tbool) *) in
  let var_input1 = List.nth Arrow.arrow_desc.node_inputs 0 in
  let var_input2 = List.nth Arrow.arrow_desc.node_inputs 1 in
  let var_output = List.nth Arrow.arrow_desc.node_outputs 0 in
  let cst b = mk_val (Cst (const_of_bool b)) Type_predef.type_bool in
  let t_arg = Types.new_univar () in (* TODO Xavier: c'est bien la bonne def ? *)
  {
    mname = Arrow.arrow_desc;
    mmemory = [var_state];
    mcalls = [];
    minstances = [];
    minit = [mkinstr (MStateAssign(var_state, cst true))];
    mstatic = [];
    mconst = [];
    mstep = {
      step_inputs = Arrow.arrow_desc.node_inputs;
      step_outputs = Arrow.arrow_desc.node_outputs;
      step_locals = [];
      step_checks = [];
      step_instrs = [mk_conditional (mk_val (StateVar var_state) Type_predef.type_bool)
			(List.map mkinstr
			[MStateAssign(var_state, cst false);
			 MLocalAssign(var_output, mk_val (LocalVar var_input1) t_arg)])
                        (List.map mkinstr
			[MLocalAssign(var_output, mk_val (LocalVar var_input2) t_arg)]) ];
      step_asserts = [];
    };
    mspec = None;
    mannot = [];
  }

let empty_desc =
  {
    node_id = Arrow.arrow_id;
    node_type = Types.bottom;
    node_clock = Clocks.bottom;
    node_inputs= [];
    node_outputs= [];
    node_locals= [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_stmts= [];
    node_dec_stateless = true;
    node_stateless = Some true;
    node_spec = None;
    node_annot = [];  }

let empty_machine =
  {
    mname = empty_desc;
    mmemory = [];
    mcalls = [];
    minstances = [];
    minit = [];
    mstatic = [];
    mconst = [];
    mstep = {
      step_inputs = [];
      step_outputs = [];
      step_locals = [];
      step_checks = [];
      step_instrs = [];
      step_asserts = [];
    };
    mspec = None;
    mannot = [];
  }

let new_instance =
  let cpt = ref (-1) in
  fun caller callee tag ->
    begin
      let o =
	if Stateless.check_node callee then
	  node_name callee
	else
	  Printf.sprintf "ni_%d" (incr cpt; !cpt) in
      let o =
	if !Options.ansi && is_generic_node callee
	then Printf.sprintf "%s_inst_%d" o (Utils.position (fun e -> e.expr_tag = tag) caller.node_gencalls)
	else o in
      o
    end


let get_machine_opt name machines =
  List.fold_left
    (fun res m ->
      match res with
      | Some _ -> res
      | None -> if m.mname.node_id = name then Some m else None)
    None machines

let get_const_assign m id =
  try
    match get_instr_desc (List.find
	     (fun instr -> match get_instr_desc instr with
	     | MLocalAssign (v, _) -> v == id
	     | _ -> false)
	     m.mconst
    ) with
    | MLocalAssign (_, e) -> e
    | _                   -> assert false
  with Not_found -> assert false


let value_of_ident loc m id =
  (* is is a state var *)
  try
    let v = List.find (fun v -> v.var_id = id) m.mmemory
    in mk_val (StateVar v) v.var_type 
  with Not_found ->
    try (* id is a node var *)
      let v = get_node_var id m.mname
      in mk_val (LocalVar v) v.var_type
  with Not_found ->
    try (* id is a constant *)
      let c = Corelang.var_decl_of_const (const_of_top (Hashtbl.find Corelang.consts_table id))
      in mk_val (LocalVar c) c.var_type
    with Not_found ->
      (* id is a tag *)
      let t = Const_tag id
      in mk_val (Cst t) (Typing.type_const loc t)

(* type of internal fun used in dimension expression *)
let type_of_value_appl f args =
  if List.mem f Basic_library.arith_funs
  then (List.hd args).value_type
  else Type_predef.type_bool

let rec value_of_dimension m dim =
  match dim.Dimension.dim_desc with
  | Dimension.Dbool b         ->
     mk_val (Cst (Const_tag (if b then Corelang.tag_true else Corelang.tag_false))) Type_predef.type_bool
  | Dimension.Dint i          ->
     mk_val (Cst (Const_int i)) Type_predef.type_int
  | Dimension.Dident v        -> value_of_ident dim.Dimension.dim_loc m v
  | Dimension.Dappl (f, args) ->
     let vargs = List.map (value_of_dimension m) args
     in mk_val (Fun (f, vargs)) (type_of_value_appl f vargs) 
  | Dimension.Dite (i, t, e)  ->
     (match List.map (value_of_dimension m) [i; t; e] with
     | [vi; vt; ve] -> mk_val (Fun ("ite", [vi; vt; ve])) vt.value_type
     | _            -> assert false)
  | Dimension.Dlink dim'      -> value_of_dimension m dim'
  | _                         -> assert false

let rec dimension_of_value value =
  match value.value_desc with
  | Cst (Const_tag t) when t = Corelang.tag_true  -> Dimension.mkdim_bool  Location.dummy_loc true
  | Cst (Const_tag t) when t = Corelang.tag_false -> Dimension.mkdim_bool  Location.dummy_loc false
  | Cst (Const_int i)                             -> Dimension.mkdim_int   Location.dummy_loc i
  | LocalVar v                                    -> Dimension.mkdim_ident Location.dummy_loc v.var_id
  | Fun (f, args)                                 -> Dimension.mkdim_appl  Location.dummy_loc f (List.map dimension_of_value args)
  | _                                             -> assert false


     let rec join_branches hl1 hl2 =
 match hl1, hl2 with
 | []          , _            -> hl2
 | _           , []           -> hl1
 | (t1, h1)::q1, (t2, h2)::q2 ->
   if t1 < t2 then (t1, h1) :: join_branches q1 hl2 else
   if t1 > t2 then (t2, h2) :: join_branches hl1 q2
   else (t1, List.fold_right join_guards h1 h2) :: join_branches q1 q2

and join_guards inst1 insts2 =
 match get_instr_desc inst1, List.map get_instr_desc insts2 with
 | _                   , []                               ->
   [inst1]
 | MBranch (x1, hl1), MBranch (x2, hl2) :: q when x1 = x2 ->
    mkinstr
      (* TODO on pourrait uniquement concatener les lustres de inst1 et hd(inst2) *)
      (MBranch (x1, join_branches (sort_handlers hl1) (sort_handlers hl2)))
   :: (List.tl insts2)
 | _ -> inst1 :: insts2

let join_guards_list insts =
 List.fold_right join_guards insts []
