open Lustre_types 
open Corelang 
open Machine_code_types
open Machine_code_common

(* (variable, node name, node instance) *)
type scope_t = (var_decl * string * string option) list * var_decl


let scope_to_sl ((sl, v) : scope_t) : string list=
  List.fold_right (
    fun (v, nodename, _) accu -> 
      v.var_id :: nodename :: accu
  ) sl [v.var_id]

let get_node name prog =
  let node_opt = List.fold_left
    (fun res top -> 
      match res, top.top_decl_desc with
      | Some _, _ -> res
      | None, Node nd -> 
	(* Format.eprintf "Checking node %s = %s: %b@." nd.node_id name (nd.node_id = name); *)
	if nd.node_id = name then Some nd else res
      | _ -> None) 
    None prog 
  in
  try 
    Utils.desome node_opt
  with Utils.DeSome -> raise Not_found

let get_machine name machines =
  try
    List.find (fun m -> m.mname.node_id = name) machines
  with Not_found -> raise Not_found

let rec compute_scopes prog main_node : scope_t list =

  (* Format.eprintf "Compute scope of %s@." main_node; *)
  try
    let node =  get_node main_node prog in    
    let all_vars = node.node_inputs @ node.node_locals @  node.node_outputs in
    let local_vars = node.node_inputs @ node.node_locals in
    let local_scopes = List.map (fun x -> [], x) local_vars  in
    let sub_scopes =
      let sub_nodes =
	List.fold_left 
	  (fun res s -> 
	    match s with 
	    | Eq ({ eq_rhs ={ expr_desc = Expr_appl (nodeid, _, _); _}; _ } as eq) -> 
	      (* Obtaining the var_del associated to the first var of eq_lhs *)
	      (
		try
		  let query v = v.var_id = List.hd eq.eq_lhs in
		  let vid = List.find query all_vars in
		  (nodeid, vid)::res
		with Not_found -> Format.eprintf "eq=%a@.local_vars=%a@." Printers.pp_node_eq eq (Utils.fprintf_list ~sep:"," Printers.pp_var) local_vars; assert false 
	      )
	    | Eq _ -> res
	    | _ -> assert false (* TODO deal with Automaton *)
	  ) [] node.node_stmts
      in
      List.map (fun (nodeid, vid) ->
	let scopes = compute_scopes prog nodeid in
	List.map (fun (sl,v) -> (vid, nodeid, None)::sl, v) scopes (* instances are not yet known, hence the None *)
      ) sub_nodes
    in
    local_scopes @ (List.flatten sub_scopes) 
  with Not_found ->  []


let print_scopes =
  Utils.fprintf_list ~sep:"@ " 
    (fun fmt ((_, v) as s) -> Format.fprintf fmt "%a: %a" 
      (Utils.fprintf_list ~sep:"." Format.pp_print_string )(scope_to_sl s)
      Types.print_ty v.var_type)
    
     
    

(* let print_path fmt p =  *)
(*   Utils.fprintf_list ~sep:"." (fun fmt (id, _) -> Format.pp_print_string fmt id) fmt p *)

let get_node_vdecl_of_name name node =
  try
    List.find 
      (fun v -> v.var_id = name) 
      (node.node_inputs  @ node.node_outputs  @ node.node_locals ) 
  with Not_found -> 
    Format.eprintf "Cannot find variable %s in node %s@." name node.node_id;
    assert false

let scope_path main_node_name prog machines all_scopes sl : scope_t =
  let rec get_path node id_list accu =
    match id_list, accu with
    | [id], (_, last_node, _)::_ -> (* last item, it should denote a local
				       memory variable (local var, memory or input *)
      let id_vdecl = 
	get_node_vdecl_of_name id (get_node last_node prog) 
      in
      List.rev accu, id_vdecl
    | varid::nodename::id_list_tl, _ -> (
      let e_machine = get_machine node.node_id machines in 
      (* Format.eprintf "Looking for def %s in call %s in machine %a@."  *)
      (* 	varid nodename *)
      (* 	Machine_code.pp_machine e_machine; *)
      let find_var = (fun v -> v.var_id = varid) in
      let instance = 
	List.find 
	  (fun i -> match get_instr_desc i with 
	  | MStep(p, o, _) -> List.exists find_var p 
	  | _ -> false
	  ) 
	  e_machine.mstep.step_instrs 
      in
      try
	let variable, instance_node, instance_id = 
	  match get_instr_desc instance with 
	  | MStep(p, o, _) -> 
	    (* Format.eprintf "Looking for machine %s@.@?" o; *)
	    let o_fun, _ = List.assoc o e_machine.mcalls in
	    if node_name o_fun = nodename then
	      List.hd p, o_fun, o 
	    else 
	      assert false
	  | _ -> assert false
	in
	let next_node = node_of_top instance_node in
	let accu = (variable, nodename, Some instance_id)::accu in
	(* Format.eprintf "Calling get path on %s@.@?" next_node.node_id; *)
	get_path next_node id_list_tl accu
      with Not_found -> Format.eprintf "toto@."; assert false
    )
    | _ -> assert false
  in
  let all_scopes_as_sl = List.map scope_to_sl all_scopes in
  if not (List.mem sl all_scopes_as_sl) then (
    Format.eprintf "%s is an invalid scope.@." (String.concat "." sl);
    exit 1
  )
  else (
    (* Format.eprintf "@.@.Required path: %s@." (String.concat "." sl) ;  *)
    let main_node = get_node main_node_name prog in
    let path, flow = (* Special treatment of first level flow *)
      match sl with 
      | [flow] -> let flow_var = get_node_vdecl_of_name flow main_node in
		  [], flow_var 
      | _ -> get_path main_node sl [] 
	
    in
    (* Format.eprintf "computed path: %a.%s@." print_path path flow.var_id; *)
    path, flow

  )

let check_scopes main_node_name prog machines all_scopes scopes =
  List.map
    (fun sl ->
      sl, scope_path main_node_name prog machines all_scopes sl 
    ) scopes

let scopes_def : string list list ref = ref []
let inputs = ref []

let option_show_scopes = ref false
let option_scopes = ref false
let option_all_scopes = ref false
let option_mem_scopes = ref false
let option_input_scopes = ref false

let scopes_map : (Lustre_types.ident list  * scope_t) list ref  = ref []

let register_scopes s = 
  option_scopes := true;
  option_all_scopes:=false; 
  let scope_list = Str.split (Str.regexp ", *") s in
  let scope_list = List.map (fun scope -> Str.split (Str.regexp "\\.") scope) scope_list in
  scopes_def := scope_list

let register_inputs s = 
  option_scopes := true;
  let input_list = Str.split (Str.regexp "[;]") s in
  let input_list = List.map (fun s -> match Str.split (Str.regexp "=") s with | [v;e] -> v, e | _ -> raise (Invalid_argument ("Input list error: " ^ s))) input_list in
  let input_list = List.map (fun (v, e) -> v, Str.split (Str.regexp "[;]") e) input_list in
  inputs := input_list


(* TODO: recuperer le type de "flow" et appeler le print correspondant 
   iterer sur path pour construire la suite des xx_mem._reg.yy_mem._reg......flow
par ex main_mem->n8->n9->_reg.flow
*)
let extract_scopes_defs scopes =
  let rec scope_path (path, flow) accu = 
    match path with 
    | [] -> accu ^ "_reg." ^ flow.var_id, flow.var_type
    | (_, _, Some instance_id)::tl -> scope_path (tl, flow) ( accu ^ instance_id ^ "->" ) 
    | _ -> assert false
  in
  let scopes_vars = 
    List.map 
      (fun (sl, scope) -> 
	String.concat "." sl, scope_path scope "main_mem.") 
      scopes 
  in
  scopes_vars

let pp_scopes_files basename mname fmt scopes =
  let scopes_vars = extract_scopes_defs scopes in
  List.iteri (fun idx _ (* (id, (var, typ)) *) ->
    Format.fprintf fmt "FILE *f_out_scopes_%i;@ " (idx+1); (* we start from 1: in1, in2, ... *)
    Format.fprintf fmt "f_out_scopes_%i = fopen(\"%s_%s_simu.scope%i\", \"w\");@ " (idx+1) basename mname (idx+1);
  ) scopes_vars

  
let pp_scopes fmt scopes = 
  let scopes_vars = extract_scopes_defs scopes in
  List.iteri (fun idx (id, (var, typ)) ->
    Format.fprintf fmt "@ %t;" 
      (fun fmt -> C_backend_common.print_put_var fmt ("_scopes_" ^ string_of_int (idx+1)) id (*var*) typ var)
  ) scopes_vars

let update_machine machine =
  let stateassign vdecl =
    mkinstr 
    (MStateAssign (vdecl, mk_val (Var vdecl) vdecl.var_type))
  in
  let local_decls = machine.mstep.step_inputs
    (* @ machine.mstep.step_outputs   *)
    @ machine.mstep.step_locals
  in
  { machine with
    mmemory = machine.mmemory @ local_decls;
    mstep = { 
      machine.mstep with 
        step_instrs = machine.mstep.step_instrs
        @ (mkinstr (MComment "Registering all flows"))::(List.map stateassign local_decls)
          
    }
  }
    

module Plugin : (
  sig
    include PluginType.PluginType
    val show_scopes: unit -> bool
    end) =
struct
  let name = "scopes"
  let is_active () = 
    !option_scopes || !option_show_scopes || !option_all_scopes || !option_mem_scopes || !option_input_scopes
      
  let show_scopes () = 
    !option_show_scopes && (
      Compiler_common.check_main ();
      true)

  let options = [
    "-select", Arg.String register_scopes, "specifies which variables to log";
    "-input", Arg.String register_inputs, "specifies the simulation input";
    "-show-possible-scopes", Arg.Set option_show_scopes, "list possible variables to log";
    "-select-all", Arg.Set option_all_scopes, "select all possible variables to log";
    "-select-mem", Arg.Set option_mem_scopes, "select all memory variables to log";
    "-select-inputs", Arg.Set option_input_scopes, "select all input variables to log";
  ]

  let activate () = 
    option_scopes := true;
    Options.optimization := 0; (* no optimization *)
    
    (* Options.salsa_enabled := false; (\* No salsa *\) TODO *)
    ()

  let rec is_valid_path path nodename prog machines =
    let nodescopes = compute_scopes prog nodename in
    let m = get_machine nodename machines in
    match path with
    | [] -> assert false
    | [vid] -> let res = List.exists (fun v -> v.var_id = vid) (m.mmemory @ m.mstep.step_inputs @ m.mstep.step_locals) in
	       (* if not res then  *)
	       (* 	 Format.eprintf "Variable %s cannot be found in machine %s@.Local vars are %a@." vid m.mname.node_id *)
	       (* 	   (Utils.fprintf_list ~sep:", " Printers.pp_var) (m.mmemory @ m.mstep.step_inputs @ m.mstep.step_locals) *)
	       (* ; *)
	       res
	       
    | inst::nodename::path' -> (* We use the scopes computed on the prog artifact *)
      (* Format.eprintf "Path is %a@ Local scopes: @[<v>%a@ @]@."  *)
      (* 	(Utils.fprintf_list ~sep:"." Format.pp_print_string) path *)
      (* 	(Utils.fprintf_list ~sep:";@ " *)
      (* 	   (fun fmt scope ->  *)
      (* 	     Utils.fprintf_list ~sep:"." Format.pp_print_string fmt (scope_to_sl scope)) *)
      (* 	)  *)
      (* 	nodescopes; *)
      if List.mem path (List.map scope_to_sl nodescopes) then (
	(* Format.eprintf "Valid local path, checking underneath@."; *)
	is_valid_path path' nodename prog machines
      )
      else
	false

      (* let instok = List.exists (fun (inst', node) -> inst' = inst) m.minstances in *)
      (* if not instok then Format.eprintf "inst = %s@." inst; *)
      (* instok &&  *)
      (* let instnode = fst (snd (List.find (fun (inst', node) -> inst' = inst) m.minstances)) in *)
      (* is_valid_path path' (Corelang.node_of_top instnode).node_id prog machines *)

  let process_scopes main_node prog machines =
    let all_scopes = compute_scopes prog !Options.main_node in
    let selected_scopes = if !option_all_scopes then
	List.map (fun s -> scope_to_sl s) all_scopes
      else
	!scopes_def
    in
    (* Making sure all scopes are defined and were not removed by various
       optmizationq *)
    let selected_scopes = 
      List.filter 
	(fun sl -> 
	  let res = is_valid_path sl main_node prog machines in
	  if not res then
	    Format.eprintf "Scope %a is cancelled due to variable removal@." (Utils.fprintf_list ~sep:"." Format.pp_print_string) sl; 
	  res
	) 
	selected_scopes 
    in
    scopes_map := check_scopes main_node prog machines all_scopes selected_scopes;
    (* Each machine is updated with fresh memories and declared as stateful  *)
    let machines = List.map update_machine machines in
     machines

  (* let pp fmt = pp_scopes fmt !scopes_map *)

  let check_force_stateful () = is_active()

  let refine_machine_code prog machine_code =
    if show_scopes () then
      begin
	let all_scopes = compute_scopes prog !Options.main_node in
      (* Printing scopes *)
      if !Options.verbose_level >= 1 then
	Format.printf "Possible scopes are:@   ";
	Format.printf "@[<v>%a@ @]@.@?" print_scopes all_scopes;
	exit 0
      end;
    if is_active () then
      process_scopes !Options.main_node prog machine_code
    else
      machine_code
	


  let c_backend_main_loop_body_suffix fmt () =
    if is_active () then
      begin
	Format.fprintf fmt "@ %a" pp_scopes !scopes_map 
      end  

  let c_backend_main_loop_body_prefix basename mname fmt () =
    if is_active () then
      begin
	Format.fprintf fmt "@ %a" (pp_scopes_files basename mname) !scopes_map 
      end  


end
    
(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
