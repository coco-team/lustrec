
(* We try to avoid opening modules here *)
module ST = Salsa.SalsaTypes
module SDT = SalsaDatatypes
module LT = LustreSpec
module MC = Machine_code

(* Datatype for Salsa: FormalEnv, Ranges, Var set ... *)
open SalsaDatatypes
(******************************************************************)
(* TODO Xavier: should those functions be declared more globally? *)

let fun_types node = 
  try
    match node.LT.top_decl_desc with 
    | LT.Node nd -> 
      let tin, tout = Types.split_arrow nd.LT.node_type in
      Types.type_list_of_type tin, Types.type_list_of_type tout
    | _ -> Format.eprintf "%a is not a node@.@?" Printers.pp_decl node; assert false
  with Not_found -> Format.eprintf "Unable to find type def for function %s@.@?" (Corelang.node_name node); assert false

let called_node_id m id = 
  let td, _ =
    try
      List.assoc id m.MC.mcalls (* TODO Xavier: mcalls or minstances ? *)
    with Not_found -> assert false
  in
  td
(******************************************************************)    

(* Returns the set of vars that appear in the expression *)
let rec get_expr_real_vars e = 
  match e.LT.value_desc with
  | LT.LocalVar v | LT.StateVar v when Types.is_real_type v.LT.var_type -> Vars.singleton v
  | LT.LocalVar _| LT.StateVar _
  | LT.Cst _ -> Vars.empty 
  | LT.Fun (_, args) -> 
    List.fold_left 
      (fun acc e -> Vars.union acc (get_expr_real_vars e)) 
      Vars.empty args
  | LT.Array _
  | LT.Access _
  | LT.Power _ -> assert false 

(* Extract the variables to appear as free variables in expressions (lhs) *)
let rec get_read_vars instrs =
  match instrs with
    [] -> Vars.empty
  | i::tl -> (
    let vars_tl = get_read_vars tl in 
    match Corelang.get_instr_desc i with
    | LT.MLocalAssign(_,e) 
    | LT.MStateAssign(_,e) -> Vars.union (get_expr_real_vars e) vars_tl
    | LT.MStep(_, _, el) -> List.fold_left (fun accu e -> Vars.union (get_expr_real_vars e) accu) vars_tl el
    | LT.MBranch(e, branches) -> (
      let vars = Vars.union (get_expr_real_vars e) vars_tl in
      List.fold_left (fun vars (_, b) -> Vars.union vars (get_read_vars b) ) vars branches
    )
    | LT.MReset _ 
    | LT.MNoReset _ 
    | LT.MComment _ -> Vars.empty  
  )

let rec get_written_vars instrs =
  match instrs with
    [] -> Vars.empty
  | i::tl -> (
    let vars_tl = get_written_vars tl in 
    match Corelang.get_instr_desc i with
    | LT.MLocalAssign(v,_) 
    | LT.MStateAssign(v,_) -> Vars.add v vars_tl 
    | LT.MStep(vdl, _, _) -> List.fold_left (fun accu v -> Vars.add v accu) vars_tl vdl
    | LT.MBranch(_, branches) -> (
      List.fold_left (fun vars (_, b) -> Vars.union vars (get_written_vars b) ) vars_tl branches
    )
    | LT.MReset _ 
    | LT.MNoReset _ 
    | LT.MComment _ -> Vars.empty    
  )


(* Optimize a given expression. It returns another expression and a computed range. *)
let optimize_expr nodename constEnv printed_vars vars_env ranges formalEnv e : LT.value_t * RangesInt.t option = 
  let rec opt_expr ranges formalEnv e =
    match e.LT.value_desc with
    | LT.Cst cst ->
      Format.eprintf "optmizing constant expr ? @ ";
      (* the expression is a constant, we optimize it directly if it is a real
  	 constant *)
      let typ = Typing.type_const Location.dummy_loc cst in
      if Types.is_real_type typ then 
	opt_num_expr ranges formalEnv e 
      else e, None
    | LT.LocalVar v
    | LT.StateVar v -> 
      if not (Vars.mem v printed_vars) && 
	(* TODO xAvier: comment recuperer le type de l'expression? Parfois e.value_type vaut 'd *)
	(Types.is_real_type e.LT.value_type ||  Types.is_real_type v.LT.var_type) 
      then
	opt_num_expr ranges formalEnv e 
      else 
	e, None  (* Nothing to optimize for expressions containing a single non real variable *)
    (* (\* optimize only numerical vars *\) *)
    (* if Type_predef.is_real_type v.LT.var_type then opt_num_expr ranges formalEnv e *)
    (* else e, None *)
    | LT.Fun (fun_id, args) -> (
      (* necessarily, this is a basic function (ie. + - * / && || mod ... ) *)
      (* if the return type is real then optimize it, otherwise call recusrsively on arguments *)
      if Types.is_real_type e.LT.value_type then
	opt_num_expr ranges formalEnv e 
      else (
	(* We do not care for computed local ranges. *)
  	let args' = List.map (fun arg -> let arg', _ = opt_expr ranges formalEnv arg in arg') args in
  	{ e with LT.value_desc = LT.Fun(fun_id, args')}, None	  
      )
    )
    | LT.Array _
    | LT.Access _
    | LT.Power _ -> assert false  
  and opt_num_expr ranges formalEnv e = 
    if debug then Format.eprintf "Optimizing expression %a@ " MC.pp_val e; 
    let fresh_id = "toto"  in (* TODO more meaningful name *)
    (* Convert expression *)
    List.iter (fun (l,c) -> Format.eprintf "%s -> %a@ " l Printers.pp_const c) constEnv;
    let e_salsa : Salsa.SalsaTypes.expression = value_t2salsa_expr constEnv e in
    Format.eprintf "apres deplaige constantes ok%a @." MC.pp_val (salsa_expr2value_t vars_env [](* constEnv *) e_salsa) ; 

    (* Convert formalEnv *)
    if debug then Format.eprintf "Formal env is [%a]@ " FormalEnv.pp formalEnv;
    let formalEnv_salsa = 
      FormalEnv.fold (fun id expr accu ->
	(id, value_t2salsa_expr constEnv expr)::accu
		     ) formalEnv []  in
    if debug then Format.eprintf "Formal env converted to salsa@ ";
    (* Substitute all occurences of variables by their definition in env *)
    let (e_salsa: Salsa.SalsaTypes.expression), _ = 
      Salsa.Rewrite.substVars 
	e_salsa
	formalEnv_salsa
	0 (* TODO: Nasrine, what is this integer value for ? *)
    in
    if debug then Format.eprintf "Substituted def in expr@ ";
    let abstractEnv = Hashtbl.fold 
      (fun id value accu -> (id,value)::accu) 
      ranges
      [] 
    in
    (* List.iter (fun (id, _) -> Format.eprintf "absenv: %s@." id) abstractEnv; *)
    (* The expression is partially evaluated by the available ranges
       valEnv2ExprEnv remplce les paires id, abstractVal par id, Cst itv - on
       garde evalPartExpr remplace les variables e qui sont dans env par la cst
       - on garde *)
    if debug then Format.eprintf "avant avant eval part@ ";
     Format.eprintf "avant evalpart: %a@." MC.pp_val (salsa_expr2value_t vars_env constEnv e_salsa); 
    let e_salsa =  
      Salsa.Float.evalPartExpr 
	e_salsa
	(Salsa.Float.valEnv2ExprEnv abstractEnv) 
	([] (* no blacklisted variables *))  
    in
     Format.eprintf "apres evalpart: %a@." MC.pp_val (salsa_expr2value_t vars_env constEnv e_salsa); 
    (* Checking if we have all necessary information *)

    let free_vars = get_salsa_free_vars vars_env constEnv abstractEnv  e_salsa in

    if Vars.cardinal free_vars > 0 then (
      Format.eprintf "Warning: unbounded free vars (%a) in expression %a. We do not optimize it.@ " 
	Vars.pp (Vars.fold (fun v accu -> let v' = {v with LT.var_id = nodename ^ "." ^ v.LT.var_id } in Vars.add v' accu) free_vars Vars.empty)
	MC.pp_val (salsa_expr2value_t vars_env constEnv e_salsa);
      if debug then Format.eprintf "Some free vars, not optimizing@.";
      let new_e = try salsa_expr2value_t vars_env constEnv e_salsa   with Not_found -> assert false in
      new_e, None
    )
    else (
      try
	if debug then
	  Format.eprintf "Analyzing expression %a with env: @[<v>%a@ @]@ "
	    MC.pp_val (salsa_expr2value_t vars_env constEnv e_salsa)
	    (Utils.fprintf_list ~sep:",@ "(fun fmt (l,r) -> Format.fprintf fmt "%s -> %a" l FloatIntSalsa.pp r)) abstractEnv
	;
	  
	let new_e_salsa, e_val = 
	  Salsa.MainEPEG.transformExpression fresh_id e_salsa abstractEnv 
	in
    	let new_e = try salsa_expr2value_t vars_env constEnv new_e_salsa   with Not_found -> assert false in
	if debug then Format.eprintf "@  @[<v>old: %a@ new: %a@ range: %a@]" MC.pp_val e MC.pp_val new_e RangesInt.pp_val e_val;
	new_e, Some e_val
      with Not_found -> assert false
      | Salsa.Epeg_types.EPEGError _ -> (
	Format.eprintf "BECAUSE OF AN ERROR, Expression %a was not optimized@ " MC.pp_val e;
	e, None
      )
    )



  in
  if debug then 
    Format.eprintf "@[<v 2>Optimizing expression %a in environment %a and ranges %a@ "
      MC.pp_val e
      FormalEnv.pp formalEnv
      RangesInt.pp ranges;
  let res = opt_expr ranges formalEnv e in
  Format.eprintf "@]@ ";
  res

    
    
(* Returns a list of assign, for each var in vars_to_print, that produce the
   definition of it according to formalEnv, and driven by the ranges. *)
let assign_vars nodename constEnv vars_env printed_vars ranges formalEnv vars_to_print =
  (* We print thhe expression in the order of definition *)

  let ordered_vars = 
    List.stable_sort
      (FormalEnv.get_sort_fun formalEnv) 
      (Vars.elements vars_to_print) 
  in
  Format.eprintf "Printing vars in the following order: [%a]@ " (Utils.fprintf_list ~sep:", " Printers.pp_var) ordered_vars ;
  List.fold_right (
    fun v (accu_instr, accu_ranges) -> 
      if debug then Format.eprintf "Printing assign for variable %s@ " v.LT.var_id;
      try
	(* Obtaining unfold expression of v in formalEnv *)
	let v_def = FormalEnv.get_def formalEnv v  in
	let e, r = optimize_expr nodename constEnv printed_vars vars_env ranges formalEnv v_def in
	let instr_desc = 
	  if try (get_var vars_env v.LT.var_id).is_local with Not_found -> assert false then
	    LT.MLocalAssign(v, e)
	  else
	    LT.MStateAssign(v, e)
	in
	(Corelang.mkinstr instr_desc)::accu_instr, 
	(match r with 
	| None -> ranges 
	| Some v_r -> RangesInt.add_def ranges v.LT.var_id v_r)
      with FormalEnv.NoDefinition _ -> (
	(* It should not happen with C backend, but may happen with Lustre backend *)
	if !Options.output = "lustre" then accu_instr, ranges else (Format.eprintf "@?"; assert false)
      )
  ) ordered_vars ([], ranges)

(* Main recursive function: modify the instructions list while preserving the
   order of assigns for state variables. Returns a quintuple: (new_instrs,
   ranges, formalEnv, printed_vars, and remaining vars to be printed) *)
let rec rewrite_instrs nodename constEnv  vars_env m instrs ranges formalEnv printed_vars vars_to_print = 
  let assign_vars = assign_vars nodename constEnv vars_env in
  if debug then (
    Format.eprintf "------------@ ";
    Format.eprintf "Current printed_vars: [%a]@ " Vars.pp printed_vars;
    Format.eprintf "Formal env is [%a]@ " FormalEnv.pp formalEnv;
  );
  match instrs with
  | [] -> 
     (* End of instruction list: we produce the definition of each variable that
       appears in vars_to_print. Each of them should be defined in formalEnv *)
     if debug then Format.eprintf "Producing definitions %a@ " Vars.pp vars_to_print;
     let instrs, ranges' = assign_vars printed_vars ranges formalEnv vars_to_print in
     instrs,
     ranges',     
     formalEnv,
     Vars.union printed_vars vars_to_print, (* We should have printed all required vars *)
     []          (* No more vars to be printed *)

  | hd_instr::tl_instrs -> 
     (* We reformulate hd_instr, producing or not a fresh instruction, updating
       formalEnv, possibly ranges and vars_to_print *)
     begin
       let hd_instrs, ranges, formalEnv, printed_vars, vars_to_print =
	 match Corelang.get_instr_desc hd_instr with 
	 | LT.MLocalAssign(vd,vt) when Types.is_real_type vd.LT.var_type  && not (Vars.mem vd vars_to_print) -> 
	    (* LocalAssign are injected into formalEnv *)
	    if debug then Format.eprintf "Registering local assign %a@ " MC.pp_instr hd_instr;
	    let formalEnv' = FormalEnv.def formalEnv vd vt in (* formelEnv updated with vd = vt *)
	    [],                        (* no instr generated *)
	    ranges,                    (* no new range computed *)
	    formalEnv',
	    printed_vars,              (* no new printed vars *)
	    vars_to_print              (* no more or less variables to print *)
	      
	 | LT.MLocalAssign(vd,vt) when Types.is_real_type vd.LT.var_type && Vars.mem vd vars_to_print ->

            if debug then Format.eprintf "Registering and producing state assign %a@ " MC.pp_instr hd_instr;
	    let formalEnv' = FormalEnv.def formalEnv vd vt in (* formelEnv updated with vd = vt *) 
	    let instrs', ranges' = (* printing vd = optimized vt *)
	      assign_vars printed_vars ranges formalEnv' (Vars.singleton vd)  
	    in
	    instrs',
	    ranges',                          (* no new range computed *)
	    formalEnv',                       (* formelEnv already updated *)
	    Vars.add vd printed_vars,        (* adding vd to new printed vars *)
	    Vars.remove vd vars_to_print     (* removed vd from variables to print *)

	 | LT.MStateAssign(vd,vt) when Types.is_real_type vd.LT.var_type && Vars.mem vd vars_to_print -> 

	    (* StateAssign are produced since they are required by the function. We still
	     keep their definition in the formalEnv in case it can optimize later
	     outputs. vd is removed from remaining vars_to_print *)
	    if debug then Format.eprintf "Registering and producing state assign %a@ " MC.pp_instr hd_instr;
	    let formalEnv' = FormalEnv.def formalEnv vd vt in (* formelEnv updated with vd = vt *) 
	    let instrs', ranges' = (* printing vd = optimized vt *)
	      assign_vars printed_vars ranges formalEnv' (Vars.singleton vd)  
	    in
	    instrs',
	    ranges',                          (* no new range computed *)
	    formalEnv,                       (* formelEnv already updated *)
	    Vars.add vd printed_vars,        (* adding vd to new printed vars *)
	    Vars.remove vd vars_to_print     (* removed vd from variables to print *)

	 | (LT.MLocalAssign(vd,vt) | LT.MStateAssign(vd,vt)) -> 
	    (* We have to produce the instruction. But we may have to produce as
	     well its dependencies *)
	    let required_vars = get_expr_real_vars vt in
	    let required_vars = Vars.diff required_vars printed_vars in (* remove
									 already
									 produced
									 variables *)
	    let prefix_instr, ranges = 
	      assign_vars printed_vars ranges formalEnv required_vars in
	    let vt', _ = optimize_expr nodename constEnv (Vars.union required_vars printed_vars) vars_env ranges formalEnv vt in
	    let new_instr = 
	      match Corelang.get_instr_desc hd_instr with
	      | LT.MLocalAssign _ -> Corelang.update_instr_desc hd_instr (LT.MLocalAssign(vd,vt'))
	      | _ -> Corelang.update_instr_desc hd_instr (LT.MStateAssign(vd,vt'))
	    in
	    let written_vars = Vars.add vd required_vars in
	    prefix_instr@[new_instr],
	    ranges,                          (* no new range computed *)
	    formalEnv,                       (* formelEnv untouched *)
	    Vars.union written_vars printed_vars,  (* adding vd + dependencies to
						    new printed vars *)
	    Vars.diff vars_to_print written_vars (* removed vd + dependencies from
						  variables to print *)

	 | LT.MStep(vdl,id,vtl) -> 
	    if debug then Format.eprintf "Call to a node %a@ " MC.pp_instr hd_instr;
	    (* Call of an external function. Input expressions have to be
	     optimized, their free variables produced. A fresh range has to be
	     computed for each output variable in vdl. Output of the function
	     call are removed from vars to be printed *)
	    let node =  called_node_id m id in
	    let node_id = Corelang.node_name node in
	    let tin, tout =  (* special care for arrow *)
	      if node_id = "_arrow" then
		match vdl with 
		| [v] -> let t = v.LT.var_type in
			 [t; t], [t]
		| _ -> assert false (* should not happen *)
	      else
		fun_types node
	    in
	    if debug then Format.eprintf "@[<v 2>... optimizing arguments@ ";
	    let vtl', vtl_ranges = List.fold_right2 (
				       fun e typ_e (exprl, range_l)-> 
				       if Types.is_real_type typ_e then
					 let e', r' = optimize_expr nodename constEnv printed_vars vars_env ranges formalEnv e in
					 e'::exprl, r'::range_l
				       else 
					 e::exprl, None::range_l
				     ) vtl tin ([], []) 
	    in 
	    if debug then Format.eprintf "... done@ @]@ ";
	    let required_vars = 
	      List.fold_left2 
		(fun accu e typ_e -> 
		 if Types.is_real_type typ_e then
		   Vars.union accu (get_expr_real_vars e) 
		 else (* we do not consider non real expressions *)
		   accu
		)
 		Vars.empty 
		vtl' tin
	    in
	    if debug then Format.eprintf "Required vars: [%a]@ Printed vars: [%a]@ Remaining required vars: [%a]@ "
					 Vars.pp required_vars 
					 Vars.pp printed_vars
					 Vars.pp (Vars.diff required_vars printed_vars)
	    ;
	      let required_vars = Vars.diff required_vars printed_vars in (* remove
									 already
									 produced
									 variables *)
	      let written_vars = Vars.union required_vars (Vars.of_list vdl) in
	      let instrs', ranges' = assign_vars (Vars.union written_vars printed_vars) ranges formalEnv required_vars in
	      instrs' @ [Corelang.update_instr_desc hd_instr (LT.MStep(vdl,id,vtl'))], (* New instrs *)
	      RangesInt.add_call ranges' vdl id vtl_ranges,   (* add information bounding each vdl var *) 
	      formalEnv,
	      Vars.union written_vars printed_vars,        (* adding vdl to new printed vars *)
	      Vars.diff vars_to_print written_vars
			
	 | LT.MBranch(vt, branches) -> 
	    (* Required variables to compute vt are introduced. 
	     Then each branch is refactored specifically 
	     *)
	    if debug then Format.eprintf "Branching %a@ " MC.pp_instr hd_instr;
	    let required_vars = get_expr_real_vars vt in
	    let required_vars = Vars.diff required_vars printed_vars in (* remove
									 already
									 produced
									 variables *)
	    let prefix_instr, ranges = 
	      assign_vars (Vars.union required_vars printed_vars) ranges formalEnv required_vars in

	    let printed_vars = Vars.union printed_vars required_vars in

	    let vt', _ = optimize_expr nodename constEnv printed_vars vars_env ranges formalEnv vt in

	    let read_vars_tl = get_read_vars tl_instrs in
	    if debug then Format.eprintf "@[<v 2>Dealing with branches@ ";
	    let branches', written_vars, merged_ranges = List.fold_right (
							     fun (b_l, b_instrs) (new_branches, written_vars, merged_ranges) -> 
							     let b_write_vars = get_written_vars b_instrs in
							     let b_vars_to_print = Vars.inter b_write_vars (Vars.union read_vars_tl vars_to_print) in 
							     let b_fe = formalEnv in               (* because of side effect
						       data, we copy it for
						       each branch *)
							     let b_instrs', b_ranges, b_formalEnv, b_printed, b_vars = 
							       rewrite_instrs nodename constEnv  vars_env m b_instrs ranges b_fe printed_vars b_vars_to_print 
							     in
							     (* b_vars should be empty *)
							     let _ = if b_vars != [] then assert false in
							     
							     (* Producing the refactored branch *)
							     (b_l, b_instrs') :: new_branches,
							     Vars.union b_printed written_vars, (* They should coincides. We
						       use union instead of
						       inter to ease the
						       bootstrap *)
							     RangesInt.merge merged_ranges b_ranges      
									     
							   ) branches ([], required_vars, ranges) in
	    if debug then Format.eprintf "dealing with branches done@ @]@ ";	  
	    prefix_instr@[Corelang.update_instr_desc hd_instr (LT.MBranch(vt', branches'))],
	    merged_ranges, (* Only step functions call within branches
			    may have produced new ranges. We merge this data by
			    computing the join per variable *)
	    formalEnv,    (* Thanks to the computation of var_to_print in each
			   branch, no new definition should have been computed
			   without being already printed *)
	    Vars.union written_vars printed_vars,
	    Vars.diff vars_to_print written_vars (* We remove vars that have been
						  produced within branches *)


	 | LT.MReset(_) | LT.MNoReset _ | LT.MComment _ ->
			   if debug then Format.eprintf "Untouched %a (non real)@ " MC.pp_instr hd_instr;

			   (* Untouched instruction *)
			   [ hd_instr ],                    (* unmodified instr *)
			   ranges,                          (* no new range computed *)
			   formalEnv,                       (* no formelEnv update *)
			   printed_vars,
			   vars_to_print                    (* no more or less variables to print *)
			     
       in
       let tl_instrs, ranges, formalEnv, printed_vars, vars_to_print = 
	 rewrite_instrs 
	   nodename
	   constEnv 	  
	   vars_env
	   m 
	   tl_instrs
	   ranges
	   formalEnv
	   printed_vars
	   vars_to_print
       in
       hd_instrs @ tl_instrs,
       ranges,
       formalEnv, 
       printed_vars,
       vars_to_print 
     end






(* TODO: deal with new variables, ie. tmp *)
let salsaStep constEnv  m s = 
  let ranges = RangesInt.empty (* empty for the moment, should be build from
				  machine annotations or externally provided information *) in
  let annots = List.fold_left (
    fun accu annl -> 
      List.fold_left (
	fun accu (key, range) ->
	  match key with 
	  | ["salsa"; "ranges"; var] -> (var, range)::accu
	  | _ -> accu
      ) accu annl.LT.annots
  ) [] m.MC.mannot
  in
  let ranges = 
    List.fold_left (fun ranges (v, value) ->
      match value.LT.eexpr_qfexpr.LT.expr_desc with 
      | LT.Expr_tuple [minv; maxv] -> (
	let get_cst e = match e.LT.expr_desc with 
	  | LT.Expr_const (LT.Const_real (c,e,s)) -> 
	    (* calculer la valeur c * 10^e *) 
	    Num.float_of_num (Num.div_num c (Num.power_num (Num.num_of_int 10) (Num.num_of_int e))) 
	  | _ -> 
	    Format.eprintf 
	      "Invalid scala range: %a. It should be a pair of constant floats.@." 
	      Printers.pp_expr value.LT.eexpr_qfexpr; 
	    assert false 
	in
	let minv, maxv = get_cst minv, get_cst maxv in
	if debug then Format.eprintf "%s in [%f, %f]@ " v minv maxv;
	RangesInt.enlarge ranges v (Salsa.SalsaTypes.I(minv, maxv),Salsa.SalsaTypes.J(0.,0.))
      )
      | _ -> 
	Format.eprintf 
	  "Invalid scala range: %a. It should be a pair of floats.@." 
	  Printers.pp_expr value.LT.eexpr_qfexpr; 
	assert false
    ) ranges annots
  in
  let formal_env = FormalEnv.empty () in
  let vars_to_print =
    Vars.real_vars  
      (
	Vars.union 
	  (Vars.of_list m.MC.mmemory) 
	  (Vars.of_list s.MC.step_outputs) 
      )
  in 
  (* TODO: should be at least step output + may be memories *)
  let vars_env = compute_vars_env m in
  let new_instrs, _, _, printed_vars, _ = 
    rewrite_instrs
      m.MC.mname.LT.node_id
      constEnv 
      vars_env
      m
      s.MC.step_instrs
      ranges
      formal_env
      (Vars.real_vars (Vars.of_list s.MC.step_inputs (* printed_vars : real
							inputs are considered as
							already printed *)))
      vars_to_print 
  in
  let all_local_vars = Vars.real_vars (Vars.of_list s.MC.step_locals) in
  let unused = (Vars.diff all_local_vars printed_vars) in
  let locals =
    if not (Vars.is_empty unused) then (
      Format.eprintf "Unused local vars: [%a]. Removing them.@.@?"
	Vars.pp unused;
      List.filter (fun v -> not (Vars.mem v unused)) s.MC.step_locals
    )
    else
      s.MC.step_locals
  in
  { s with MC.step_instrs = new_instrs; MC.step_locals = locals } (* we have also to modify local variables to declare new vars *)


let machine_t2machine_t_optimized_by_salsa constEnv  mt = 
  try
    if debug then Format.eprintf "@[<v 2>------------------ Optimizing machine %s@ " mt.MC.mname.LT.node_id;
    let new_step = salsaStep constEnv  mt mt.MC.mstep in
    if debug then Format.eprintf "@]@.";
    { mt with MC.mstep = new_step } 
    
      
  with FormalEnv.NoDefinition v as exp -> 
    Format.eprintf "No definition for variable %a@.@?" Printers.pp_var v; 
    raise exp


(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)

