(* We try to solve all algebraic loops (AL) from prog:
each node is mapped to its own cycles
each cycle is tentatively solved by inlining one of its component

When done, a report is generated.

- for each initial AL, the cycle is presented
   - either it is solvable and we provide the set of inlines that solves it
   - or it is not and we write the AL as unsolvable by inlining

   If the option solve_al is activated, the resulting partially inlined prog is
   propagated fur future processing Otherwise the compilation stops
*)
open Lustre_types
open Corelang
open Utils

(* An single algebraic loop is defined (partition, node calls, inlined, status)
   ie 

   1. the list of variables in the loop, ident list
   
   2.the possible functions identifier to inline, and whether they have been
   inlined yet (ident * tag * bool) list and 

   3. a status whether the inlining is enough bool
*)

type call = ident * expr * eq (* fun id, expression, and containing equation *)
  
type algebraic_loop = ident list * (call * bool) list * bool
type report = (node_desc * algebraic_loop list) list
exception Error of report


(* Module that extract from the DataCycle the set of node that could be inlined
   to solve the problem. *)
module CycleResolution =
struct

  (* We iter over calls in node defs. If the call defined on of the variable in
     the cycle, we store it for future possible inline. *)
  let resolve node partition : call list =
    let partition = ISet.of_list partition in
    (* Preprocessing calls: associate to each of them the eq.lhs associated *)
    let calls_expr = Causality.NodeDep.get_calls (fun _ -> true) node in
    let eqs, auts = get_node_eqs node in
    assert (auts = []); (* TODO voir si on peut acceder directement aux eqs qui font les calls *)
    let calls = List.map (
      fun expr ->
	let eq = List.find (fun eq ->
	  Corelang.expr_contains_expr expr.expr_tag eq.eq_rhs 
	) eqs in
	let fun_name = match expr.expr_desc with
	  | Expr_appl(fun_id, _, _) -> fun_id
	  | _ -> assert false
	in
	fun_name, expr, eq
    ) calls_expr in
    List.fold_left (
      fun accu ((_, _, eq) as call) ->
	let shared_vars = ISet.inter (ISet.of_list eq.eq_lhs) partition in
	if not (ISet.is_empty shared_vars) then
	  (* We have a match: keep the eq and the expr to inline *)
	  call::accu
	else
	  accu
    ) [] calls
end


(* Format.fprintf fmt "@[<v 2>Possible resolution:@ %a@]" pp_resolution resolution*)

    
let pp_resolution fmt resolution =
  fprintf_list ~sep:"@ " (fun fmt (eq, tag) ->
    Format.fprintf fmt "inlining: %a" Printers.pp_node_eq eq
  ) fmt resolution
  
let al_is_solved (_, als) = List.for_all (fun (vars, calls, status) -> status) als
  
(**********************************************************************)
(* Functions to access or toggle the local inlining feature of a call *)
(* expression                                                         *)
(**********************************************************************)

let inline_annotation loc =
  Inliner.keyword,
  Corelang.mkeexpr loc
    (Corelang.mkexpr loc
       (Expr_const (Const_tag tag_true) ))

let is_inlining_annot (key, status) =
key = Inliner.keyword && (
  match status.eexpr_qfexpr.expr_desc with
  | Expr_const (Const_tag tag) when tag = tag_true ->
     true
  | Expr_const (Const_tag tag) when tag = tag_false ->
     false
  | _ -> assert false (* expecting true or false value *)	 
)
  
let is_expr_inlined nd expr =
  match expr.expr_annot with
    None -> false
  | Some anns -> (
     (* Sanity check: expr should have the annotation AND the annotation should be declared *)
     let local_ann = List.exists is_inlining_annot anns.annots in
     let all_expr_inlined = Hashtbl.find_all Annotations.expr_annotations Inliner.keyword in
     let registered =
       List.exists
	 (fun (nd_id, expr_tag) -> nd_id = nd.node_id && expr_tag = expr.expr_tag)
	 all_expr_inlined
     in
     match local_ann, registered with
     | true, true -> true (* Everythin' all righ' ! *)
     | false, false -> false (* idem *)
     | _ -> assert false 
  )

let pp_calls nd fmt calls = Format.fprintf fmt "@[<v 0>%a@]"
  (fprintf_list ~sep:"@ " (fun fmt (funid,expr, _) -> Format.fprintf fmt "%s: %i (inlined:%b)"
    funid
    expr.expr_tag
    (is_expr_inlined nd expr)
   ))
  calls

(* Inline the provided expression *)
let inline_expr node expr =
  (* Format.eprintf "inlining %a@ @?" Printers.pp_expr expr; *)
  let ann = inline_annotation expr.expr_loc in
  let ann = {annots = [ann]; annot_loc = expr.expr_loc} in
  let res = update_expr_annot node.node_id expr ann in
  (* assert (is_expr_inlined node res); *)
  (* Format.eprintf "Is expression inlined? %b@." (is_expr_inlined node res); *)
  res

(* Perform the steps of stage1/stage2 to revalidate the schedulability of the program *)
let fast_stages_processing prog =
  Log.report ~level:3
    (fun fmt -> Format.fprintf fmt "@[<v 2>Fast revalidation: normalization + schedulability@ ");
  Options.verbose_level := !Options.verbose_level - 2;

  (* Mini stage 1 *)
  (* Extracting dependencies: fill some table with typing info *)
  ignore (Compiler_common.import_dependencies prog);
  (* Local inlining *)
  let prog = Inliner.local_inline prog (* type_env clock_env *) in
  (* Checking stateless/stateful status *)
  if Plugins.check_force_stateful () then
    Compiler_common.force_stateful_decls prog
  else
    Compiler_common.check_stateless_decls prog;
  (* Typing *)
  let _ (*computed_types_env*) = Compiler_common.type_decls !Global.type_env prog in
  (* Clock calculus *)
  let _ (*computed_clocks_env*) = Compiler_common.clock_decls !Global.clock_env prog in
  (* Normalization *)
  let prog = Normalization.normalize_prog ~backend:!Options.output prog in
  (* Mini stage 2 : Scheduling *)
  let res = Scheduling.schedule_prog prog in
  Options.verbose_level := !Options.verbose_level + 2;

  Log.report ~level:3
    (fun fmt -> Format.fprintf fmt "@]@ ");
  res

(**********************)
(* Returns a modified node, if possible, and an algebraic_loop report *)
let rec solving_node max_inlines prog nd existing_al partitions =
  (* let pp_calls = pp_calls nd in *)
  (* For each partition, we identify the original one *)
  let rerun, max_inlines, al = List.fold_left (fun (rerun, inlines, al) part ->
    let part_vars = ISet.of_list part in 
    (* Useful functions to filter list of elements *)
    let match_al (vars, calls, status) =
      not (ISet.is_empty (ISet.inter (ISet.of_list vars) part_vars)) in
    (* Identifying previous alarms that could be associated to current conflict *)
    let matched, non_matched = List.partition match_al existing_al in
    let previous_calls =
      match matched with
      | [] -> []
      | [_ (*vars*), calls, _ (*status*)] -> List.map fst calls (* we just keep the calls *)
      | _ -> (* variable should not belong to two different algrebraic loops. At least I
		hope so! *)
	 assert false
    in
    let match_previous (eq, expr, fun_id) =
      List.exists
	(fun (_, expr', _) -> expr'.expr_tag = expr.expr_tag)
	previous_calls
    in
    (* let match_inlined (_, expr, _) = is_expr_inlined nd expr in *)
    
    (* let previous_inlined, previous_no_inlined = List.partition match_inlined previous_calls in *)
    (* Format.eprintf "Previous calls: @[<v 0>inlined: {%a}@ no inlined: {%a}@ @]@ " *)
    (*   pp_calls previous_inlined *)
    (*   pp_calls previous_no_inlined *)

    (* ; *)
    
    let current_calls = CycleResolution.resolve nd part in
    (* Format.eprintf "Current calls: %a" pp_calls current_calls; *)
    (* Filter out calls from current_calls that were not already in previous calls *)
    let current_calls = List.filter (fun c -> not (match_previous c)) current_calls
    in
    (* Format.eprintf "Current new calls (no old ones): %a" pp_calls current_calls; *)
    let calls = previous_calls @ current_calls in
    (* Format.eprintf "All calls (previous + new): %a" pp_calls calls; *)
    
    (* Filter out already inlined calls: actually they should not appear
       ... since they were inlined. We keep it for safety. *)
    let _ (* already_inlined *), possible_resolution =
      List.partition (fun (_, expr, _) -> is_expr_inlined nd expr) calls in
    (* Inlining the first uninlined call *)
    match possible_resolution with
    | (fun_id, expr, eq)::_ -> ((* One could inline expr *)
      Log.report ~level:2 (fun fmt-> Format.fprintf fmt "inlining call to %s@ " fun_id); 
      (* Forcing the expr to be inlined *)
      let _ = inline_expr nd expr in
      (* Format.eprintf "Making sure that the inline list evolved: inlined = {%a}" *)
      (* 	pp_calls  *)
      (* ; *)
      true, (* we have to rerun to see if the inlined expression solves the issue *)
      max_inlines - 1,
      (
	part,
	List.map (fun ((eq, expr2, fcn_name) as call)->  call, (expr2.expr_tag = expr.expr_tag)) calls,
	true (* TODO was false. Should be put it true and expect a final
		scheduling to change it to false in case of failure ? *) (*
									   Status is nok, LA is unsolved yet *)
	  
      )::non_matched
    )	 
    | [] -> (* No more calls to inline: algebraic loop is not solvable *)
       rerun, (* we don't force rerun since the current node is not solvable *)
      max_inlines,
      (
	part, (* initial list of troublesogme variables *)
	List.map (fun ((eq, expr, fcn_name) as call) ->  call, false) calls,
	false (* Status is nok, LA is unsolved *)
      )::non_matched 
	
  ) (false, max_inlines, existing_al) partitions
  in
  (* if partition an already identified al ? *)
  (* if ISet.of_list partition *)
  if rerun && max_inlines > 0 then
    (* At least one partition could be improved: we try to inline the calls and reschedule the node. *)
    try
      Log.report ~level:2 (fun fmt -> Format.fprintf fmt "rescheduling node with new inlined call@ ");
      let _ = fast_stages_processing prog in
      (* If everything went up to here, the problem is solved! All associated
	 alarms are mapped to valid status. *)
      let al = List.map (fun (v,c,_) -> v,c,true) al in
      Log.report ~level:2 (fun fmt -> Format.fprintf fmt "AL solved@ ");
      Some(nd, al), max_inlines
    with Causality.Error (Causality.DataCycle partitions2) -> (
      Log.report ~level:3 (fun fmt -> Format.fprintf fmt "AL not solved yet. Further processing.@ ");
      solving_node max_inlines prog nd al partitions2
    )
  else ((* No rerun, we return the current node and computed alarms *)
    Log.report ~level:3 (fun fmt -> Format.fprintf fmt "AL not solved yet. Stopping.@ ");
    Some(nd, al), max_inlines
  )
      
(** This function takes a prog and returns (prog', status, alarms)
    where prog' is a modified prog with some locally inlined calls
    status is true is the final prog' is schedulable, ie no algebraic loop
    In case of failure, ie. inlining does not solve the problem; the status is false.
    Alarms contain the list of inlining performed or advised for each node. 
    This could be provided as a feedback to the user.
*)
let clean_al prog : program * bool * report =
  let max_inlines = !Options.al_nb_max in
(* We iterate over each node *)
  let _, prog, al_list =
    List.fold_right (
      fun top (max_inlines, prog_accu, al_list) ->
	match top.top_decl_desc with
	| Node nd -> (
	  let error, max_inlines =
	    try (* without exception the node is schedulable; nothing to declare *)
	      let _ = Scheduling.schedule_node nd in
	      None, max_inlines
	    with Causality.Error (Causality.DataCycle partitions) -> (
	      Log.report ~level:2 (fun fmt -> Format.fprintf fmt "@[<v 2>AL in node %s@ " nd.node_id);
	      let error, max_inlines = solving_node max_inlines prog nd [] partitions in
	      Log.report ~level:2 (fun fmt -> Format.fprintf fmt "@ @]");
	      error, max_inlines
	    )
	  in
	  match error with
	  | None -> max_inlines, top::prog_accu, al_list (* keep it as is *)
	  | Some (nd, al) ->
	     (* returning the updated node, possible solved, as well as the
		generated alarms *)
	     max_inlines,
	     {top with top_decl_desc = Node nd}::prog_accu,
	    (nd, al)::al_list 
	)	   
	| _ -> max_inlines, top::prog_accu, al_list
    ) prog (max_inlines, [], []) 
  in
  prog, List.for_all al_is_solved al_list, al_list


(* (ident list * (ident * expr* bool) list * bool) *)
let pp_al nd fmt (partition, calls, status) =
  let open Format in
  fprintf fmt "@[<v 0>";
  fprintf fmt "variables in the alg. loop: @[<hov 0>%a@]@ "
    (fprintf_list ~sep:",@ " pp_print_string) partition;
  fprintf fmt "@ involved node calls: @[<v 0>%a@]@ "
    (fprintf_list ~sep:",@ "
       (fun fmt ((funid, expr, eq), status) ->
	 fprintf fmt "%s" funid;
	 if status && is_expr_inlined nd expr then fprintf fmt " (inlining it solves the alg. loop)";
       )
    ) calls;
  fprintf fmt "@]"
     (* TODO ploc:
	First analyse the cycle and identify a list of nodes to be inlined, or node instances
	Then two behaviors: 
	- print that list instead of the unreadable cyclic dependency comment
	- modify the node by adding the inline information
	- recall the subset of stage1 but restricted to a single node:
	- inline locally, typing, clocking (may have to reset the tables first), normalization of the node, mpfr injection
        - recall stage2
     *)
    
    
let pp_report fmt report =
  let open Format in
  fprintf_list ~sep:"@."
    (fun fmt (nd, als) ->
      let top = Corelang.node_from_name (nd.node_id) in
      let pp =
	if not !Options.solve_al || List.exists (fun (_,_,valid) -> not valid) als then
	  Error.pp_error (* at least one failure: erroneous node *)
	else
	  Error.pp_warning (* solvable cases: warning only *)
      in
      pp top.top_decl_loc
	(fun fmt -> 
	  fprintf fmt "algebraic loop in node %s: {@[<v 0>%a@]}"
	    nd.node_id
	    (fprintf_list ~sep:"@ " (pp_al nd)) als
	)
    ) fmt report;
  fprintf fmt "@."
    


let analyze cpt prog =
  Log.report ~level:1 (fun fmt ->
    Format.fprintf fmt "@[<v 2>Algebraic loop detected: ";
    if !Options.solve_al then Format.fprintf fmt "solving mode actived";
    Format.fprintf fmt "@ ";    
  );
  let prog, status_ok, report = clean_al prog in
  
  let res =
    if cpt > 0 && !Options.solve_al && status_ok then (
      try
	fast_stages_processing prog
      with _ -> assert false (* Should not happen since the error has been
				catched already *)
    )
    else (
      (* We stop with unresolved AL *)(* TODO create a report *)
      (* Printing the report on stderr *)
      Format.eprintf "%a" pp_report report;
      raise (Corelang.Error (Location.dummy_loc, Error.AlgebraicLoop))
    )
  in
  (* Printing the report on stderr *)
  Format.eprintf "%a" pp_report report;
  res
    
let analyze prog =
  analyze !Options.al_nb_max prog
    
    
