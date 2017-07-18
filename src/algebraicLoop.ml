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
open LustreSpec
open Corelang
open Utils
open Format

(* An single algebraic loop is defined (partition, node calls, inlined, status)
   ie 

   1. the list of variables in the loop, ident list
   
   2.the possible functions identifier to inline, and whether they have been
   inlined yet (ident * tag * bool) list and 

   3. a status whether the inlining is enough bool
*)

type call = ident * expr * eq (* fun id, expression, and containing equation *)
  
type algebraic_loop = ident list * (call * bool) list * bool
  
exception Error of (node_desc * (algebraic_loop list)) list

(* Module that extract from the DataCycle the set of node that could be inlined
   to solve the problem. *)
module CycleResolution =
struct

  (* We iter over calls in node defs. If the call defined on of the variable in
     the cycle, we store it for future possible inline. *)
  let resolve node partition : call list =
    let partition = ISet.of_list partition in
    (* Preprocessing calls: associate to each of them the eq.lhs associated *)
    let eqs = get_node_eqs node in
    let calls_expr = Causality.NodeDep.get_calls (fun _ -> true) eqs in
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
  
let al_is_solved (_, (vars, calls, status)) = status
  
(**********************************************************************)
(* Functions to access or toggle the local inlining feature of a call *)
(* expression                                                         *)
(**********************************************************************)

let inline_annotation loc =
  Inliner.keyword,
  Corelang.mkeexpr loc
    (Corelang.mkexpr loc
       (Expr_const (Const_tag tag_true) ))

let is_expr_inlined expr =
  match expr.expr_annot with
    None -> false
  | Some anns -> if List.mem_assoc Inliner.keyword anns.annots then
      let status = List.assoc Inliner.keyword anns.annots in
      match status.eexpr_qfexpr.expr_desc with
      | Expr_const (Const_tag tag) when tag = tag_true ->
	 true
      | Expr_const (Const_tag tag) when tag = tag_false ->
	 false
      | _ -> assert false (* expecting true or false value *)	 
    else false
      
(* Inline the provided expression *)
let inline_expr expr =
  let ann = inline_annotation expr.expr_loc in
    match expr.expr_annot with
    | None -> expr.expr_annot = Some {annots = [ann]; annot_loc = expr.expr_loc} 
    | Some anns -> expr.expr_annot = Some {anns with annots = ann::anns.annots}
  
(* Inline the call tagged tag in expr: should not be used anymore, thanks to a direct call to inline_expr *)
let rec inline_call tag expr =
  if expr.expr_tag = tag then
    inline_expr expr
  else
    let _in = inline_call tag in
    let _ins = List.exists _in
    in
    match expr.expr_desc with
    | Expr_const _
    | Expr_ident _ -> false
    | Expr_tuple el -> _ins el
    | Expr_ite (g,t,e) -> _ins [g;t;e]
    | Expr_arrow (e1, e2) 
    | Expr_fby (e1, e2) -> _ins [e1; e2]
    | Expr_array el -> _ins el
    | Expr_access (e, _)
    | Expr_power (e, _)
    | Expr_pre e
    | Expr_when (e, _, _) -> _in e
    | Expr_merge (_, hl) -> _ins (List.map snd hl)
    | Expr_appl (_, arg, Some r) -> _ins [arg; r]
    | Expr_appl (_, arg, None) -> _in arg


(**********************)
(* Returns a modified node, if possible, and an algebraic_loop report *)
let rec solving_node nd existing_al partitions =
  (* For each partition, we identify the original one *)
  let rerun, al = List.fold_left (fun (rerun, al) part ->
    let part_vars = ISet.of_list part in 
    let match_al (vars, calls, status) =
      not (ISet.is_empty (ISet.inter (ISet.of_list vars) part_vars))  in
    let matched, non_matched = List.partition match_al existing_al in
    match matched, non_matched with
    | [], _ -> ((* A new conflict partition *)
      let calls = CycleResolution.resolve nd part in
      (* Filter out already inlined calls *)
      let already_inlined, possible_resolution =
	List.partition (fun (_, expr, _) -> is_expr_inlined expr) calls in
      (* Inlining the first uninlined call *)
      match possible_resolution with
      | (eq, expr, fun_id)::_ -> ((* One could inline expr *)
	(* Forcing the expr to be inlined *)
	let _ = inline_expr expr in
	true, (* we have to rerun to see if the inlined expression solves the issue *)
	(
	  part,
	  List.map (fun (eq, expr2, fcn_name) ->  fcn_name, expr2, (expr2.expr_tag = expr.expr_tag)) calls,
	  false (* Status is nok, LA is unsolved yet *)
	    
	)::non_matched
      )	 
      | [] -> (* No more calls to inline: algebraic loop is not solvable *)
	 rerun, (* we don't force rerun since the current node is not solvable *)
	(
	  part, (* initial list of troublesome variables *)
	  List.map (fun (eq, expr, fcn_name) ->  fcn_name, expr, false) calls,
	  false (* Status is nok, LA is unsolved *)
	)::non_matched 
	  
    )
    | [vars, calls, status], _ ->
       assert false (* Existing partitions were already addressed:
		       on peut regarder si la partition courante concerne xxx
		       else (* New cycle ??? *)
		       
		       


		       Que faut il faire?

		       dans un premiertemps on peut ramasser les contraintes et s'arreter là.
		       pas d'appel recursif donc on a toujours des nouvelles partitions
		       on calcule les apples, on les associe à faux (on a pas inliné encore)
		       et le statue global est à faux (on a rien resolu)

		       comment resoud-on ?
		       on ne doit se focaliser que sur les partitions courantes. C'est elle qui sont en conflit
		       pour chaque, on pick un call et on l'inlinee
		       apres on appel stage1/stage2 pour faire le scheudling du node


		    *)

	 
  ) (false, existing_al) partitions
  in
  (* if partition an already identified al ? *)
  (* if ISet.of_list partition *)

  (* if rerun then *)
  (*   assert false (\* the missing part, redo stage 1, ... with the updated inline flags *\) *)
  (* ; *)
  (* TODO xxx il faut ici ajouter le inline sur le 1er appel,
     puis reafaire le stage1 et scheduling du noeud *)
  Some(nd, al)

(** This function takes a prog and returns (prog', status, alarms)
    where prog' is a modified prog with some locally inlined calls
    status is true is the final prog' is schedulable, ie no algebraic loop
    In case of failure, ie. inlining does not solve the problem; the status is false.
    Alarms contain the list of inlining performed or advised for each node. 
    This could be provided as a feedback to the user.
*)
let clean_al prog =
(* We iterate over each node *)
  let prog, al_list =
    List.fold_right (
      fun top (prog_accu, al_list) ->
	match top.top_decl_desc with
	| Node nd -> (
	  let error =
	    try (* without exception the node is schedulable; nothing to declare *)
	      let _ = Scheduling.schedule_node nd in
	      None
	    with Causality.Error (Causality.DataCycle partitions) -> solving_node nd [] partitions
	  in
	  match error with
	  | None -> top::prog_accu, al_list (* keep it as is *)
	  | Some (nd, al) ->
	     (* returning the updated node, possible solved, as well as the
		generated alarms *)
	     {top with top_decl_desc = Node nd}::prog_accu, (nd, al)::al_list 
	)	   
	| _ -> top::prog_accu, al_list
    ) prog ([], []) 
  in
  prog, List.for_all al_is_solved al_list, al_list


(* (ident list * (ident * expr* bool) list * bool) *)
let pp_al fmt (partition, calls, status) =
  fprintf fmt "@[<v 0>";
  fprintf fmt "variables in the alg. loop: @[<hov 0>%a@]@ "
    (fprintf_list ~sep:",@ " pp_print_string) partition;
  fprintf fmt "@ involved node calls: @[<v 0>%a@]@ "
    (fprintf_list ~sep:",@ "
       (fun fmt (funid, expr, status) ->
	 fprintf fmt "%s" funid;
	 if status && is_expr_inlined expr then fprintf fmt " (inlining it solves the alg. loop)";
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
  fprintf fmt "@.Algebraic loops in nodes@.";
  fprintf_list ~sep:"@."
    (fun fmt (nd, als) ->
      let top = Corelang.node_from_name (nd.node_id) in
      Error.pp_error top.top_decl_loc
	(fun fmt -> 
	  fprintf fmt "node %s: {@[<v 0>%a@]}"
	    nd.node_id
	    (fprintf_list ~sep:"@ " pp_al) als
	)
    ) fmt report;
  fprintf fmt "@."
    

let analyze prog =
  let prog, status_ok, report = clean_al prog in
  if !Options.solve_al && status_ok then
    Scheduling.schedule_prog prog
  else (
    (* TODO create a report *)
    Format.eprintf "%a" pp_report report;
    raise (Corelang.Error (Location.dummy_loc, Error.AlgebraicLoop))
  )
