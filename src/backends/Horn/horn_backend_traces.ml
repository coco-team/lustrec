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
open Corelang
open Machine_code_types

open Horn_backend_common
open Horn_backend_printers

let pp_traces = (Utils.fprintf_list ~sep:", " (fun fmt (v,e) -> Format.fprintf fmt "%s -> %a"
                                                         v
                                                         Printers.pp_expr e))

(* Compute memories associated to each machine *)
let compute_mems machines m =
  let rec aux fst prefix m =
    (List.map (fun mem -> (prefix, mem)) m.mmemory) @
      List.fold_left (fun accu (id, (n, _)) ->
	let name = node_name n in
	if name = "_arrow" then accu else
	  let machine_n = get_machine machines name in
	  ( aux false ((id,machine_n)::prefix) machine_n )
	  @ accu
      ) [] m.minstances
  in
  aux true [] m


(* We extract the annotation dealing with traceability *)
let machines_traces machines = 
  List.map (fun m ->
    let traces : (ident * expr) list=
      let all_annots = List.flatten (List.map (fun ann -> ann.annots) m.mannot) in
      let filtered =
	List.filter (fun (kwds, _) -> kwds = ["traceability"]) all_annots
      in
      (* List.iter (Format.eprintf "Annots: %a@." Printers.pp_expr_annot) (m.mannot); *)
      let content = List.map snd filtered in
      (* Elements are supposed to be a pair (tuple): variable, expression *)
      List.map (fun ee ->
	match ee.eexpr_quantifiers, ee.eexpr_qfexpr.expr_desc with
	| [], Expr_tuple [v;e] -> (
	  match v.expr_desc with
	  | Expr_ident vid -> vid, e
	  | _ -> assert false )
	| _ -> assert false)
	content
    in
    (* Format.eprintf "Build traces: %a@." pp_traces traces; *)
    m, traces

  ) machines
  
let memories_old machines m =
  List.map (fun (p, v) ->
    let machine = match p with | [] -> m | (_,m')::_ -> m' in
    let traces = List.assoc machine (machines_traces machines) in
    if List.mem_assoc v.var_id traces then 
      (
	(* We take the expression associated to variable v in the trace
	   info *)

	 (* eprintf "Found variable %a in traces: %a@."  Printers.pp_var v
	  *   Printers.pp_expr (List.assoc v.var_id traces);  *)
	p, List.assoc v.var_id traces
      )
    else 
      begin

	(* We keep the variable as is: we create an expression v *)

	 (* eprintf "Unable to found variable %a in traces (%a)@."  Printers.pp_var v
	  *   pp_traces traces;  *)

	p, mkexpr Location.dummy_loc (Expr_ident v.var_id)
      end

  ) (compute_mems machines m)
      
let memories_next  machines m = (* We remove the topest pre in each expression *)
  List.map
    (fun (prefix, ee) ->
      let m = match prefix with | [] -> m | (_,m')::_ -> m' in
      match ee.expr_desc with
      | Expr_pre e -> prefix, e
      | Expr_ident var_id -> (
        (* This memory was not introduced through
           normalization. It shall then be a regular x = pre y
           expression. Otherwise it would have been rewritten. We
           have to get its definition and extract the argument of
           the pre *)
        
        let selected_def =
          try  
            List.find
              (fun def ->
                match def with
                | Eq eq -> (match eq.eq_lhs with
                            | [v] -> v = var_id 
                           )
                | _ -> false)
              m.mname.node_stmts
          with _ -> (Format.eprintf
                       "Unable to find definition of %s in stmts %a@.prefix=%a@.@?"
                       var_id
                       Printers.pp_node_stmts m.mname.node_stmts
                       (Utils.fprintf_list ~sep:","
      	                  (fun fmt (id,n) -> fprintf fmt "(%s,%s)" id n.mname.node_id ))
      	               (List.rev prefix)
      	            
                    ;
                      assert false)
        in
        let def = match selected_def with
          | Eq eq -> (
            match eq.eq_lhs, eq.eq_rhs.expr_desc with
            | [single_var], Expr_pre e -> if single_var = var_id then e else assert false
            | _ -> assert false
          )
          | _ -> assert false
        in
        prefix, def
      )
                           
      | _ ->
         eprintf "Mem Failure: (prefix: %a, eexpr: %a)@.@?"
      	   (Utils.fprintf_list ~sep:","
      	      (fun fmt (id,n) -> fprintf fmt "(%s,%s)" id n.mname.node_id ))
      	   (List.rev prefix)
      	   Printers.pp_expr ee;
      	 assert false
    )
    (memories_old machines m)
      


let pp_prefix_rev fmt prefix =
  Utils.fprintf_list ~sep:"." 
    (fun fmt (id,n) -> fprintf fmt "(%s,%s)" id n.mname.node_id) 
    fmt 
    (List.rev prefix)
      

let traces_file fmt basename prog machines =
  fprintf fmt "<?xml version=\"1.0\"?>@.";
  fprintf fmt "<Traces xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">@.";
  fprintf fmt "@[<v 5>@ %a@ @]@."
    (Utils.fprintf_list ~sep:"@ " (fun fmt m ->
      let pp_var = pp_horn_var m in
      let memories_old = memories_old  machines m in
      let memories_next = memories_next  machines m in
      
      (* fprintf fmt "; Node %s@." m.mname.node_id; *)
      fprintf fmt "@[<v 2><Node name=\"%s\">@ " m.mname.node_id;
      
      let input_vars = (rename_machine_list m.mname.node_id m.mstep.step_inputs) in
      let output_vars = (rename_machine_list m.mname.node_id m.mstep.step_outputs) in
      fprintf fmt "<input name=\"%a\" type=\"%a\">%a</input>@ "
	(Utils.fprintf_list ~sep:" | " (pp_horn_var m)) input_vars
	(Utils.fprintf_list ~sep:" | "  (fun fmt id -> pp_type fmt id.var_type)) input_vars
	(Utils.fprintf_list ~sep:" | " (pp_horn_var m)) (m.mstep.step_inputs);

      fprintf fmt "<output name=\"%a\" type=\"%a\">%a</output>@ "
	(Utils.fprintf_list ~sep:" | " pp_var)  output_vars
	(Utils.fprintf_list ~sep:" | "  (fun fmt id -> pp_type fmt id.var_type)) output_vars
	(Utils.fprintf_list ~sep:" | " pp_var) (m.mstep.step_outputs);

      let local_vars =
	(try
	   full_memory_vars ~without_arrow:true machines m
	 with Not_found -> Format.eprintf "machine %s@.@?" m.mname.node_id; assert false
	)
      in
      let init_local_vars = rename_next_list local_vars in
      let step_local_vars = rename_current_list local_vars in

      fprintf fmt "<localInit name=\"%a\" type=\"%a\">%t%a</localInit>@ "
	(Utils.fprintf_list ~sep:" | " pp_var) init_local_vars
	(Utils.fprintf_list ~sep:" | "  (fun fmt id -> pp_type fmt id.var_type)) init_local_vars
	(fun fmt -> match memories_next with [] -> () | _ -> fprintf fmt "")
	(Utils.fprintf_list ~sep:" | " (fun fmt (prefix, ee) -> fprintf fmt "%a" pp_xml_expr ee)) memories_next;

      fprintf fmt "<localStep name=\"%a\" type=\"%a\">%t%a</localStep>@ "
	(Utils.fprintf_list ~sep:" | " pp_var) step_local_vars
	(Utils.fprintf_list ~sep:" | "  (fun fmt id -> pp_type fmt id.var_type)) step_local_vars
	(fun fmt -> match memories_old with [] -> () | _ -> fprintf fmt "")
	(Utils.fprintf_list ~sep:" | " (fun fmt (prefix,ee) -> fprintf fmt "(%a)"
          pp_xml_expr ee)) (memories_old);

      let arrow_vars = arrow_vars machines m in
      let arrow_vars_curr = rename_current_list arrow_vars and
          arrow_vars_mid = rename_mid_list arrow_vars and
	  arrow_vars_next = rename_next_list arrow_vars
      in
      Utils.fprintf_list ~sep:"@ "
      	(fun fmt v -> fprintf fmt "<reset name=\"%a\"/>" pp_var v)
      	fmt (arrow_vars_curr@arrow_vars_mid@arrow_vars_next);
      fprintf fmt "@]@ </Node>";
     )) (List.rev machines);
  fprintf fmt "</Traces>@."
  

(* (Utils.fprintf_list ~sep:" | " (fun fmt (prefix, ee) -> fprintf fmt
   "%a%a" pp_prefix_rev prefix Printers.pp_expr ee)) memories_next; *)
(* (Utils.fprintf_list ~sep:" | " (fun fmt (prefix,ee) -> fprintf fmt
   "%a(%a)" *)
(* pp_prefix_rev prefix Printers.pp_expr ee)) (memories_old); *)


(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
