open Graph
open LustreSpec
open Corelang
module TopologicalDepGraph = Topological.Make(Causality.IdentDepGraph)

let get_node nid prog =
  List.find (fun t -> match t.top_decl_desc with Node n -> n.node_id = nid | _ -> false) prog

let check_external_defs x not_nodes = true (* TODO, check whether a node, a function or an include defines this node *)

let sort prog =
  let not_nodes, nodes = 
    List.partition (fun top -> match top.top_decl_desc with Node _ -> false | _ -> true) prog 
  in
  let sorted = 
    try
      let g = Causality.NodeDep.dependence_graph nodes in
      Causality.CycleDetection.check_cycles g;
    
      (
	TopologicalDepGraph.fold 
	  (fun x accu -> 
	    try 
	      (get_node x nodes)::accu
	    with Not_found -> 
	      (* check whether it is an imported node, a function or in the includes *)
	      if check_external_defs x not_nodes then
		accu 
	      else 
		(Format.eprintf "Impossible to find node %s@.@?" x; failwith x)
	  )
	  g []
      )
  with (Causality.Cycle v) as exc ->
    Causality.pp_error Format.err_formatter v;
    raise exc
  in
  Log.report ~level:3 
    (fun fmt -> Format.fprintf fmt "Ordered list of declarations:@.%a@.@?" (Utils.fprintf_list ~sep:"@." Printers.pp_short_decl) sorted);
  	  not_nodes@sorted

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
