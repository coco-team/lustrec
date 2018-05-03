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

open Lustre_types
open Corelang

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
	Causality.TopologicalDepGraph.fold 
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
  with (Causality.Error err) as exc ->
    Causality.pp_error Format.err_formatter err;
    raise exc
  in
  Log.report ~level:3 
    (fun fmt -> Format.fprintf fmt "Ordered list of declarations:@.%a@.@?" (Utils.fprintf_list ~sep:"@." Printers.pp_short_decl) sorted);
  	  not_nodes@sorted


let sort_node_locals nd =
  { nd with node_locals = Causality.VarClockDep.sort nd.node_locals}
    
let sort_nodes_locals prog =
  List.map
    (fun top ->
      match top.top_decl_desc with
      | Node nd -> {top with top_decl_desc = Node (sort_node_locals nd)}
      | _ -> top
    )
    prog
    
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
