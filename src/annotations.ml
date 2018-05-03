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


(* Associate to each annotation key the pair (node, expr tag) *)
let expr_annotations : (string list, ident * tag) Hashtbl.t=  Hashtbl.create 13
let node_annotations : (string list, ident) Hashtbl.t=  Hashtbl.create 13

let add_expr_ann node_id expr_tag key = Hashtbl.add expr_annotations key (node_id, expr_tag)
let add_node_ann node_id key = Hashtbl.add node_annotations key node_id

let get_expr_annotations key = Hashtbl.find_all expr_annotations key
