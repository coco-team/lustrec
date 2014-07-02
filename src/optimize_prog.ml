open Corelang
open LustreSpec

(* Consts unfoooolding *)
let is_const i consts = 
  List.exists (fun c -> c.const_id = i) consts

let get_const i consts =
  let c = List.find (fun c -> c.const_id = i) consts in
  c.const_value

let rec expr_unfold_consts consts e = 
{ e with expr_desc = expr_desc_unfold_consts consts e.expr_desc e.expr_type }

and expr_desc_unfold_consts consts e e_type =
  let unfold = expr_unfold_consts consts in
  match e with
  | Expr_const _ -> e
  | Expr_ident i -> if is_const i consts && not (Types.is_array_type e_type) then Expr_const (get_const i consts) else e
  | Expr_array el -> Expr_array (List.map unfold el)
  | Expr_access (e1, d) -> Expr_access (unfold e1, d)
  | Expr_power (e1, d) -> Expr_power (unfold e1, d)
  | Expr_tuple el -> Expr_tuple (List.map unfold el)
  | Expr_ite (c, t, e) -> Expr_ite (unfold c, unfold t, unfold e)
  | Expr_arrow (e1, e2)-> Expr_arrow (unfold e1, unfold e2) 
  | Expr_fby (e1, e2) -> Expr_fby (unfold e1, unfold e2)
  (* | Expr_concat (e1, e2) -> Expr_concat (unfold e1, unfold e2) *)
  (* | Expr_tail e' -> Expr_tail (unfold e') *)
  | Expr_pre e' -> Expr_pre (unfold e')
  | Expr_when (e', i, l)-> Expr_when (unfold e', i, l)
  | Expr_merge (i, hl) -> Expr_merge (i, List.map (fun (t, h) -> (t, unfold h)) hl)
  | Expr_appl (i, e', i') -> Expr_appl (i, unfold e', i')  

let eq_unfold_consts consts eq =
  { eq with eq_rhs = expr_unfold_consts consts eq.eq_rhs }

let node_unfold_consts consts node = 
  { node with node_eqs = List.map (eq_unfold_consts consts) node.node_eqs }

let prog_unfold_consts prog =
  let consts = get_consts prog in
    List.map (
      fun decl -> match decl.top_decl_desc with 
	| Node nd -> {decl with top_decl_desc = Node (node_unfold_consts consts nd)}
	| _       -> decl
    ) prog 

let apply_stack expr stack =
 List.fold_left (fun expr (v, t) -> mkexpr expr.expr_loc (Expr_when (expr, v, t))) expr stack

let expr_distribute_when expr =
  let rec distrib stack expr =
    match expr.expr_desc with
    | Expr_const _
    | Expr_ident _
    | Expr_arrow _
    | Expr_fby _
    | Expr_pre _
	-> apply_stack expr stack
    | Expr_appl (id, _, _) when not (Stateless.check_node (node_from_name id))
	-> apply_stack expr stack
    | Expr_ite (c, t, e)
        -> let cid = ident_of_expr c in
           mkexpr expr.expr_loc
	     (Expr_merge (cid,
			  [(tag_true , distrib ((cid,tag_true )::stack) t);
			   (tag_false, distrib ((cid,tag_false)::stack) e)]))
    | Expr_array el -> { expr with expr_desc = (Expr_array (List.map (distrib stack) el)) }
    | Expr_access (e1, d) -> { expr with expr_desc = Expr_access (distrib stack e1, d) }
    | Expr_power (e1, d) -> { expr with expr_desc = Expr_power (distrib stack e1, d) }
    | Expr_tuple el -> { expr with expr_desc = Expr_tuple (List.map (distrib stack) el) }
    | Expr_when (e', i, l)-> distrib ((i, l)::stack) e'
    | Expr_merge (i, hl) -> { expr with expr_desc = Expr_merge (i, List.map (fun (t, h) -> (t, distrib stack h)) hl) }
    | Expr_appl (id, e', i') -> { expr with expr_desc = Expr_appl (id, distrib stack e', i')}
  in distrib [] expr

let eq_distribute_when eq =
  { eq with eq_rhs = expr_distribute_when eq.eq_rhs }

let node_distribute_when node =
  { node with node_eqs = List.map eq_distribute_when node.node_eqs }

let prog_distribute_when prog =
    List.map (
      fun decl -> match decl.top_decl_desc with 
	| Node nd -> {decl with top_decl_desc = Node (node_distribute_when nd)}
	| _       -> decl
    ) prog 
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
