open Corelang

(* Consts unfoooolding *)
let is_const i consts = 
  List.exists (fun c -> c.const_id = i) consts

let get_const i consts =
  let c = List.find (fun c -> c.const_id = i) consts in
  c.const_value

let rec expr_unfold_consts consts e = 
{ e with expr_desc = expr_desc_unfold_consts consts e.expr_desc }

and expr_desc_unfold_consts consts e =
  let unfold = expr_unfold_consts consts in
  match e with
  | Expr_const _ -> e
  | Expr_ident i -> if is_const i consts then Expr_const (get_const i consts) else e
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
  | Expr_uclock (e', i) -> Expr_uclock (unfold e', i) 
  | Expr_dclock (e', i) -> Expr_dclock (unfold e', i)
  | Expr_phclock _ -> e  

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

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
