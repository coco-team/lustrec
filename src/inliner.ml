open LustreSpec
open Corelang

let check_node_name id = (fun t -> 
  match t.top_decl_desc with 
  | Node nd -> nd.node_id = id 
  | _ -> false) 


(* 
    expr, locals', eqs = inline_call id args' reset locals nodes

We select the called node equations and variables.
   renamed_inputs = args
   renamed_eqs

the resulting expression is tuple_of_renamed_outputs
   
TODO: convert the specification/annotation/assert and inject them
TODO: deal with reset
*)
let inline_call orig_expr args reset locals node =
  let loc = orig_expr.expr_loc in
  let uid = orig_expr.expr_tag in
  let rename v = 
    Format.fprintf Format.str_formatter "%s_%i_%s" 
      node.node_id uid v;
    Format.flush_str_formatter ()
  in
  let eqs' = List.map 
    (fun eq -> { eq with
      eq_lhs = List.map rename eq.eq_lhs; 
      eq_rhs = expr_replace_var rename eq.eq_rhs
    } ) node.node_eqs
  in
  let rename_var v = { v with var_id = rename v.var_id } in
  let inputs' = List.map rename_var node.node_inputs in
  let outputs' = List.map rename_var node.node_outputs in
  let locals' = List.map rename_var node.node_locals in

  (* checking we are at the appropriate (early) step: node_checks and
     node_gencalls should be empty (not yet assigned) *)
  assert (node.node_checks = []);
  assert (node.node_gencalls = []);

  (* Bug included: todo deal with reset *)
  assert (reset = None);

  let assign_inputs = mkeq loc (List.map (fun v -> v.var_id) inputs', args) in
  let expr = expr_of_expr_list 
    loc 
    (List.map (fun v -> mkexpr loc (Expr_ident v.var_id)) outputs')
  in
  expr , inputs'@outputs'@locals'@locals, assign_inputs::eqs'




(* 
   new_expr, new_locals, new_eqs = inline_expr expr locals nodes
   
   Each occurence of a node in nodes in the expr should be replaced by fresh
   variables and the code of called node instance added to new_eqs

*)
let rec inline_expr expr locals nodes =
  let inline_tuple el = 
    List.fold_right (fun e (el_tail, locals, eqs) -> 
      let e', locals', eqs' = inline_expr e locals nodes in
      e'::el_tail, locals', eqs'@eqs
    ) el ([], locals, [])
  in
  let inline_pair e1 e2 = 
    let el', l', eqs' = inline_tuple [e1;e2] in
    match el' with
    | [e1'; e2'] -> e1', e2', l', eqs'
    | _ -> assert false
  in
  let inline_triple e1 e2 e3 = 
    let el', l', eqs' = inline_tuple [e1;e2;e3] in
    match el' with
    | [e1'; e2'; e3'] -> e1', e2', e3', l', eqs'
    | _ -> assert false
  in
    
  match expr.expr_desc with
  | Expr_appl (id, args, reset) ->
    let args', locals', eqs' = inline_expr args locals nodes in 
    if List.exists (check_node_name id) nodes then 
      (* The node should be inlined *)
(*      let _ =     Format.eprintf "Inlining call to %s@." id in
  *)    let node = try List.find (check_node_name id) nodes 
	with Not_found -> (assert false) in
      let node = match node.top_decl_desc with Node nd -> nd | _ -> assert false in
      let node = inline_node node nodes in
      let expr, locals', eqs'' = 
	inline_call expr args' reset locals' node in
      expr, locals', eqs'@eqs''
    else 
      (* let _ =     Format.eprintf "Not inlining call to %s@." id in *)
      { expr with expr_desc = Expr_appl(id, args', reset)}, locals', eqs'

  (* For other cases, we just keep the structure, but convert sub-expressions *)
  | Expr_const _ 
  | Expr_ident _ -> expr, locals, []
  | Expr_tuple el -> 
    let el', l', eqs' = inline_tuple el in
    { expr with expr_desc = Expr_tuple el' }, l', eqs'
  | Expr_ite (g, t, e) ->
    let g', t', e', l', eqs' = inline_triple g t e in
    { expr with expr_desc = Expr_ite (g', t', e') }, l', eqs'
  | Expr_arrow (e1, e2) ->
    let e1', e2', l', eqs' = inline_pair e1 e2 in
    { expr with expr_desc = Expr_arrow (e1', e2') } , l', eqs'
  | Expr_fby (e1, e2) ->
    let e1', e2', l', eqs' = inline_pair e1 e2 in
    { expr with expr_desc = Expr_fby (e1', e2') }, l', eqs'
  | Expr_array el ->
    let el', l', eqs' = inline_tuple el in
    { expr with expr_desc = Expr_array el' }, l', eqs'
  | Expr_access (e, dim) ->
    let e', l', eqs' = inline_expr e locals nodes in 
    { expr with expr_desc = Expr_access (e', dim) }, l', eqs'
  | Expr_power (e, dim) ->
    let e', l', eqs' = inline_expr e locals nodes in 
    { expr with expr_desc = Expr_power (e', dim) }, l', eqs'
  | Expr_pre e ->
    let e', l', eqs' = inline_expr e locals nodes in 
    { expr with expr_desc = Expr_pre e' }, l', eqs'
  | Expr_when (e, id, label) ->
    let e', l', eqs' = inline_expr e locals nodes in 
    { expr with expr_desc = Expr_when (e', id, label) }, l', eqs'
  | Expr_merge (id, branches) ->
    let el, l', eqs' = inline_tuple (List.map snd branches) in
    let branches' = List.map2 (fun (label, _) v -> label, v) branches el in
    { expr with expr_desc = Expr_merge (id, branches') }, l', eqs'
  | Expr_uclock _
  | Expr_dclock _
  | Expr_phclock _ -> assert false 
and inline_node nd nodes = 
  let new_locals, eqs = 
    List.fold_left (fun (locals, eqs) eq ->
      let eq_rhs', locals', new_eqs' = 
	inline_expr eq.eq_rhs locals nodes 
      in
      locals', { eq with eq_rhs = eq_rhs' }::new_eqs'@eqs 
    ) (nd.node_locals, []) nd.node_eqs
  in
  { nd with
    node_locals = new_locals;
    node_eqs = eqs
  }

let inline_all_calls node nodes =
  let nd = match node.top_decl_desc with Node nd -> nd | _ -> assert false in
  { node with top_decl_desc = Node (inline_node nd nodes) }
    




let witness filename main_name orig inlined type_env clock_env =
  let loc = Location.dummy_loc in
  let rename_local_node nodes prefix id =
    if List.exists (check_node_name id) nodes then
      prefix ^ id 
    else
      id
  in
  let main_orig_node = match (List.find (check_node_name main_name) orig).top_decl_desc with
  Node nd -> nd | _ -> assert false in
  
  let orig_rename = rename_local_node orig "orig_" in
  let inlined_rename = rename_local_node inlined "inlined_" in
  let identity = (fun x -> x) in
  let is_node top = match top.top_decl_desc with Node _ -> true | _ -> false in
  let orig = rename_prog orig_rename identity identity orig in
  let inlined = rename_prog inlined_rename identity identity inlined in
  let nodes_origs, others = List.partition is_node orig in
  let nodes_inlined, _ = List.partition is_node inlined in

  (* One ok_i boolean variable  per output var *)
  let nb_outputs = List.length main_orig_node.node_outputs in
  let ok_i = List.map (fun id ->
    mkvar_decl 
      loc 
      ("OK" ^ string_of_int id,
       {ty_dec_desc=Tydec_bool; ty_dec_loc=loc},
       {ck_dec_desc=Ckdec_any; ck_dec_loc=loc},
       false)
  ) (Utils.enumerate nb_outputs) 
  in

  (* OK = ok_1 and ok_2 and ... ok_n-1 *)
  let ok_ident = "OK" in
  let ok_output = mkvar_decl 
    loc 
    (ok_ident,
     {ty_dec_desc=Tydec_bool; ty_dec_loc=loc},
     {ck_dec_desc=Ckdec_any; ck_dec_loc=loc},
     false)
  in
  let main_ok_expr =
    let mkv x = mkexpr loc (Expr_ident x) in
    match ok_i with
    | [] -> assert false
    | [x] -> mkv x.var_id 
    | hd::tl -> 
      List.fold_left (fun accu elem -> 
	mkpredef_call loc "&&" [mkv elem.var_id; accu]
      ) (mkv hd.var_id) tl
  in

  (* Building main node *)

  let main_node = {
    node_id = "check";
    node_type = Types.new_var ();
    node_clock = Clocks.new_var true;
    node_inputs = main_orig_node.node_inputs;
    node_outputs = [ok_output];
    node_locals = [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_eqs = [
      { eq_loc = loc;
	eq_lhs = List.map (fun v -> v.var_id) ok_i;
	eq_rhs = 
	  let inputs = expr_of_expr_list  loc (List.map (fun v -> mkexpr loc (Expr_ident v.var_id)) main_orig_node.node_inputs) in
	  let call_orig = 
	    mkexpr loc (Expr_appl ("orig_" ^ main_name, inputs, None)) in
	  let call_inlined = 
	    mkexpr loc (Expr_appl ("inlined_" ^ main_name, inputs, None)) in
	  let args = mkexpr loc (Expr_tuple [call_orig; call_inlined]) in 
	  mkexpr loc (Expr_appl ("=", args, None))
      };
      { eq_loc = loc;
	eq_lhs = [ok_ident];
	eq_rhs = main_ok_expr;
      }
    ];
    node_dec_stateless = false;
    node_stateless = None;
    node_spec = Some 
      {requires = []; 
       ensures = [EnsuresExpr (mkeexpr loc (EExpr_ident ok_ident))];
       behaviors = []
      };
    node_annot = None;
  }
  in
  let main = [{ top_decl_desc = Node main_node; top_decl_loc = loc }] in
  let new_prog = others@nodes_origs@nodes_inlined@main in
  let _ = Typing.type_prog type_env new_prog in
  let _ = Clock_calculus.clock_prog clock_env new_prog in

   
  let witness_file = (Options.get_witness_dir filename) ^ "/" ^ "inliner_witness.lus" in
  let witness_out = open_out witness_file in
  let witness_fmt = Format.formatter_of_out_channel witness_out in
  Format.fprintf witness_fmt
    "(* Generated lustre file to check validity of inlining process *)@.";
  Printers.pp_prog witness_fmt new_prog;
  Format.fprintf witness_fmt "@.";
  () (* xx *)

let global_inline basename prog type_env clock_env =
  (* We select the main node desc *)
  let main_node, other_nodes, other_tops = 
    List.fold_left 
      (fun (main_opt, nodes, others) top -> 
	match top.top_decl_desc with 
	| Node nd when nd.node_id = !Options.main_node -> 
	  Some top, nodes, others
	| Node _ -> main_opt, top::nodes, others
	| _ -> main_opt, nodes, top::others) 
      (None, [], []) prog 
  in
  (* Recursively each call of a node in the top node is replaced *)
  let main_node = Utils.desome main_node in
  let main_node' = inline_all_calls main_node other_nodes in
  let res = main_node'::other_tops in
  if !Options.witnesses then (
    witness 
      basename
      (match main_node.top_decl_desc  with Node nd -> nd.node_id | _ -> assert false) 
      prog res type_env clock_env
  );
  res

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
