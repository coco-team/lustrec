open Lustre_types 
open Corelang
open Log
open Format

module IdSet = Set.Make (struct type t = expr * int let compare = compare end)

let inout_vars = ref [] 

(* This was used to add inout variables in the final signature. May have to be
   reactivated later *)
  
(* let print_tautology_var fmt v = *)
(*   match (Types.repr v.var_type).Types.tdesc with *)
(*   | Types.Tbool -> Format.fprintf fmt "(%s or not %s)" v.var_id v.var_id *)
(*   | Types.Tint -> Format.fprintf fmt "(%s > 0 or %s <= 0)" v.var_id v.var_id *)
(*   | Types.Treal -> Format.fprintf fmt "(%s > 0 or %s <= 0)" v.var_id v.var_id *)
(*   | _ -> Format.fprintf fmt "(true)" *)

(* let print_path arg = match !inout_vars with *)
(*   | [] -> Format.printf "%t@." arg   *)
(*   | l -> Format.printf "%t and %a@." arg (Utils.fprintf_list ~sep:" and " (fun fmt elem -> print_tautology_var fmt elem)) l *)

let rel_op = ["="; "!="; "<"; "<="; ">" ; ">=" ]

(* Used when we were printing the expression directly. Now we are constructing
   them as regular expressions.

   let rec print_pre fmt nb_pre = if nb_pre <= 0 then () else ( Format.fprintf
   fmt "pre "; print_pre fmt (nb_pre-1) )
*)
  
let rec mk_pre n e =
  if n <= 0 then
    e
  else
    mkexpr e.expr_loc (Expr_pre e)
   
(*
   let combine2 f sub1 sub2 = 
   let elem_e1 = List.fold_right IdSet.add (List.map fst sub1) IdSet.empty in
   let elem_e2 = List.fold_right IdSet.add (List.map fst sub2) IdSet.empty in
   let common = IdSet.inter elem_e1 elem_e2 in
   let sub1_filtered = List.filter (fun (v, _) -> not (IdSet.mem v common)) sub1 in
   let sub2_filtered = List.filter (fun (v, _) -> not (IdSet.mem v common)) sub2 in
   (List.map (fun (v, negv) -> (v, f negv e2)) sub1_filtered) @
   (List.map (fun (v, negv) -> (v, f e1 negv)) sub2_filtered) @
   (List.map (fun v -> (v, {expr with expr_desc = Expr_arrow(List.assoc v sub1, List.assoc v sub2)}) (IdSet.elements common))      )
*)

let rec select (v: expr * int) (active: bool list) (modified: ((expr * int) * expr) list list) (orig: expr list) =
match active, modified, orig with
| true::active_tl, e::modified_tl, _::orig_tl -> (List.assoc v e)::(select v active_tl modified_tl orig_tl)
| false::active_tl, _::modified_tl, e::orig_tl -> e::(select v active_tl modified_tl orig_tl)
| [], [], [] -> []
| _ -> assert false
  
let combine (f: expr list -> expr ) subs orig : ((expr * int) * expr) list  = 
  let elems = List.map (fun sub_i -> List.fold_right IdSet.add (List.map fst sub_i) IdSet.empty) subs in
  let all = List.fold_right IdSet.union elems IdSet.empty in
  List.map (fun v ->
    let active_subs = List.map (IdSet.mem v) elems in
    v, f (select v active_subs subs orig)
  ) (IdSet.elements all)


(* In a previous version, the printer was introducing fake description, ie
   tautologies, over inout variables to make sure they were not suppresed by
   some other algorithms *)

(* Takes the variable on which these coverage criteria will apply, as well as
   the expression and its negated version. Returns the expr and the variable
   expression, as well as the two new boolean expressions descibing the two
   associated modes. *)
let mcdc_var vi_as_expr expr expr_neg_vi =
  let loc = expr.expr_loc in
  let changed_expr = mkpredef_call loc "!=" [expr; expr_neg_vi] in
  let not_vi_as_expr = mkpredef_call loc "not" [vi_as_expr] in
  let expr1 = mkpredef_call loc "&&" [vi_as_expr; changed_expr] in
  let expr2 = mkpredef_call loc "&&" [not_vi_as_expr; changed_expr] in
  ((expr,vi_as_expr),[(true,expr1);(false,expr2)]) (* expr1 corresponds to atom
                                                     true while expr2
                                                     corresponds to atom
                                                     false *)

  (* Format.printf "%a@." Printers.pp_expr expr1;  *)
  (* print_path (fun fmt -> Format.fprintf fmt "%a and (%a != %a)" *)
  (*   Printers.pp_expr vi_as_expr *)
  (*   Printers.pp_expr expr (\*v*\) *)
  (*   Printers.pp_expr expr_neg_vi); *)
  (* Format.printf "%a@." Printers.pp_expr expr2;  *)
  (* print_path (fun fmt -> Format.fprintf fmt "(not %a) and (%a != %a)" *)
  (*   Printers.pp_expr vi_as_expr *)
  (*   Printers.pp_expr expr (\*v*\) *)
  (*   Printers.pp_expr expr_neg_vi) *)
    
let rec compute_neg_expr cpt_pre (expr: Lustre_types.expr) =
  let neg_list l = 
    List.fold_right (fun e (vl,el) -> let vl', e' = compute_neg_expr cpt_pre e in (vl'@vl), e'::el) l ([], [])
  in
  match expr.expr_desc with
  | Expr_tuple l -> 
     let vl, neg = neg_list l in
     vl, combine (fun l' -> {expr with expr_desc = Expr_tuple l'}) neg l
       
  | Expr_ite (i,t,e) when (Types.is_bool_type t.expr_type) -> (
    let list = [i; t; e] in
    let vl, neg = neg_list list in
    vl, combine (fun l ->
      match l with
      | [i'; t'; e'] -> {expr with expr_desc = Expr_ite(i', t', e')}
      | _ -> assert false
    ) neg list
  )
  | Expr_ite (i,t,e) -> ( (* We return the guard as a new guard *)
    let vl = gen_mcdc_cond_guard i in
    let list = [i; t; e] in
    let vl', neg = neg_list list in
    vl@vl', combine (fun l ->
      match l with
      | [i'; t'; e'] -> {expr with expr_desc = Expr_ite(i', t', e')}
      | _ -> assert false
    ) neg list
  )
  | Expr_arrow (e1, e2) -> 
     let vl1, e1' = compute_neg_expr cpt_pre e1 in
     let vl2, e2' = compute_neg_expr cpt_pre e2 in
     vl1@vl2, combine (fun l -> match l with
     | [x;y] -> { expr with expr_desc = Expr_arrow (x, y) }
     | _ -> assert false
     ) [e1'; e2'] [e1; e2]

  | Expr_pre e ->
     let vl, e' = compute_neg_expr (cpt_pre+1) e in
     vl, List.map
       (fun (v, negv) -> (v, { expr with expr_desc = Expr_pre negv } )) e'

  | Expr_appl (op_name, args, r) when List.mem op_name rel_op -> 
     [], [(expr, cpt_pre), mkpredef_call expr.expr_loc "not" [expr]]

  | Expr_appl (op_name, args, r) ->
     let vl, args' = compute_neg_expr cpt_pre args in
     vl, List.map 
       (fun (v, negv) -> (v, { expr with expr_desc = Expr_appl (op_name, negv, r) } ))
       args'

  | Expr_ident _ when (Types.is_bool_type expr.expr_type) ->
     [], [(expr, cpt_pre), mkpredef_call expr.expr_loc "not" [expr]]
  | _ -> [] (* empty vars *) , [] 
and gen_mcdc_cond_var v expr =
  report ~level:1 (fun fmt ->
    Format.fprintf fmt ".. Generating MC/DC cond for boolean flow %s and expression %a@."
      v
      Printers.pp_expr expr);
  let vl, leafs_n_neg_expr = compute_neg_expr 0 expr in
  if List.length leafs_n_neg_expr >= 1 then (
    List.fold_left (fun accu ((vi, nb_pre), expr_neg_vi) ->
      (mcdc_var  (mk_pre nb_pre vi) expr expr_neg_vi)::accu
    ) vl leafs_n_neg_expr
  )
  else
    (* TODO: deal with the case length xxx = 1 with a simpler condition  *)
    vl

and gen_mcdc_cond_guard expr =
  report ~level:1 (fun fmt ->
    Format.fprintf fmt".. Generating MC/DC cond for guard %a@."
      Printers.pp_expr expr);
  let vl, leafs_n_neg_expr = compute_neg_expr 0 expr in
  if List.length leafs_n_neg_expr >= 1 then (
    List.fold_left (fun accu ((vi, nb_pre), expr_neg_vi) ->
      (mcdc_var  (mk_pre nb_pre vi) expr expr_neg_vi)::accu
    ) vl leafs_n_neg_expr)
  else
    (* TODO: deal with the case length xxx = 1 with a simpler condition  *)
    vl
  

let rec mcdc_expr cpt_pre expr = 
  match expr.expr_desc with
  | Expr_tuple l ->
     let vl =
       List.fold_right (fun e accu_v ->
	 let vl = mcdc_expr cpt_pre e in
	 (vl@accu_v))
	 l
	 []
     in
     vl
  | Expr_ite (i,t,e) ->
     let vl_i = gen_mcdc_cond_guard i in
     let vl_t = mcdc_expr cpt_pre t in
     let vl_e = mcdc_expr cpt_pre e in
     vl_i@vl_t@vl_e
  | Expr_arrow (e1, e2) ->
     let vl1 = mcdc_expr cpt_pre e1 in
     let vl2 = mcdc_expr cpt_pre e2 in
     vl1@vl2
  | Expr_pre e ->
     let vl = mcdc_expr (cpt_pre+1) e in
     vl
  | Expr_appl (f, args, r) ->
     let vl = mcdc_expr cpt_pre args in
     vl
  | _ -> []

let mcdc_var_def v expr = 
  if Types.is_bool_type expr.expr_type then
     let vl = gen_mcdc_cond_var v expr in
     vl
  else
    let vl = mcdc_expr 0 expr in
    vl
      
let mcdc_node_eq eq =
  let vl =
    match eq.eq_lhs, Types.is_bool_type eq.eq_rhs.expr_type, (Types.repr eq.eq_rhs.expr_type).Types.tdesc, eq.eq_rhs.expr_desc with
    | [lhs], true, _, _ -> gen_mcdc_cond_var lhs eq.eq_rhs 
    | _::_, false, Types.Ttuple tl, Expr_tuple rhs ->
       (* We iterate trough pairs, but accumulate variables aside. The resulting
	  expression shall remain a tuple defintion *)
       let vl = List.fold_right2 (fun lhs rhs accu ->
	 let v = mcdc_var_def lhs rhs in
	 (* we don't care about the expression it. We focus on the coverage
	    expressions in v *)
	 v@accu
       ) eq.eq_lhs rhs []
       in
       vl
    | _ -> mcdc_expr 0 eq.eq_rhs 
  in
  vl

let mcdc_node_stmt stmt =
  match stmt with
  | Eq eq -> let vl = mcdc_node_eq eq in vl
  | Aut aut -> assert false

let mcdc_top_decl td = 
  match td.top_decl_desc with
  | Node nd ->
     let new_coverage_exprs =
       List.fold_right (
	   fun s accu_v ->
	   let vl' = mcdc_node_stmt s in
	   vl'@accu_v
	 ) nd.node_stmts []
     in
     (* We add coverage vars as boolean internal flows. *)
     let fresh_cov_defs = List.flatten (List.map (fun ((_, atom), expr_l) -> List.map (fun (atom_valid, case) -> atom, atom_valid, case) expr_l) new_coverage_exprs) in
     let nb_total = List.length fresh_cov_defs in
     let fresh_cov_vars = List.mapi (fun i (atom, atom_valid, cov_expr) ->
				     let loc = cov_expr.expr_loc in
				     Format.fprintf Format.str_formatter "__cov_%i_%i" i nb_total;
				     let cov_id = Format.flush_str_formatter () in
				     let cov_var = mkvar_decl loc
							      (cov_id, mktyp loc Tydec_bool, mkclock loc Ckdec_any, false, None, None) in
				     let cov_def = Eq (mkeq loc ([cov_id], cov_expr)) in
				     cov_var, cov_def, atom, atom_valid
				    ) fresh_cov_defs
     in
     let fresh_vars, fresh_eqs =
       List.fold_right
	 (fun (v,eq,_,_) (accuv, accueq)-> v::accuv, eq::accueq )
	 fresh_cov_vars
	 ([], [])
     in
     let fresh_annots = (* We produce two sets of annotations: PROPERTY ones for
			   kind2, and regular ones to keep track of the nature
			   of the annotations. *)
       List.map
	 (fun (v, _, atom, atom_valid) ->
	  let e = expr_of_vdecl v in
	  let neg_ee = expr_to_eexpr (mkpredef_call e.expr_loc "not" [e]) in
	  {annots =  [["PROPERTY"], neg_ee; (* Using negated property to force
                                               model-checker to produce a
                                               suitable covering trace *)
                      let loc = Location.dummy_loc in
		      let valid_e = let open Corelang in mkexpr loc (Expr_const (const_of_bool atom_valid)) in
		      ["coverage";"mcdc";v.var_id], expr_to_eexpr (Corelang.expr_of_expr_list loc [e; atom; valid_e])
		     ];
	   annot_loc = v.var_loc})
	 fresh_cov_vars
     in
     Format.printf "%i coverage criteria generated for node %s@ " nb_total nd.node_id;
     (* And add them as annotations --%PROPERTY: var TODO *)
     {td with top_decl_desc = Node {nd with
				     node_locals = nd.node_locals@fresh_vars;
				     node_stmts = nd.node_stmts@fresh_eqs;
				     node_annot = nd.node_annot@fresh_annots
				   }}
  | _ -> td


let mcdc prog =
  (* If main node is provided add silly constraints to show in/out variables in the path condition *)
  if !Options.main_node <> "" then (
    inout_vars := 
      let top = List.find 
	(fun td -> 
	  match td.top_decl_desc with 
	  | Node nd when nd.node_id = !Options.main_node -> true
	  | _ -> false) 
	prog 
      in
      match top.top_decl_desc with
      | Node nd -> nd.node_inputs @ nd.node_outputs
      | _ -> assert false);
  List.map mcdc_top_decl prog


    
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)

    
