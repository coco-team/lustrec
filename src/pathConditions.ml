open LustreSpec 
open Corelang
open Log
open Format

module IdSet = Set.Make (struct type t = expr * int let compare = compare end)

let inout_vars = ref [] 

let print_tautology_var fmt v =
  match (Types.repr v.var_type).Types.tdesc with
  | Types.Tbool -> Format.fprintf fmt "(%s or not %s)" v.var_id v.var_id
  | Types.Tint -> Format.fprintf fmt "(%s > 0 or %s <= 0)" v.var_id v.var_id
  | Types.Treal -> Format.fprintf fmt "(%s > 0 or %s <= 0)" v.var_id v.var_id
  | _ -> Format.fprintf fmt "(true)"

let print_path arg = match !inout_vars with
  | [] -> Format.printf "%t@." arg  
  | l -> Format.printf "%t and %a@." arg (Utils.fprintf_list ~sep:" and " (fun fmt elem -> print_tautology_var fmt elem)) l

let rel_op = ["="; "!="; "<"; "<="; ">" ; ">=" ]

let rec print_pre fmt nb_pre =
  if nb_pre <= 0 then () 
  else (
    Format.fprintf fmt "pre ";
    print_pre fmt (nb_pre-1)
  )
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

let rec compute_neg_expr cpt_pre expr = 
  match expr.expr_desc with
  | Expr_tuple l -> 
    let neg = List.map (compute_neg_expr cpt_pre) l in
    combine (fun l' -> {expr with expr_desc = Expr_tuple l'}) neg l

  | Expr_ite (i,t,e) when (Types.repr t.expr_type).Types.tdesc = Types.Tbool -> 
    let list = [i; t; e] in
    let neg = List.map (compute_neg_expr cpt_pre) list in
    combine (fun [i'; t'; e'] -> {expr with expr_desc = Expr_ite(i', t', e')}) neg list
  | Expr_ite (i,t,e) -> ( (* We return the guard as a new guard *)
    gen_mcdc_cond_guard i;
    let list = [i; t; e] in
    let neg = List.map (compute_neg_expr cpt_pre) list in
    combine (fun [i'; t'; e'] -> {expr with expr_desc = Expr_ite(i', t', e')}) neg list
  )
  | Expr_arrow (e1, e2) -> 
    let e1' = compute_neg_expr cpt_pre e1 in
    let e2' = compute_neg_expr cpt_pre e2 in
    combine (fun [x;y] -> { expr with expr_desc = Expr_arrow (x, y) }) [e1'; e2'] [e1; e2]
  | Expr_pre e -> 
    List.map 
      (fun (v, negv) -> (v, { expr with expr_desc = Expr_pre negv } ))
      (compute_neg_expr (cpt_pre+1) e)

  | Expr_appl (op_name, args, r) when List.mem op_name rel_op -> 
    [(expr, cpt_pre), mkpredef_unary_call Location.dummy_loc "not" expr]

  | Expr_appl (op_name, args, r) -> 
    List.map 
      (fun (v, negv) -> (v, { expr with expr_desc = Expr_appl (op_name, negv, r) } ))
	(compute_neg_expr cpt_pre args)

  | Expr_ident _ when (Types.repr expr.expr_type).Types.tdesc = Types.Tbool ->
    [(expr, cpt_pre), mkpredef_unary_call Location.dummy_loc "not" expr]
  | _ -> []

and  
 gen_mcdc_cond_var v expr =
  report ~level:1 (fun fmt -> Format.fprintf fmt ".. Generating MC/DC cond for boolean flow %s and expression %a@." v Printers.pp_expr expr);
  let leafs_n_neg_expr = compute_neg_expr 0 expr in
  if List.length leafs_n_neg_expr > 1 then (
    List.iter (fun ((vi, nb_pre), expr_neg_vi) -> 
      print_path (fun fmt -> Format.fprintf fmt "%a%a and (%s != %a)" print_pre nb_pre Printers.pp_expr vi v Printers.pp_expr expr_neg_vi);
      print_path (fun fmt -> Format.fprintf fmt "(not %a%a) and (%s != %a)" print_pre nb_pre Printers.pp_expr vi v Printers.pp_expr expr_neg_vi)
    ) leafs_n_neg_expr
  )

and gen_mcdc_cond_guard expr =
  report ~level:1 (fun fmt -> Format.fprintf fmt".. Generating MC/DC cond for guard %a@." Printers.pp_expr expr);
  let leafs_n_neg_expr = compute_neg_expr 0 expr in
  if List.length leafs_n_neg_expr > 1 then (
    List.iter (fun ((vi, nb_pre), expr_neg_vi) -> 
      print_path (fun fmt -> Format.fprintf fmt "%a%a and (%a != %a)" print_pre nb_pre Printers.pp_expr vi Printers.pp_expr expr Printers.pp_expr expr_neg_vi);
      print_path (fun fmt -> Format.fprintf fmt  "(not %a%a) and (%a != %a)" print_pre nb_pre  Printers.pp_expr vi Printers.pp_expr expr Printers.pp_expr expr_neg_vi)
   
 ) leafs_n_neg_expr
  )
  

let rec mcdc_expr cpt_pre expr = 
  match expr.expr_desc with
  | Expr_tuple l -> List.iter (mcdc_expr cpt_pre) l
  | Expr_ite (i,t,e) -> (gen_mcdc_cond_guard i; List.iter (mcdc_expr cpt_pre) [t; e])
  | Expr_arrow (e1, e2) -> List.iter (mcdc_expr cpt_pre) [e1; e2]
  | Expr_pre e -> mcdc_expr (cpt_pre+1) e 
  | Expr_appl (_, args, _) -> mcdc_expr cpt_pre args
  | _ -> ()

let mcdc_var_def v expr = 
  match (Types.repr expr.expr_type).Types.tdesc with
  | Types.Tbool -> gen_mcdc_cond_var v expr
  | _ -> mcdc_expr 0 expr

let mcdc_node_eq eq =
  match eq.eq_lhs, (Types.repr eq.eq_rhs.expr_type).Types.tdesc, eq.eq_rhs.expr_desc with
  | [lhs], Types.Tbool, _ -> gen_mcdc_cond_var lhs eq.eq_rhs 
  | _::_, Types.Ttuple tl, Expr_tuple rhs -> List.iter2 mcdc_var_def eq.eq_lhs rhs
  | _ -> mcdc_expr 0 eq.eq_rhs 

let mcdc_top_decl td = 
  match td.top_decl_desc with
  | Node nd -> List.iter mcdc_node_eq nd.node_eqs
  | _ -> ()


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
  List.iter mcdc_top_decl prog

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)

    
