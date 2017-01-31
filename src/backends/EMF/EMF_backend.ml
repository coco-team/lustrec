open LustreSpec
open Format
open Utils

let pp_var_string fmt v = fprintf fmt "\"%s\"" v
let pp_var_name fmt v = fprintf fmt "\"%a\"" Printers.pp_var_name v

let pp_node_args = fprintf_list ~sep:", " pp_var_name

let pp_expr vars fmt expr =
  (* simple function to extract the element id in the list. Starts from 1. *)
  let rec get_idx x l =
    match l with
    | hd::tl -> if hd = x then 1 else 1+(get_idx x tl)
    | [] -> assert false
  in
  let rec pp_expr fmt expr =
    match expr.expr_desc with
    | Expr_const c -> Printers.pp_const fmt c
    | Expr_ident id ->
       if List.mem id vars then
	 Format.fprintf fmt "u(%i)" (get_idx id vars)
       else
	 assert false (* impossible to find element id in var list *)
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp_expr c pp_expr t pp_expr e
    | Expr_arrow (e1, e2) ->(
      match e1.expr_desc, e2.expr_desc with
      | Expr_const c1, Expr_const c2 -> if c1 = Corelang.const_of_bool true && c2 = Corelang.const_of_bool false then fprintf fmt "STEP" else assert false (* only handle true -> false *)
      | _ -> assert false (* only handle true -> false *)
    )
    | Expr_fby (e1, e2) -> assert false (* not covered yet *)
    | Expr_pre e -> fprintf fmt "UNITDELAY" 
    | Expr_when (e, id, l) -> assert false (* clocked based expressions are not handled yet *)
    | Expr_merge (id, hl) -> assert false (* clocked based expressions are not handled yet *)
    | Expr_appl (id, e, r) -> pp_app fmt id e r

  and pp_tuple fmt el =
    fprintf_list ~sep:"," pp_expr fmt el

  and pp_app fmt id e r =
    match r with
    | None -> pp_call fmt id e
    | Some c -> assert false (* clocked based expressions are not handled yet *)

  and pp_call fmt id e =
    match id, e.expr_desc with
    | "+", Expr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
    | "uminus", _ -> fprintf fmt "(- %a)" pp_expr e
    | "-", Expr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
    | "*", Expr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
    | "/", Expr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_expr e1 pp_expr e2
    | "mod", Expr_tuple([e1;e2]) -> fprintf fmt "mod (%a, %a)" pp_expr e1 pp_expr e2
    | "&&", Expr_tuple([e1;e2]) -> fprintf fmt "(%a & %a)" pp_expr e1 pp_expr e2
    | "||", Expr_tuple([e1;e2]) -> fprintf fmt "(%a | %a)" pp_expr e1 pp_expr e2
    | "xor", Expr_tuple([e1;e2]) -> fprintf fmt "xor (%a, %a)" pp_expr e1 pp_expr e2
    | "impl", Expr_tuple([e1;e2]) -> fprintf fmt "((~%a) | %a)" pp_expr e1 pp_expr e2
    | "<", Expr_tuple([e1;e2]) -> fprintf fmt "(%a < %a)" pp_expr e1 pp_expr e2
    | "<=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a <= %a)" pp_expr e1 pp_expr e2
    | ">", Expr_tuple([e1;e2]) -> fprintf fmt "(%a > %a)" pp_expr e1 pp_expr e2
    | ">=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a >= %a)" pp_expr e1 pp_expr e2
    | "!=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a != %a)" pp_expr e1 pp_expr e2
    | "=", Expr_tuple([e1;e2]) -> fprintf fmt "(%a = %a)" pp_expr e1 pp_expr e2
    | "not", _ -> fprintf fmt "(~%a)" pp_expr e
    | _, Expr_tuple _ -> fprintf fmt "%s %a" id pp_expr e
    | _ -> fprintf fmt "%s (%a)" id pp_expr e

  in
  pp_expr fmt expr

(*
let rec translate_expr expr vars =
  match expr with
    match expr.expr_desc with
    | Expr_const _ -> expr, vars
    | Expr_ident id -> if List.exists id Format.fprintf fmt "%s" id
    | Expr_array a -> fprintf fmt "[%a]" pp_tuple a
    | Expr_access (a, d) -> fprintf fmt "%a[%a]" pp_expr a Dimension.pp_dimension d
    | Expr_power (a, d) -> fprintf fmt "(%a^%a)" pp_expr a Dimension.pp_dimension d
    | Expr_tuple el -> fprintf fmt "(%a)" pp_tuple el
    | Expr_ite (c, t, e) -> fprintf fmt "(if %a then %a else %a)" pp_expr c pp_expr t pp_expr e
    | Expr_arrow (e1, e2) -> fprintf fmt "(%a -> %a)" pp_expr e1 pp_expr e2
    | Expr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_expr e1 pp_expr e2
    | Expr_pre e -> fprintf fmt "pre %a" pp_expr e
    | Expr_when (e, id, l) -> fprintf fmt "%a when %s(%s)" pp_expr e l id
    | Expr_merge (id, hl) -> 
      fprintf fmt "merge %s %a" id pp_handlers hl
    | Expr_appl (id, e, r) -> pp_app fmt id e r
*)
    
let pp_stmt fmt stmt =
  match stmt with
  | Eq eq -> (
    match eq.eq_lhs with
      [var] -> (
     (* first, we extract the expression and associated variables *)
	let vars = Utils.ISet.elements (Corelang.get_expr_vars eq.eq_rhs) in
	
	fprintf fmt "\"%s\": @[<v 2>{ \"expr\": \"%a\";@ \"vars\": [%a] @]}"
	  var
	  (pp_expr vars) eq.eq_rhs (* todo_pp_expr expr *)
	  (fprintf_list ~sep:", " pp_var_string) vars
      )
    | _ -> assert false (* should not happen for input of EMF backend (cocospec generated nodes *)
  )
  | _ -> assert false (* should not happen with EMF backend *)

let pp_node fmt nd =
  fprintf fmt "@[<v 2>\"%s\": {@ \"inputs\": [%a];@ \"outputs\": [%a];@ "
    nd.node_id
    pp_node_args nd.node_inputs
    pp_node_args nd.node_outputs;
  fprintf fmt "\"exprs\": {@[<v 1> %a@]@ }"
    (fprintf_list ~sep:";@ " pp_stmt ) nd.node_stmts;
  fprintf fmt "@]@ }"
    
let pp_decl fmt decl =
  match decl.top_decl_desc with
  | Node nd -> fprintf fmt "%a@ " pp_node nd
  | ImportedNode _ 
  | Const _
  | Open _ 
  | TypeDef _ -> eprintf "should not happen in EMF backend"


let translate fmt prog =
  fprintf fmt "@[<v 0>{@ ";
  fprintf_list ~sep:"@ " pp_decl fmt prog;
  fprintf fmt "@ @]}"

