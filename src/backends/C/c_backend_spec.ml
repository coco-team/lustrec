(**************************************************************************)
(*     Printing spec for c *)

(**************************************************************************)


let pp_econst fmt c = 
  match c with
    | EConst_int i -> pp_print_int fmt i
    | EConst_real r -> pp_print_string fmt r
    | EConst_float r -> pp_print_float fmt r
    | EConst_bool b -> pp_print_bool fmt b
    | EConst_string s -> pp_print_string fmt ("\"" ^ s ^ "\"")

let rec pp_eexpr is_output fmt eexpr = 
  let pp_eexpr = pp_eexpr is_output in
  match eexpr.eexpr_desc with
    | EExpr_const c -> pp_econst fmt c
    | EExpr_ident id -> 
      if is_output id then pp_print_string fmt ("*" ^ id) else pp_print_string fmt id
    | EExpr_tuple el -> Utils.fprintf_list ~sep:"," pp_eexpr fmt el
    | EExpr_arrow (e1, e2) -> fprintf fmt "%a -> %a" pp_eexpr e1 pp_eexpr e2
    | EExpr_fby (e1, e2) -> fprintf fmt "%a fby %a" pp_eexpr e1 pp_eexpr e2
    (* | EExpr_concat (e1, e2) -> fprintf fmt "%a::%a" pp_eexpr e1 pp_eexpr e2 *)
    (* | EExpr_tail e -> fprintf fmt "tail %a" pp_eexpr e *)
    | EExpr_pre e -> fprintf fmt "pre %a" pp_eexpr e
    | EExpr_when (e, id) -> fprintf fmt "%a when %s" pp_eexpr e id
    | EExpr_merge (id, e1, e2) -> 
      fprintf fmt "merge (%s, %a, %a)" id pp_eexpr e1 pp_eexpr e2
    | EExpr_appl (id, e, r) -> pp_eapp is_output fmt id e r
    | EExpr_forall (vars, e) -> fprintf fmt "forall %a; %a" Printers.pp_node_args vars pp_eexpr e 
    | EExpr_exists (vars, e) -> fprintf fmt "exists %a; %a" Printers.pp_node_args vars pp_eexpr e 


    (* | EExpr_whennot _ *)
    (* | EExpr_uclock _ *)
    (* | EExpr_dclock _ *)
    (* | EExpr_phclock _ -> assert false *)
and pp_eapp is_output fmt id e r =
  let pp_eexpr = pp_eexpr is_output in
  match r with
  | None ->
    (match id, e.eexpr_desc with
    | "+", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a + %a)" pp_eexpr e1 pp_eexpr e2
    | "uminus", _ -> fprintf fmt "(- %a)" pp_eexpr e
    | "-", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a - %a)" pp_eexpr e1 pp_eexpr e2
    | "*", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a * %a)" pp_eexpr e1 pp_eexpr e2
    | "/", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a / %a)" pp_eexpr e1 pp_eexpr e2
    | "mod", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a mod %a)" pp_eexpr e1 pp_eexpr e2
    | "&&", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a && %a)" pp_eexpr e1 pp_eexpr e2
    | "||", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a || %a)" pp_eexpr e1 pp_eexpr e2
    | "xor", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a ^^ %a)" pp_eexpr e1 pp_eexpr e2
    | "impl", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a ==> %a)" pp_eexpr e1 pp_eexpr e2
    | "<", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a < %a)" pp_eexpr e1 pp_eexpr e2
    | "<=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a <= %a)" pp_eexpr e1 pp_eexpr e2
    | ">", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a > %a)" pp_eexpr e1 pp_eexpr e2
    | ">=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a >= %a)" pp_eexpr e1 pp_eexpr e2
    | "!=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a != %a)" pp_eexpr e1 pp_eexpr e2
    | "=", EExpr_tuple([e1;e2]) -> fprintf fmt "(%a == %a)" pp_eexpr e1 pp_eexpr e2
    | "not", _ -> fprintf fmt "(! %a)" pp_eexpr e
    | "ite", EExpr_tuple([e1;e2;e3]) -> fprintf fmt "(if %a then %a else %a)" pp_eexpr e1 pp_eexpr e2 pp_eexpr e3
    | _ -> fprintf fmt "%s (%a)" id pp_eexpr e)
  | Some x -> fprintf fmt "%s (%a) every %s" id pp_eexpr e x 

let pp_ensures is_output fmt e =
  match e with
    | EnsuresExpr e -> fprintf fmt "ensures %a;@ " (pp_eexpr is_output) e
    | SpecObserverNode (name, args) -> fprintf fmt "observer %s (%a);@ " name (Utils.fprintf_list ~sep:", " (pp_eexpr is_output)) args

let pp_acsl_spec outputs fmt spec =
  let is_output = fun oid -> List.exists (fun v -> v.var_id = oid) outputs in
  let pp_eexpr = pp_eexpr is_output in
  fprintf fmt "@[<v 2>/*@@ ";
  Utils.fprintf_list ~sep:"" (fun fmt r -> fprintf fmt "requires %a;@ " pp_eexpr r) fmt spec.requires;
  Utils.fprintf_list ~sep:"" (pp_ensures is_output) fmt spec.ensures;
  fprintf fmt "@ ";
  (* fprintf fmt "assigns *self%t%a;@ "  *)
  (*   (fun fmt -> if List.length outputs > 0 then fprintf fmt ", ") *)
  (*   (fprintf_list ~sep:"," (fun fmt v -> fprintf fmt "*%s" v.var_id)) outputs; *)
  Utils.fprintf_list ~sep:"@ " (fun fmt (name, assumes, requires) -> 
    fprintf fmt "behavior %s:@[@ %a@ %a@]" 
      name
      (Utils.fprintf_list ~sep:"@ " (fun fmt r -> fprintf fmt "assumes %a;" pp_eexpr r)) assumes
      (Utils.fprintf_list ~sep:"@ " (pp_ensures is_output)) requires
  ) fmt spec.behaviors;
  fprintf fmt "@]@ */@.";
  ()




let print_machine_decl_prefix fmt m =
   (* Print specification if any *)
   (match m.mspec with
  | None -> ()
  | Some spec -> 
    pp_acsl_spec m.mstep.step_outputs fmt spec
  )

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
