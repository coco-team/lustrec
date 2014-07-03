open Format
open LustreSpec
open Machine_code
open C_backend_common
open Utils

(**************************************************************************)
(*     Printing spec for c *)

(**************************************************************************)
(* OLD STUFF ???


let pp_acsl_type var fmt t =
  let rec aux t pp_suffix =
  match (Types.repr t).Types.tdesc with
  | Types.Tclock t'       -> aux t' pp_suffix
  | Types.Tbool           -> fprintf fmt "int %s%a" var pp_suffix ()
  | Types.Treal           -> fprintf fmt "real %s%a" var pp_suffix ()
  | Types.Tint            -> fprintf fmt "int %s%a" var pp_suffix ()
  | Types.Tarray (d, t')  ->
    let pp_suffix' fmt () = fprintf fmt "%a[%a]" pp_suffix () pp_c_dimension d in
    aux t' pp_suffix'
  (* | Types.Tstatic (_, t') -> fprintf fmt "const "; aux t' pp_suffix *)
  (* | Types.Tconst ty       -> fprintf fmt "%s %s" ty var *)
  (* | Types.Tarrow (_, _)   -> fprintf fmt "void (\*%s)()" var *)
  | _                     -> eprintf "internal error: pp_acsl_type %a@." Types.print_ty t; assert false
  in aux t (fun fmt () -> ())

let pp_acsl_var_decl fmt id =
  pp_acsl_type id.var_id fmt id.var_type


let rec pp_eexpr is_output fmt eexpr = 
  let pp_eexpr = pp_eexpr is_output in
  match eexpr.eexpr_desc with
    | EExpr_const c -> Printers.pp_econst fmt c
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

  *)

(**************************************************************************)
(*                              MAKEFILE                                  *)
(**************************************************************************)

let makefile_targets fmt basename nodename dependencies =
  fprintf fmt "FRAMACEACSL=`frama-c -print-share-path`/e-acsl@.";
  (* EACSL version of library file . c *)
  fprintf fmt "%s_eacsl.c: %s.c %s.h@." basename basename basename;
  fprintf fmt 
    "\tframa-c -e-acsl-full-mmodel -machdep x86_64 -e-acsl %s.c -then-on e-acsl -print -ocode %s_eacsl.c@." 
    basename basename; 
  fprintf fmt "@.";
  fprintf fmt "@.";

  (* EACSL version of library file . c + main .c  *)
  fprintf fmt "%s_main_eacsl.c: %s.c %s.h %s_main.c@." basename basename basename basename;
  fprintf fmt "\tframa-c -e-acsl-full-mmodel -machdep x86_64 -e-acsl %s.c %s_main.c -then-on e-acsl -print -ocode %s_main_eacsl.i@." 
    basename basename basename; 
  (* Ugly hack to deal with eacsl bugs *)
  fprintf fmt "\tgrep -v _fc_stdout %s_main_eacsl.i > %s_main_eacsl.c" basename basename;
  fprintf fmt "@.";
  fprintf fmt "@.";

  (* EACSL version of binary *)
  fprintf fmt "%s_main_eacsl: %s_main_eacsl.c@." basename basename;
  fprintf fmt "\t${GCC} -Wno-attributes -I${INC} -I. -c %s_main_eacsl.c@." basename; (* compiling instrumented lib + main *)
  C_backend_makefile.fprintf_dependencies fmt dependencies; 
  fprintf fmt "\t${GCC} -Wno-attributes -o %s_main_eacsl io_frontend.o %a %s %s_main_eacsl.o %a@." 
    basename 
    (Utils.fprintf_list ~sep:" " (fun fmt (s, _, _) -> Format.fprintf fmt "%s.o" s)) 
    (C_backend_makefile.compiled_dependencies dependencies)
    ("${FRAMACEACSL}/e_acsl.c " 
     ^ "${FRAMACEACSL}/memory_model/e_acsl_bittree.c " 
     ^ "${FRAMACEACSL}/memory_model/e_acsl_mmodel.c")
    basename 
    (Utils.fprintf_list ~sep:" " (fun fmt lib -> fprintf fmt "-l%s" lib)) 
    (C_backend_makefile.lib_dependencies dependencies)
  ;
  fprintf fmt "@.";

module MakefileMod =
struct
  let other_targets = makefile_targets
    
end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
