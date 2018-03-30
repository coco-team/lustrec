(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT - LIFL             *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

(* Parts of this file come from the Prelude compiler *)

(*open LustreSpec*)
open Type_predef
open Clock_predef
open Delay_predef
open Dimension

module TE = Env

let static_op ty =
  type_static (mkdim_var ()) ty

let type_env =
  List.fold_left
    (fun env (op, op_type) -> TE.add_value env op op_type)
    TE.initial
    [
      "true", (static_op type_bool);
      "false", (static_op type_bool);
      "+", (static_op type_bin_poly_op);
      "uminus", (static_op type_unary_poly_op);
      "-", (static_op type_bin_poly_op);
      "*", (static_op type_bin_poly_op);
      "/", (static_op type_bin_poly_op);
      "mod", (static_op type_bin_int_op);
      "&&", (static_op type_bin_bool_op);
      "||", (static_op type_bin_bool_op);
      "xor", (static_op type_bin_bool_op);
      "equi", (static_op type_bin_bool_op);
      "impl", (static_op type_bin_bool_op);
      "<", (static_op type_bin_comp_op);
      "<=", (static_op type_bin_comp_op);
      ">", (static_op type_bin_comp_op);
      ">=", (static_op type_bin_comp_op);
      "!=", (static_op type_bin_comp_op);
      "=", (static_op type_bin_comp_op);
      "not", (static_op type_unary_bool_op)
]

module CE = Env

let clock_env =
  let init_env = CE.initial in
  let env' =
    List.fold_right (fun op env -> CE.add_value env op ck_nullary_univ)
      ["true"; "false"] init_env in
  let env' =
    List.fold_right (fun op env -> CE.add_value env op ck_unary_univ)
      ["uminus"; "not"] env' in
  let env' =
    List.fold_right (fun op env -> CE.add_value env op ck_bin_univ)
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "equi"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
  env'

module DE = Env

let delay_env =
  let init_env = DE.initial in
  let env' =
    List.fold_right (fun op env -> DE.add_value env op delay_nullary_poly_op)
      ["true"; "false"] init_env in
  let env' =
    List.fold_right (fun op env -> DE.add_value env op delay_unary_poly_op)
      ["uminus"; "not"] env' in
  let env' =
    List.fold_right (fun op env -> DE.add_value env op delay_binary_poly_op)
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "equi"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
  let env' =
    List.fold_right (fun op env -> DE.add_value env op delay_ternary_poly_op)
      [] env' in
  env'

module VE = Env

let eval_env =
  let defs = [
    "uminus", (function [Dint a] -> Dint (-a)           | _ -> assert false);
    "not", (function [Dbool b] -> Dbool (not b)         | _ -> assert false);
    "+", (function [Dint a; Dint b] -> Dint (a+b)       | _ -> assert false);
    "-", (function [Dint a; Dint b] -> Dint (a-b)       | _ -> assert false);
    "*", (function [Dint a; Dint b] -> Dint (a*b)       | _ -> assert false);
    "/", (function [Dint a; Dint b] -> Dint (a/b)       | _ -> assert false);
    "mod", (function [Dint a; Dint b] -> Dint (a mod b) | _ -> assert false);
    "&&", (function [Dbool a; Dbool b] -> Dbool (a&&b)  | _ -> assert false);
    "||", (function [Dbool a; Dbool b] -> Dbool (a||b)  | _ -> assert false);
    "xor", (function [Dbool a; Dbool b] -> Dbool (a<>b) | _ -> assert false);
    "equi", (function [Dbool a; Dbool b] -> Dbool (a=b) | _ -> assert false);
    "impl", (function [Dbool a; Dbool b] -> Dbool (a<=b)| _ -> assert false);
    "<", (function [Dint a; Dint b] -> Dbool (a<b)      | _ -> assert false);
    ">", (function [Dint a; Dint b] -> Dbool (a>b)      | _ -> assert false);
    "<=", (function [Dint a; Dint b] -> Dbool (a<=b)    | _ -> assert false);
    ">=", (function [Dint a; Dint b] -> Dbool (a>=b)    | _ -> assert false);
    "!=", (function [a; b] -> Dbool (a<>b)              | _ -> assert false);
    "=", (function [a; b] -> Dbool (a=b)                | _ -> assert false);
  ]
  in
  List.fold_left
    (fun env (op, op_eval) -> VE.add_value env op op_eval)
    VE.initial
    defs

let arith_funs = ["+";"-";"*";"/";"mod"; "uminus"]
let bool_funs  = ["&&";"||";"xor";"equi";"impl"; "not"]
let rel_funs   = ["<";">";"<=";">=";"!=";"="]

let internal_funs = arith_funs@bool_funs@rel_funs
 
let rec is_internal_fun x targs =
(*Format.eprintf "is_internal_fun %s %a@." x Types.print_ty (List.hd targs);*)
  match targs with
  | []                              -> assert false
  | t::_ when Types.is_real_type t  -> List.mem x internal_funs && not !Options.mpfr 
  | t::_ when Types.is_array_type t -> is_internal_fun x [Types.array_element_type t]
  | _                               -> List.mem x internal_funs

let is_expr_internal_fun expr =
  let open Lustre_types in
  match expr.expr_desc with
  | Expr_appl (f, e, _) -> is_internal_fun f (Types.type_list_of_type e.expr_type)
  | _                   -> assert false

let is_value_internal_fun v =
  let open Machine_code_types in
  match v.value_desc with
  | Fun (f, vl) -> is_internal_fun f (List.map (fun v -> v.value_type) vl)
  | _           -> assert false

let is_numeric_operator x =
  List.mem x arith_funs

let is_homomorphic_fun x =
  List.mem x internal_funs

let is_stateless_fun x =
  List.mem x internal_funs


(* let pp_java i pp_val fmt vl = *)
(*   match i, vl with *)
(*   (\*  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(%a?(%a):(%a))" pp_val v1 pp_val v2 pp_val v3 *\) *)
(*     | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v *)
(*     | "not", [v] -> Format.fprintf fmt "(!%a)" pp_val v *)
(*     | "impl", [v1; v2] -> Format.fprintf fmt "(!%a || %a)" pp_val v1 pp_val v2 *)
(*     | "=", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 *)
(*     | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2 *)
(*     | "equi", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 *)
(*     | "xor", [v1; v2] -> Format.fprintf fmt "(%a != %a)" pp_val v1 pp_val v2 *)
(*     | _, [v1; v2] -> Format.fprintf fmt "(%a %s %a)" pp_val v1 i pp_val v2 *)
(*     | _ -> (Format.eprintf "internal error: Basic_library.pp_java %s@." i; assert false) *)


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
