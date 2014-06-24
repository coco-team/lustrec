(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2013, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 * Copyright (C) 2012-2013, INPT, Toulouse, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

(* This module is used for the lustre to C compiler *)

open LustreSpec
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
    List.fold_right (fun op env -> CE.add_value env op ck_unary_univ)
      ["uminus"; "not"] init_env in
  let env' = 
    List.fold_right (fun op env -> CE.add_value env op ck_bin_univ)
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "equi"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
  env'

module DE = Env

let delay_env =
  let init_env = DE.initial in
  let env' = 
    List.fold_right (fun op env -> DE.add_value env op delay_unary_poly_op)
      ["uminus"; "not"] init_env in
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

let internal_funs = ["+";"-";"*";"/";"mod";"&&";"||";"xor";"equi";"impl";"<";">";"<=";">=";"!=";"=";"uminus";"not"]

let is_internal_fun x =
  List.mem x internal_funs

let pp_c i pp_val fmt vl =
  match i, vl with
  (*  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(%a?(%a):(%a))" pp_val v1 pp_val v2 pp_val v3 *)
    | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
    | "not", [v] -> Format.fprintf fmt "(!%a)" pp_val v 
    | "impl", [v1; v2] -> Format.fprintf fmt "(!%a || %a)" pp_val v1 pp_val v2 
    | "=", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 
    | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2
    | "equi", [v1; v2] -> Format.fprintf fmt "(!%a == !%a)" pp_val v1 pp_val v2
    | "xor", [v1; v2] -> Format.fprintf fmt "(!%a != !%a)" pp_val v1 pp_val v2
    | _, [v1; v2] -> Format.fprintf fmt "(%a %s %a)" pp_val v1 i pp_val v2 
    | _ -> failwith i

let pp_java i pp_val fmt vl =
  match i, vl with
  (*  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(%a?(%a):(%a))" pp_val v1 pp_val v2 pp_val v3 *)
    | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
    | "not", [v] -> Format.fprintf fmt "(!%a)" pp_val v 
    | "impl", [v1; v2] -> Format.fprintf fmt "(!%a || %a)" pp_val v1 pp_val v2 
    | "=", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 
    | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2
    | "equi", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2
    | "xor", [v1; v2] -> Format.fprintf fmt "(%a != %a)" pp_val v1 pp_val v2
    | _, [v1; v2] -> Format.fprintf fmt "(%a %s %a)" pp_val v1 i pp_val v2 
    | _ -> assert false

let pp_horn i pp_val fmt vl = 
  match i, vl with
  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(@[<hov 2>ite %a@ %a@ %a@])" pp_val v1 pp_val v2 pp_val v3 
  | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
  | "not", [v] -> Format.fprintf fmt "(not %a)" pp_val v 
  | "=", [v1; v2] -> Format.fprintf fmt "(= %a %a)" pp_val v1 pp_val v2 
  | "&&", [v1; v2] -> Format.fprintf fmt "(and %a %a)" pp_val v1 pp_val v2 
  | "||", [v1; v2] -> Format.fprintf fmt "(or %a %a)" pp_val v1 pp_val v2 
  | "impl", [v1; v2] -> Format.fprintf fmt "(=> %a %a)" pp_val v1 pp_val v2 
  | "mod", [v1; v2] -> Format.fprintf fmt "(mod %a %a)" pp_val v1 pp_val v2
  | "equi", [v1; v2] -> Format.fprintf fmt "(%a = %a)" pp_val v1 pp_val v2
  | "xor", [v1; v2] -> Format.fprintf fmt "(%a xor %a)" pp_val v1 pp_val v2
  | "!=", [v1; v2] -> Format.fprintf fmt "(not (= %a %a))" pp_val v1 pp_val v2 
  | _, [v1; v2] -> Format.fprintf fmt "(%s %a %a)" i pp_val v1 pp_val v2 
  | _ -> assert false
(*  | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2 
  
*)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
