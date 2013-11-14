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
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
  env'

module DE = Env

let delay_env =
  let init_env = DE.initial in
  let env' = 
    List.fold_right (fun op env -> DE.add_value env op delay_unary_poly_op)
      ["uminus"; "not"] init_env in
  let env' = 
    List.fold_right (fun op env -> DE.add_value env op delay_binary_poly_op)
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
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

let internal_funs = ["+";"-";"*";"/";"mod";"&&";"||";"xor";"impl";"<";">";"<=";">=";"!=";"=";"uminus";"not"]

let is_internal_fun x =
  List.mem x internal_funs

(*
let imported_node name inputs outputs sl spec =
  mktop_decl Location.dummy_loc
    (
      ImportedNode
	{nodei_id = name;
	 nodei_type = Types.new_var ();
	 nodei_clock = Clocks.new_var true;
	 nodei_inputs = inputs;
	 nodei_outputs = outputs;
	 nodei_stateless = sl;
	nodei_spec = spec})
    
let mk_new_var id =
  let loc = Location.dummy_loc in
  mkvar_decl loc (id, mktyp loc Tydec_any, mkclock loc Ckdec_any, false)

let _ = 
  let binary_fun id = id, [mk_new_var "x"; mk_new_var "y"], [mk_new_var "z"] in
  let unary_fun id = id, [mk_new_var "x"], [mk_new_var "y"] in
  (* All following functions are stateless *)
  let st = true in
  List.iter (fun (n,i,o) -> Hashtbl.add node_table n (imported_node n i o st None))
    (
(*("ite", [mk_new_var "g"; mk_new_var "x"; mk_new_var "y"], [mk_new_var "z"])::*)
    (List.map binary_fun
	["+";"-";"*";"/";"mod";"&&";"||";"xor";"impl";"<";">";"<=";">=";"!=";"="])
     @(List.map unary_fun ["uminus";"not"]))
*)  
let pp_c i pp_val fmt vl =
  match i, vl with
  (*  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(%a?(%a):(%a))" pp_val v1 pp_val v2 pp_val v3 *)
    | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
    | "not", [v] -> Format.fprintf fmt "(!%a)" pp_val v 
    | "impl", [v1; v2] -> Format.fprintf fmt "(!%a || %a)" pp_val v1 pp_val v2 
    | "=", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 
    | "mod", [v1; v2] -> Format.fprintf fmt "(%a %% %a)" pp_val v1 pp_val v2 
    | "xor", [v1; v2] -> Format.fprintf fmt "(%a ^ %a)" pp_val v1 pp_val v2
    | _, [v1; v2] -> Format.fprintf fmt "(%a %s %a)" pp_val v1 i pp_val v2 
    | _ -> assert false

let pp_java i pp_val fmt vl =
  match i, vl with
  (*  | "ite", [v1; v2; v3] -> Format.fprintf fmt "(%a?(%a):(%a))" pp_val v1 pp_val v2 pp_val v3 *)
    | "uminus", [v] -> Format.fprintf fmt "(- %a)" pp_val v
    | "not", [v] -> Format.fprintf fmt "(!%a)" pp_val v 
    | "impl", [v1; v2] -> Format.fprintf fmt "(!%a || %a)" pp_val v1 pp_val v2 
    | "=", [v1; v2] -> Format.fprintf fmt "(%a == %a)" pp_val v1 pp_val v2 
    | _, [v1; v2] -> Format.fprintf fmt "(%a %s %a)" pp_val v1 i pp_val v2 
    | _ -> assert false


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
