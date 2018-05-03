(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)


(** Base types and predefined operator types. *)
open Init

let init_zero = new_univar ()

let init_un =
  let univ = new_univar () in
  new_init Isucc(univ)

let rec init_omega =
  { init_desc = Isucc init_omega ; iid = -1 }

let is_omega init =
  let rec search path init =
 match init.init_desc with
 | Isucc init' -> List.mem init path or search (init::path) init'
 | _           -> false
  in search [] init

let init_unary_poly_op =
  let univ = new_univar () in
  new_init (Iarrow (univ, univ))

let init_pre_op =
  let univ = new_univar () in
  new_init (Iarrow (univ, new_init (Isucc univ)))

let init_arrow_op =
  let univ = new_univar () in
  new_init (Iarrow (new_init (Ituple [univ; init_un]), univ))

let init_fby_op_1 =
  let univ = new_univar () in
  new_init (Iarrow (init_zero,init_zero))

let init_fby_op_2 =
  init_pre_op

let init_bin_poly_op =
  let univ = new_univar () in
  new_init (Iarrow (new_init (Ituple [univ;univ]), univ))

let init_ter_poly_op =
  let univ = new_univar () in
  new_init (Iarrow (new_init (Ituple [univ;univ;univ]), univ))

let env =
  let init_env = Env.initial in
  let env' = 
    List.fold_right (fun op env -> Env.add_value env op init_unary_poly_op)
      ["uminus"; "not"] init_env in
  let env' = 
    List.fold_right (fun op env -> Env.add_value env op init_binary_poly_op)
      ["+"; "-"; "*"; "/"; "mod"; "&&"; "||"; "xor"; "impl"; "<"; "<="; ">"; ">="; "!="; "="] env' in
  let env' = 
    List.fold_right (fun op env -> Env.add_value env op init_ternary_poly_op)
      ["ite"] init_env in
  env'

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
