(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT - LIFL             *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *) 
(*  This file was originally from the Prelude compiler              *)
(*                                                                  *) 
(********************************************************************)

(** Base types and predefined operator types. *)

  
module Make (T: Types.S) =
struct
  (* module T = Types.Make (BT) *)
  module BT = T.BasicT
  include T 
  
  let type_int = new_ty type_int
  let type_real = new_ty type_real
  let type_bool = new_ty type_bool
  let type_string = new_ty type_string
  let type_clock ty = new_ty (Tclock ty)
  let type_const tname = new_ty (Tconst tname)
  let type_enum taglist = new_ty (Tenum taglist)
  let type_struct fieldlist = new_ty (Tstruct fieldlist)
  let type_tuple tl = new_ty (Ttuple tl)
  let type_arrow ty1 ty2 = new_ty (Tarrow (ty1, ty2))
  let type_array d ty = new_ty (Tarray (d, ty))
  let type_static d ty = new_ty (Tstatic (d, ty))
    
  let type_unary_bool_op =
    new_ty (Tarrow (type_bool, type_bool))

  let type_unary_poly_op =
    let univ = new_univar () in
    type_arrow univ univ

  let type_bin_int_op =
    type_arrow (type_tuple [type_int;type_int]) type_int

  let type_bin_bool_op =
    type_arrow (type_tuple [type_bool;type_bool]) type_bool

  let type_ite_op =
    let univ = new_univar () in
    type_arrow (type_tuple [type_bool;univ;univ]) univ

  let type_bin_poly_op =
    let univ = new_univar () in
    type_arrow (type_tuple [univ;univ]) univ

  let type_bin_comp_op =
    let univ = new_univar () in
    new_ty (Tarrow (new_ty (Ttuple [univ;univ]), type_bool))

  let type_univ_bool_univ =
    let univ = new_univar () in
    type_arrow (type_tuple [univ;type_bool]) univ

  let type_bool_univ3 =
    let univ = new_univar () in
    type_arrow (type_tuple [type_bool;univ;univ]) univ

  let type_access =
    let d = Dimension.mkdim Location.dummy_loc Dimension.Dunivar in
    let d' = Dimension.mkdim Location.dummy_loc Dimension.Dunivar in
    let univ = new_univar () in
    type_arrow (type_tuple [type_array d univ; type_static d' type_int]) univ

  let type_power =
    let d = Dimension.mkdim Location.dummy_loc Dimension.Dunivar in
    let univ = new_univar () in
    type_arrow (type_tuple [univ; type_static d type_int]) (type_array d univ)
end


(* module BaseBuilder = *)
(* struct *)
(*   let type_int_builder = Tbasic Basic.Tint *)
(*   let type_real_builder = Tbasic Basic.Treal *)
(*   let type_bool_builder = Tbasic Basic.Tbool *)
(*   let type_string_builder = Tbasic Basic.Tstring *)
(* end *)
  
module Main = Make (Types.Main)
include Main

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
