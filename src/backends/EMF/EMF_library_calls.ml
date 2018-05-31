(** This function focuses on standard library calls: conversion functions and
    math library. It could be later extended to handle more functions. For the
    moment, modular compilation of multiple lustre sources as one output JSON is not
    considered. *)

open Lustre_types
open Machine_code_types
open Format
open EMF_common

let pp_call fmt m f outputs inputs =
  let (decl, _) = List.assoc f m.mcalls in
  if Corelang.is_imported_node decl then
    let inode = Corelang.imported_node_of_top decl in
    match inode.nodei_id, Filename.basename decl.top_decl_owner with
    | name, (("lustrec_math" | "simulink_math_fcn" | "conv") as lib) -> (
      fprintf fmt "\"kind\": \"functioncall\",@ \"name\": \"%s\",@ \"library\": \"%s\",@ "
        name lib;
      fprintf fmt "\"lhs\": [@[%a@]],@ \"args\": [@[%a@]]"
	(Utils.fprintf_list ~sep:",@ " (fun fmt v -> fprintf fmt "\"%a\"" Printers.pp_var_name v)) outputs
	pp_emf_cst_or_var_list inputs
    )
    | _ ->
       Format.eprintf "Calls to function %s in library %s are not handled yet.@."
      	 inode.nodei_id
      	 (Filename.basename decl.top_decl_owner)
      ;
      assert false
  else
    assert false (* shall not happen *)



  
  (* Local Variables: *)
  (* compile-command: "make -C ../.." *)
  (* End: *)
  
