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

(* The compilation presented here was first defined in Garoche, Gurfinkel,
   Kahsai, HCSV'14.

   This is a modified version that handle reset
*)

open Format
open LustreSpec
open Corelang
open Machine_code

open Horn_backend_common
open Horn_backend_printers
open Horn_backend_collecting_sem

(*
TODO:
- gerer les traces. Ca merde pour l'instant dans le calcul des memoires sur les arrows

- gerer le reset --- DONE
- reconstruire les rechable states DONE
- reintroduire le cex/traces ... DONE
- traiter les types enum et les branchements sur ces types enum (en particulier les traitements des resets qui ont lieu dans certaines branches et pas dans d'autres )
*)

let main_print machines fmt =
if !Options.main_node <> "" then
  begin
    let node = !Options.main_node in
    let machine = get_machine machines node in
    collecting_semantics machines fmt node machine;
    check_prop machines fmt node machine;
    if !Options.horn_cex then(
      cex_computation machines fmt node machine;
      get_cex machines fmt node machine)
end

let print_type_definitions fmt =
  let cpt_type = ref 0 in
  Hashtbl.iter (fun typ decl ->
    match typ with
    | Tydec_const var ->
      (match decl.top_decl_desc with
      | TypeDef tdef -> (
	match tdef.tydef_desc with
	| Tydec_enum tl ->
	  incr cpt_type;
	  fprintf fmt "(declare-datatypes () ((%s %a));@.@."
	    var
	    (Utils.fprintf_list ~sep:" " pp_print_string) tl
	| _ -> assert false
      )
      | _ -> assert false
      )
    | _        -> ()) type_table


let translate fmt basename prog machines =
  (* We print typedef *)
  print_type_definitions fmt;
  List.iter (print_machine machines fmt) (List.rev machines);
  main_print machines fmt

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
