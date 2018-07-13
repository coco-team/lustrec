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

   This is a modified version that handles reset and automaton
*)

open Format
open Lustre_types
open Corelang
open Machine_code_types

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
    if !Options.horn_cex then(
      cex_computation machines fmt node machine;
      get_cex machines fmt node machine)
    else (
      collecting_semantics machines fmt node machine;
      check_prop machines fmt node machine;
    )
end


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

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
	  fprintf fmt "(declare-datatypes () ((%s %a)));@.@."
	    var
	    (Utils.fprintf_list ~sep:" " pp_print_string) tl
	| _ -> assert false
      )
      | _ -> assert false
      )
    | _        -> ()) type_table




let print_dep fmt prog =
  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>.. extracting Horn libraries@,");
  fprintf fmt "; Statically linked libraries@,";
  let dependencies = Corelang.get_dependencies prog in
  List.iter
    (fun dep ->
      let (local, s) = Corelang.dependency_of_top dep in
      let basename = (Options_management.name_dependency (local, s)) ^ ".smt2" in
      Log.report ~level:1 (fun fmt -> Format.fprintf fmt "@[<v 0> Horn Library %s@," basename);
      let horn = load_file basename in
      fprintf fmt "@.%s@." (horn);
    )
    dependencies

let check_sfunction mannot =
 (*Check if its an sfunction*)
  match mannot with
    [] -> false
  | [x] ->
     begin
       match x.annots with
         [] -> false
        |[(key,va)] ->
          begin
            match key with
              [] -> false
            | [x]  -> x == "c_code" || x =="matlab"
            | _ -> false
          end
        |(_,_)::_ -> false
     end
  | _::_ -> false

let preprocess machines =
  List.fold_right (fun m res ->
    if List.mem m.mname.node_id registered_keywords then
      { m with mname  = { m.mname with node_id = protect_kwd m.mname.node_id }}::res
       else
	m :: res
  ) machines []
     
let translate fmt basename prog machines=
  let machines = preprocess machines in
  (* We print typedef *)
  print_dep fmt prog; (*print static library e.g. math*)
  print_type_definitions fmt;
  (*List.iter (print_machine machines fmt) (List.rev machines);*)
  List.iter(fun m ->
      let is_sfunction = check_sfunction m.mannot in
      if is_sfunction then(
        Log.report ~level:1 (fun fmt -> fprintf fmt ".. detected sfunction: %s@," m.mname.node_id);
        print_sfunction machines fmt m
      ) else (
         print_machine machines fmt m)
         )
           (List.rev machines);
  main_print machines fmt

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
