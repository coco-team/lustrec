(* An application that loads json provided input and produces Lustre

Usage:
lustrei -vhdl myvhdl.json 
lustrei -scade myscademodel.json 
  will produce a lustre file that can be compiled and analyzed

VHDL is handled in a double way: as a backend and as an import language
In a first step, lustrei -vhdl -print myvhdl.json shall print the VHDL model in stdout

 *)
(*
open Vhdl_ast
open Vhdl_test
  *)
open Yojson.Safe
open Vhdl_deriving_yojson
open Vhdl_json_lib
open Printf

let _ =
(*
  (* Load model with Yojson *)
  let json = xx in

  (* Create VHDL values *)
  let vhdl : vhdl_design_t = xxxx json in

  (* Printing result *)
  Format.printf "Loaded VHDL:@.%a@." pp_vhdl_design vhdl
 *)

  let vhdl_json = from_file Sys.argv.(1) in
  Format.printf "Original file:\n%s\n\n" (pretty_to_string vhdl_json);

  (*let vhdl = design1 in
  Format.printf "Loaded VHDL:@.%a@." pp_vhdl_design vhdl;*)

  let vhdl1_json = vhdl_json |> 
                   prune_str "TOKEN" |>
                   prune_str "IDENTIFIER" |>
                   prune_str "SUBTYPE_INDICATION" |>
                   prune_null_assoc |>
                   to_list_content_str "DESIGN_UNIT" |>
                   to_list_content_str "INTERFACE_VARIABLE_DECLARATION" |>
                   flatten_ivd |>
                   flatten_numeric_literal |>
                   to_list_str "ENTITY_DECLARATION" |>
                   to_list_str "ARCHITECTURE_BODY" |>
                   to_list_str "PACKAGE_DECLARATION" in
  Format.printf "Preprocessed json:\n";
  Format.printf "%s\n\n" (pretty_to_string vhdl1_json);
(*  List.iter (Format.printf "%s\n") (print_depth vhdl1_json 7 ""); *)

  to_file (Sys.argv.(1)^".out.json") vhdl1_json;

(*
  let typ = {name = "type"; definition = (Some (Range (Some "toto", 7, 0)))} in
  Format.printf "\nModel to string\n%s\n\n" (pretty_to_string (vhdl_subtype_indication_t_to_yojson typ));

  let elem = "[\"SUBTYPE_DECLARATION\", {\"name\": \"byte\", \"typ\": { \"name\": \"bit_vector\", \"definition\": [ \"RANGE_WITH_DIRECTION\", \"downto\", 7, 0 ]}}]" in
  match vhdl_definition_t_of_yojson (from_string elem) with
    Ok x -> Format.printf "\nString to string\n%s\n\n" (pretty_to_string (vhdl_definition_t_to_yojson x));
  | Error e -> Format.printf "Error: %s\n" e;
*)

  match vhdl_file_t_of_yojson vhdl1_json with
    Ok x -> Format.printf "Parsed VHDL: \n%s\n" (pretty_to_string (vhdl_file_t_to_yojson x))
  | Error e -> Format.printf "Error: %s\n" e;
