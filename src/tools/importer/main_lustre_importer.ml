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
open Vhdl_ast_utils
open Vhdl_ast_map
open Vhdl_ast
open Ppxlib_traverse_builtins
open Printf

let _ =
  (* Load model with Yojson *)
  let vhdl_json = from_file Sys.argv.(1) in

  (* Create VHDL values *)
  let vhdl = vhdl_file_t_of_yojson vhdl_json in

  (* Simplify VHDL values *)
  match vhdl with
    Ok x ->
      Format.printf "Parsed VHDL: \n%s\n" (pretty_to_string (vhdl_file_t_to_yojson x));
      let folded = replace_op_expr#vhdl_file_t x in
      Format.printf "PP VHDL: \n%s\n" (show_vhdl_file_t folded);
  | Error e -> Format.printf "Error: %s\n" e;

