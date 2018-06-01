(* An application that loads json provided input and produces Lustre

Usage:
lustrei -vhdl myvhdl.json 
lustrei -scade myscademodel.json 
  will produce a lustre file that can be compiled and analyzed

VHDL is handled in a double way: as a backend and as an import language
In a first step, lustrei -vhdl -print myvhdl.json shall print the VHDL model in stdout

 *)

open Vhdl_ast
open Vhdl_test
       
let _ =
(*
  (* Load model with Yojson *)
  let json = xx in

  (* Create VHDL values *)
  let vhdl : vhdl_design_t = xxxx json in

  (* Printing result *)
  Format.printf "Loaded VHDL:@.%a@." pp_vhdl_design vhdl
 *)

  let vhdl = design1 in
  Format.printf "Loaded VHDL:@.%a@." pp_vhdl_design vhdl;
  ()
