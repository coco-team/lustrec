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

open Format
open Utils
(********************************************************************************************)
(*                         Translation function                                             *)
(********************************************************************************************)


let gen_files funs basename prog machines dependencies  machines target =

  let source_dir = target ^ "/" ^ "src" in
  mk_dir source_dir;
  let source_lib_file = source_dir ^ "/" ^ "main.rs" in
  let source_lib_out = open_out source_lib_file in
  let source_lib_fmt = formatter_of_out_channel source_lib_out in

  let print_lib_rust, print_main_rust, print_cargo = funs in

  (* Generating Lib Rust file *)
  print_lib_rust source_lib_fmt basename prog machines dependencies;

  (* close_out source_lib_out; *)

  match !Options.main_node with
  | "" ->  () (* No main node: we do not genenrate main nor makefile *)
  | main_node -> (
    match Machine_code.get_machine_opt main_node machines with
    | None -> Format.eprintf "Unable to find a main node named %s@.@?" main_node; assert false
    | Some m -> begin
      (* let source_main_out = open_out source_main_file in *)
      (* let source_main_fmt = formatter_of_out_channel source_main_out in *)

      (* (\* Generating Main C file *\) *)
      (* print_main_horn source_main_fmt m basename prog machines dependencies; *)

        (* Generating Toml *)

     print_cargo false basename target;

     (* close_out source_main_out; *)


    end
  )





let translate_to_rust destname basename prog machines dependencies =


  let module SourceMod = Rust_backend_src.EmptyMod in
  let module Source = Rust_backend_src.Main (SourceMod) in

  let module SourceMainMod = Rust_backend_main.EmptyMod in
  let module SourceMain = Rust_backend_main.Main (SourceMainMod) in

  let module CargoMod = Cargo.EmptyMod in
  let module Toml = Cargo.Main (CargoMod) in

  let target = basename ^ "_rust" in
  mk_dir target;


  let funs =
    Source.print_lib_rust,
    SourceMain.print_main_rust,
    Toml.print_cargo
  in
  gen_files funs basename prog machines dependencies machines target


(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
