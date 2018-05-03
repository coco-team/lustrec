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
open C_backend_mauve
(********************************************************************************************)
(*                         Translation function                                             *)
(********************************************************************************************)
(* USELESS
let makefile_opt print basename dependencies makefile_fmt machines =
  (* If a main node is identified, generate a main target for it *)
  match !Options.main_node with
  | "" ->  ()
  | main_node -> (
    match Machine_code.get_machine_opt main_node machines with
    | None -> Format.eprintf "Unable to find a main node named %s@.@?" main_node; ()
    | Some _ -> print basename !Options.main_node dependencies makefile_fmt
  )
*)

let gen_files funs basename prog machines dependencies =
  let destname = !Options.dest_dir ^ "/" ^ basename in
  
  let print_header, print_lib_c, print_main_c, print_makefile(* , print_cmake *) = funs in

  (* Generating H file *)
  let alloc_header_file = destname ^ "_alloc.h" in (* Could be changed *)
  let header_out = open_out alloc_header_file in
  let header_fmt = formatter_of_out_channel header_out in
  print_header header_fmt basename prog machines dependencies;
  close_out header_out;
  
  (* Generating Lib C file *)
  let source_lib_file = (if !Options.cpp then destname ^ ".cpp" else destname ^ ".c") in (* Could be changed *)
  let source_lib_out = open_out source_lib_file in
  let source_lib_fmt = formatter_of_out_channel source_lib_out in
  print_lib_c source_lib_fmt basename prog machines dependencies;
  close_out source_lib_out;

  (match !Options.main_node with
  | "" ->  () (* No main node: we do not generate main *)
  | main_node -> (
    match Machine_code_common.get_machine_opt main_node machines with
    | None -> begin
      Global.main_node := main_node;
      Format.eprintf "Code generation error: %a@." Error.pp_error_msg Error.Main_not_found;
      raise (Corelang.Error (Location.dummy_loc, Error.Main_not_found))
    end
    | Some m -> begin
      let source_main_file = (if !Options.cpp then destname ^ "_main.cpp" else destname ^ "_main.c") in (* Could be changed *)
      let source_main_out = open_out source_main_file in
      let source_main_fmt = formatter_of_out_channel source_main_out in

      (* Generating Main C file *)
      print_main_c source_main_fmt m basename prog machines dependencies;

      close_out source_main_out;
    end
  ));

  (match !Options.mauve with
  | "" ->  ()
  | mauve -> (
    (* looking for the main node *)
    match Machine_code_common.get_machine_opt mauve machines with
    | None -> begin
      Global.main_node := mauve;
      Format.eprintf "Code generation error: %a@." Error.pp_error_msg Error.Main_not_found;
      raise (Corelang.Error (Location.dummy_loc, Error.Main_not_found))
    end
    | Some m -> begin
      let source_mauve_file = destname ^ "_mauve.hpp" in
      let source_mauve_out = open_out source_mauve_file in
      let source_mauve_fmt = formatter_of_out_channel source_mauve_out in
      (* Header *)
      print_mauve_header source_mauve_fmt m basename prog machines dependencies;
      (* Shell *)
      print_mauve_shell source_mauve_fmt m basename prog machines dependencies;
      (* Core *)
      print_mauve_core source_mauve_fmt m basename prog machines dependencies;
      (* FSM *)
      print_mauve_fsm source_mauve_fmt m basename prog machines dependencies;

      close_out source_mauve_out;
    end
  ));


  (* Makefiles:
     - for the moment two cases
     1. Classical Makefile, only when provided with a main node
     May contain additional framac eacsl targets
     2. Cmake : 2 files
         - lustrec-basename.cmake describing all variables
         - the main CMakeLists.txt activating the first file
     - Later option 1 should be removed
  *)
  (* Case 1 *)
  (match !Options.main_node with
  | "" ->  () (* No main node: we do not generate main *)
  | main_node -> (
    let makefile_file = destname ^ ".makefile" in (* Could be changed *)
    let makefile_out = open_out makefile_file in
    let makefile_fmt = formatter_of_out_channel makefile_out in
    
    (* Generating Makefile *)
    print_makefile basename main_node dependencies makefile_fmt;
    
    close_out makefile_out
  ))(* ; *)
  
  (* (\* Case 2 *\) *)
  (* let cmake_file = "lustrec-" ^ basename ^ ".cmake" in *)
  (* let cmake_file_full_path = !Options.dest_dir ^ "/" ^ cmake_file in *)
  (* let cmake_out = open_out cmake_file_full_path in *)
  (* let cmake_fmt = formatter_of_out_channel cmake_out in *)
  (* (\* Generating Makefile *\) *)
  (* print_cmake basename main_node dependencies makefile_fmt; *)
    
  (*   close_out makefile_out *)
  

let translate_to_c basename prog machines dependencies =
  match !Options.spec with
  | "no" -> begin
    let module HeaderMod = C_backend_header.EmptyMod in
    let module SourceMod = C_backend_src.EmptyMod in
    let module SourceMainMod = C_backend_main.EmptyMod in
    let module MakefileMod = C_backend_makefile.EmptyMod in

    let module Header = C_backend_header.Main (HeaderMod) in
    let module Source = C_backend_src.Main (SourceMod) in
    let module SourceMain = C_backend_main.Main (SourceMainMod) in
    let module Makefile = C_backend_makefile.Main (MakefileMod) in
    (* let module CMakefile = C_backend_cmake.Main (MakefileMod) in *)
    
    let funs = 
      Header.print_alloc_header, 
      Source.print_lib_c, 
      SourceMain.print_main_c, 
      Makefile.print_makefile(* , *)
      (* CMakefile.print_makefile *)
    in
    gen_files funs basename prog machines dependencies 

  end
  | "acsl" -> begin

    let module HeaderMod = C_backend_header.EmptyMod in
    let module SourceMod = C_backend_src.EmptyMod in
    let module SourceMainMod = C_backend_main.EmptyMod in
    let module MakefileMod = C_backend_spec.MakefileMod in

    let module Header = C_backend_header.Main (HeaderMod) in
    let module Source = C_backend_src.Main (SourceMod) in
    let module SourceMain = C_backend_main.Main (SourceMainMod) in
    let module Makefile = C_backend_makefile.Main (MakefileMod) in
    (* let module CMakefile = C_backend_cmake.Main (MakefileMod) in *)
    
    let funs = 
      Header.print_alloc_header, 
      Source.print_lib_c,
      SourceMain.print_main_c,
      Makefile.print_makefile(* , *)
      (* CMakefile.print_makefile  *)
    in
    gen_files funs basename prog machines dependencies 

  end
  | "c" -> begin
    assert false (* not implemented yet *)
  end
  | _ -> assert false
(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
