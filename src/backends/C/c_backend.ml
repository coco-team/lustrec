(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2013, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 * Copyright (C) 2012-2013, INPT, Toulouse, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

(* This module is used for the lustre to C compiler *)

open Format
(********************************************************************************************)
(*                         Translation function                                             *)
(********************************************************************************************)

let makefile_opt print basename dependencies makefile_fmt machines =
  (* If a main node is identified, generate a main target for it *)
  match !Options.main_node with
  | "" ->  ()
  | main_node -> (
    match Machine_code.get_machine_opt main_node machines with
    | None -> Format.eprintf "Unable to find a main node named %s@.@?" main_node; ()
    | Some _ -> print basename !Options.main_node dependencies makefile_fmt
  )


let gen_files funs basename prog machines dependencies header_file source_lib_file source_main_file makefile_file machines =
  let header_out = open_out header_file in
  let header_fmt = formatter_of_out_channel header_out in
  let source_lib_out = open_out source_lib_file in
  let source_lib_fmt = formatter_of_out_channel source_lib_out in
  
  let print_header, print_lib_c, print_main_c, print_makefile = funs in
  (* Generating H file *)
  print_header header_fmt basename prog machines;
  
  (* Generating Lib C file *)
  print_lib_c source_lib_fmt basename prog machines dependencies;

  match !Options.main_node with
  | "" ->  () (* No main node: we do not genenrate main nor makefile *)
  | main_node -> (
    match Machine_code.get_machine_opt main_node machines with
    | None -> Format.eprintf "Unable to find a main node named %s@.@?" main_node; assert false
    | Some m -> begin
      let source_main_out = open_out source_main_file in
      let source_main_fmt = formatter_of_out_channel source_main_out in
      let makefile_out = open_out makefile_file in
      let makefile_fmt = formatter_of_out_channel makefile_out in

      (* Generating Main C file *)
      print_main_c source_main_fmt m basename prog machines dependencies;
      
      (* Generating Makefile *)
     print_makefile basename main_node dependencies makefile_fmt
    end
  )
    


let translate_to_c header source_lib source_main makefile basename prog machines dependencies =

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
        
    let funs = Header.print_header, Source.print_lib_c, SourceMain.print_main_c, Makefile.print_makefile in
    gen_files funs basename prog machines dependencies header source_lib source_main makefile machines

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
        
    let funs = Header.print_header, Source.print_lib_c, SourceMain.print_main_c, Makefile.print_makefile in
    gen_files funs basename prog machines dependencies header source_lib source_main makefile machines

  end
  | "c" -> begin
    assert false (* not implemented yet *)
  end
  | _ -> assert false
(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
