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


(********************************************************************************************)
(*                         Translation function                                             *)
(********************************************************************************************)


let translate_to_c header_fmt source_fmt makefile_fmt spec_fmt_opt 
                   basename prog machines dependencies =


  let module HeaderMod = C_backend_header.EmptyMod in
  let module Header = C_backend_header.Main (HeaderMod) in

  let module SourceMod = C_backend_src.EmptyMod in
  let module Source = C_backend_src.Main (SourceMod) in

  let module MakefileMod = C_backend_makefile.EmptyMod in
  let module Makefile = C_backend_makefile.Main (MakefileMod) in


  (* Generating H file *)
  Header.print_header header_fmt basename prog machines;

  (* Generating C file *)
  Source.print_c source_fmt basename prog machines dependencies;

  (* Generating Makefile *)
  (* If a main node is identified, generate a main target for it *)
  match !Options.main_node with
      | "" ->  ()
      | main_node -> (
	match Machine_code.get_machine_opt main_node machines with
	| None -> Format.eprintf "Unable to find a main node named %s@.@?" main_node; 
	  ()
	| Some _ -> Makefile.print_makefile basename !Options.main_node dependencies makefile_fmt
      )

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
