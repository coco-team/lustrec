(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
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

let version = "0.1-"^Version.number
let main_node = ref ""
let static_mem = ref true
let print_types = ref true
let print_clocks = ref true
let delay_calculus = ref true
let track_exceptions = ref true
let ansi = ref false
let check = ref false
let c_spec = ref false
let output = ref "C"
let dest_dir = ref ""
let verbose_level = ref 1
let global_inline = ref false
let witnesses = ref false

let options =
  [ "-d", Arg.Set_string dest_dir,
    "produces code in the specified directory";
    "-node", Arg.Set_string main_node, "specifies the main node";
    "-init", Arg.Set delay_calculus, "performs an initialisation analysis for Lustre nodes";
    "-dynamic", Arg.Clear static_mem, "specifies a dynamic allocation scheme for main Lustre node (default: static)";
    "-ansi", Arg.Set ansi, "specifies that generated C code is ansi C90 compliant (default is C99)";
    "-check-access", Arg.Set check, "checks at runtime that array accesses always lie within bounds (default: no check)";
    "-c-spec", Arg.Set c_spec, 
    "generates a C encoding of the specification instead of ACSL contracts and annotations. Only meaningful for the C backend";
    "-java", Arg.Unit (fun () -> output := "java"), "generates Java output instead of C";
    "-horn", Arg.Unit (fun () -> output := "horn"), "generates Horn clauses encoding output instead of C";
    "-inline", Arg.Set global_inline, "inline all node calls (require a main node)";
    "-witnesses", Arg.Set witnesses, "enable production of witnesses during compilation";
    "-print_types", Arg.Set print_types, "prints node types";
    "-print_clocks", Arg.Set print_clocks, "prints node clocks";
    "-verbose", Arg.Set_int verbose_level, " changes verbose level <default: 1>";
    "-version", Arg.Unit (fun () -> print_endline version), " displays the version";]


(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
