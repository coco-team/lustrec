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

(* This module is used for the lustre test generator *)

open Format
open Log

open Utils
open Lustre_types
open Compiler_common

let usage = "Usage: lustret [options] \x1b[4msource file\x1b[0m"

let extensions = [".lus"]

let pp_trace trace_filename mutation_list = 
  let trace_file = open_out trace_filename in
  let trace_fmt = formatter_of_out_channel trace_file in
  Format.fprintf trace_fmt "@[<v 2>{@ %a@ }@]"
    (fprintf_list
       ~sep:",@ "
       (fun fmt (mutation, mutation_loc, mutant_name) ->
	 Format.fprintf fmt "\"%s\": { @[<v 0>%a,@ %a@ }@]" 
	   mutant_name
	   Mutation.print_directive_json mutation
	   Mutation.print_loc_json mutation_loc
       ))
    mutation_list;
  Format.fprintf trace_fmt "@.@?" 
  
  
let testgen_source dirname basename extension =
  let source_name = dirname ^ "/" ^ basename ^ extension in

  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v>");

  (* Parsing source *)
  let prog = parse_source source_name in
  let prog, dependencies = Compiler_stages.stage1 prog dirname basename in

  (* Two cases
     - generation of coverage conditions
     - generation of mutants: a number of mutated lustre files 
  *)
  
  if !Options.gen_mcdc then (
    let prog_mcdc = PathConditions.mcdc prog in
    (* We re-type the fresh equations *)
    let _ = import_dependencies prog_mcdc in
    let _ = type_decls !Global.type_env prog_mcdc in

    let destname = !Options.dest_dir ^ "/" ^ basename in
    let source_file = destname ^ ".mcdc" in (* Could be changed *)

    (* Modified Lustre is produced in fresh .lus file *)
    let source_lus = source_file ^ ".lus" in
    let source_out = open_out source_lus in
    let fmt = formatter_of_out_channel source_out in
    Printers.pp_prog fmt prog_mcdc;
    Format.fprintf fmt "@.@?";

    (* Prog is 
       (1) cleaned from initial equations TODO
       (2) produced as EMF
    *)
    Options.output := "emf";
    let prog_mcdc = Normalization.normalize_prog ~backend:"emf" prog_mcdc in
    let machine_code = Compiler_stages.stage2 prog_mcdc in
    let source_emf = source_file ^ ".emf" in 
    let source_out = open_out source_emf in
    let fmt = formatter_of_out_channel source_out in
    EMF_backend.translate fmt basename prog_mcdc machine_code;

    exit 0
  ) ;

  
  (* generate mutants *)
  let mutants = Mutation.mutate !Options.nb_mutants prog in
  
  (* Print generated mutants in target directory. *)
  let cpt = ref 0 in
  let mutation_list =
    List.map (fun (mutation, mutation_loc, mutant) ->
    (* Debugging code *)
    (* if List.mem !cpt [238;371;601;799;875;998] then *)
    (*   Format.eprintf "Mutant %i: %a -> %a" !cpt Printers.pp_expr orig_e Printers.pp_expr new_e  *)
    (* ; *)
      incr cpt;
      let mutant_basename = (Filename.basename basename)^ ".mutant.n" ^ (string_of_int !cpt) ^ extension  in
      let mutant_filename = 
	match !Options.dest_dir with
	| "" -> (* Mutants are generated in source directory *)
	   basename^ ".mutant.n" ^ (string_of_int !cpt) ^ extension 
      | dir ->  (* Mutants are generated in target directory *)
	 dir ^ "/" ^ mutant_basename 
    in
    let mutant_out = (
      try 
	open_out mutant_filename 
      with
	Sys_error _ -> Format.eprintf "Unable to open file %s for writing.@." mutant_filename; exit 1
    )
    in
    let mutant_fmt = formatter_of_out_channel mutant_out in
    report ~level:1 (fun fmt -> fprintf fmt ".. generating mutant %s: %a@,@?"
      mutant_filename
      Mutation.print_directive mutation
    );
    Format.fprintf mutant_fmt "%a@." Printers.pp_prog mutant;
    mutation, mutation_loc, mutant_basename
    )
      mutants
  in
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. done @ @]@.");
  
  (* Printing traceability *)
  let trace_filename = 
    match !Options.dest_dir with
    | "" -> (* Mutant report is generated in source directory *)
       basename^ ".mutation.json" 
    | dir ->  (* Mutants are generated in target directory *)
       dir ^ "/" ^ (Filename.basename basename)^ ".mutation.json"
  in
  pp_trace trace_filename mutation_list;

  (* Printing the CMakeLists.txt file *)
  let cmakelists = 
    (if !Options.dest_dir = "" then "" else !Options.dest_dir ^ "/") ^ "CMakeLists.txt"
  in
  let cmake_file = open_out cmakelists in
  let cmake_fmt = formatter_of_out_channel cmake_file in
  Format.fprintf cmake_fmt "cmake_minimum_required(VERSION 3.5)@.";
  Format.fprintf cmake_fmt "include(\"%s/share/helpful_functions.cmake\")@." Version.prefix;
  Format.fprintf cmake_fmt "include(\"%s/share/FindLustre.cmake\")@." Version.prefix;
  Format.fprintf cmake_fmt "LUSTREFILES(LFILES ${CMAKE_CURRENT_SOURCE_DIR} )@.";
  Format.fprintf cmake_fmt "@[<v 2>FOREACH(lus_file ${LFILES})@ ";
  Format.fprintf cmake_fmt "get_lustre_name_ext(${lus_file} L E)@ ";
  Format.fprintf cmake_fmt "Lustre_Compile(@[<v 0>@ ";
  if !Options.main_node <> "" then Format.fprintf cmake_fmt "NODE \"%s_mutant\"@ " !Options.main_node;
  Format.fprintf cmake_fmt "LIBNAME \"${L}_%s_mutant\"@ " !Options.main_node;
  Format.fprintf cmake_fmt "LUS_FILES \"${lus_file}\")@]@]@.";
  Format.fprintf cmake_fmt "ENDFOREACH()@.@?";
  
  
  (* We stop the process here *)
  exit 0
    
let testgen dirname basename extension =
  match extension with
  | ".lus"   -> testgen_source dirname basename extension
  | _        -> assert false

let anonymous filename =
  let ok_ext, ext = List.fold_left
    (fun (ok, ext) ext' ->
      if not ok && Filename.check_suffix filename ext' then
	true, ext'
      else
	ok, ext)
    (false, "") extensions in
  if ok_ext then
    let dirname = Filename.dirname filename in
    let basename = Filename.chop_suffix (Filename.basename filename) ext in
    testgen dirname basename ext
  else
    raise (Arg.Bad ("Can only compile *.lus files"))

let _ =
  Global.initialize ();
  Corelang.add_internal_funs ();
  try
    Printexc.record_backtrace true;

    let options = Options_management.lustret_options

    in
    
    Arg.parse options anonymous usage
  with
  | Parse.Error _
  | Types.Error (_,_) | Clocks.Error (_,_)
  | Corelang.Error _ (*| Task_set.Error _*)
  | Causality.Error _ -> exit 1
  | Sys_error msg -> (eprintf "Failure: %s@." msg)
  | exc -> (track_exception (); raise exc)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
