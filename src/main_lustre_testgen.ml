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
open LustreSpec
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
    PathConditions.mcdc prog;
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
    let mutant_filename = 
      match !Options.dest_dir with
      | "" -> (* Mutants are generated in source directory *)
	 basename^ ".mutant.n" ^ (string_of_int !cpt) ^ extension 
      | dir ->  (* Mutants are generated in target directory *)
	 dir ^ "/" ^ (Filename.basename basename)^ ".mutant.n" ^ (string_of_int !cpt) ^ extension 
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
    mutation, mutation_loc, mutant_filename
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
