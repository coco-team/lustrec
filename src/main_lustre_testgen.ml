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

(* From prog to prog *)
let stage1 prog dirname basename =
  (* Removing automata *) 
  let prog = expand_automata prog in

  Log.report ~level:4 (fun fmt -> fprintf fmt ".. after automata expansion:@.@[<v 2>@ %a@]@," Printers.pp_prog prog);

  (* Importing source *)
  let _ = Modules.load_program ISet.empty prog in

  (* Extracting dependencies *)
  let dependencies, type_env, clock_env = import_dependencies prog in

  (* Sorting nodes *)
  let prog = SortProg.sort prog in

  (* Perform inlining before any analysis *)
  let orig, prog =
    if !Options.global_inline && !Options.main_node <> "" then
      (if !Options.witnesses then prog else []),
      Inliner.global_inline basename prog type_env clock_env
    else (* if !Option.has_local_inline *)
      [],
      Inliner.local_inline basename prog type_env clock_env
  in

  check_stateless_decls prog;
  
  (* Typing *)
  let _ (*computed_types_env*) = type_decls type_env prog in

  (* Clock calculus *)
  let _ (*computed_clocks_env*) = clock_decls clock_env prog in

  (* Creating destination directory if needed *)
  create_dest_dir ();

  Typing.uneval_prog_generics prog;
  Clock_calculus.uneval_prog_generics prog;

  if !Options.global_inline && !Options.main_node <> "" && !Options.witnesses then
    begin
      let orig = Corelang.copy_prog orig in
      Log.report ~level:1 (fun fmt -> fprintf fmt ".. generating witness file@,");
      check_stateless_decls orig;
      let _ = Typing.type_prog type_env orig in
      let _ = Clock_calculus.clock_prog clock_env orig in
      Typing.uneval_prog_generics orig;
      Clock_calculus.uneval_prog_generics orig;
      Inliner.witness
	basename
	!Options.main_node
	orig prog type_env clock_env
    end;

  (* Normalization phase *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. normalization@,");
  (* Special treatment of arrows in lustre backend. We want to keep them *)
  if !Options.output = "lustre" then
    Normalization.unfold_arrow_active := false;
  let prog = Normalization.normalize_prog prog in
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);

  prog, dependencies

let testgen_source dirname basename extension =
  let source_name = dirname ^ "/" ^ basename ^ extension in

  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v>");

  (* Parsing source *)
  let prog = parse_source source_name in

  let prog, dependencies = stage1 prog dirname basename in

  if !Options.gen_mcdc then (
    PathConditions.mcdc prog;
    exit 0
  ) ;
  (* generate mutants *)
  let mutants, mutation_printer = Mutation.mutate !Options.nb_mutants prog in
  
  (* Print generated mutants in target directory. *)
  let cpt = ref 0 in
  List.iter (fun (mutation, mutant) ->
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
    report ~level:1 (fun fmt -> fprintf fmt ".. generating mutant %s: %a@,@?" mutant_filename mutation_printer mutation);
    Format.fprintf mutant_fmt "%a@." Printers.pp_prog mutant    
  )
    mutants;
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. done @ @]@.");
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
