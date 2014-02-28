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
open Log

let usage = "Usage: lustrec [options] <source-file>"

let extensions = [".ec";".lus"]

let type_decls env decls =  
  report ~level:1 (fun fmt -> fprintf fmt ".. typing@,@?");
  let new_env = 
    begin
      try
	Typing.type_prog env decls
    (*Typing.uneval_prog_generics prog*)
      with (Types.Error (loc,err)) as exc ->
	Format.eprintf "Typing error at loc %a: %a@]@."
	  Location.pp_loc loc
	  Types.pp_error err;
	raise exc
    end 
  in
  if !Options.print_types then
    report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@,@?" Corelang.pp_prog_type decls);
  new_env
      
let clock_decls env decls = 
  report ~level:1 (fun fmt -> fprintf fmt ".. clock calculus@,@?");
  let new_env =
    begin
      try
	Clock_calculus.clock_prog env decls
      with (Clocks.Error (loc,err)) as exc ->
	Location.print loc;
	eprintf "Clock calculus error at loc %a: %a@]@." Location.pp_loc loc Clocks.pp_error err;
	raise exc
    end
  in
  if !Options.print_clocks then
    report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@,@?" Corelang.pp_prog_clock decls);
  new_env

(* Loading Lusi file and filing type tables with parsed
   functions/nodes *)
let load_lusi filename =
  Location.input_name := filename;
  let lexbuf = Lexing.from_channel (open_in filename) in
  Location.init lexbuf filename;
  (* Parsing *)
  report ~level:1 (fun fmt -> fprintf fmt "@[<v>.. parsing header file %s@,@?" filename);
  let header = 
    try
      Parse.prog Parser_lustre.header Lexer_lustre.token lexbuf
    with (Lexer_lustre.Error err) | (Parse.Syntax_err err) as exc -> 
      Parse.report_error err;
      raise exc
  in
  let new_tenv = type_decls Basic_library.type_env header in   (* Typing *)
  let new_cenv: Clocks.clock_expr Utils.IMap.t = clock_decls Basic_library.clock_env header in   (* Clock calculus *)
  header, new_tenv, new_cenv
  
    
let rec compile basename extension =
  (* Loading the input file *)
  let source_name = basename^extension in
  Location.input_name := source_name;
  let lexbuf = Lexing.from_channel (open_in source_name) in
  Location.init lexbuf source_name;
  (* Parsing *)
  report ~level:1 
    (fun fmt -> fprintf fmt "@[<v>.. parsing file %s@,@?" source_name);
  let prog =
    try
      Parse.prog Parser_lustre.prog Lexer_lustre.token lexbuf
    with (Lexer_lustre.Error err) | (Parse.Syntax_err err) as exc -> 
      Parse.report_error err;
      raise exc
  in
  (* Extracting dependencies *)
  report ~level:1 (fun fmt -> fprintf fmt ".. extracting dependencies@,@?");
  let dependencies = 
    List.fold_right 
      (fun d accu -> match d.Corelang.top_decl_desc with 
      | Corelang.Open s -> s::accu 
      | _ -> accu) 
      prog [] 
  in
  let type_env, clock_env =
    List.fold_left (fun (type_env, clock_env) s -> 
      try
	let basename = s ^ ".lusi" in 
	report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>Library %s@ " s);
	let _, lusi_type_env, lusi_clock_env = load_lusi basename in 
	report ~level:1 (fun fmt -> fprintf fmt "@]@,@?");
	Env.overwrite type_env lusi_type_env,
	Env.overwrite clock_env lusi_clock_env      
      with Sys_error msg -> (
	Format.eprintf "Failure: impossible to load library %s.@.%s@." s msg;
	exit 1
      )
    )  (Basic_library.type_env, Basic_library.clock_env) dependencies
  in
  
  (* Unfold consts *)
  (*let prog = Corelang.prog_unfold_consts prog in*)

  (* Sorting nodes *)
  let prog = SortProg.sort prog in
  
  (* Typing *)
  let computed_types_env = type_decls type_env prog in
  
  (* Clock calculus *)
  let computed_clocks_env = clock_decls clock_env prog in

  (* Perform global inlining *)
  let prog =
    if !Options.global_inline && 
      (match !Options.main_node with | "" -> false | _ -> true) then
      Inliner.global_inline prog type_env clock_env
    else
      prog
  in

  (* Delay calculus *)
  (*
    if(!Options.delay_calculus)
    then
    begin
    report ~level:1 (fun fmt -> fprintf fmt ".. initialisation analysis@?");
    try
    Delay_calculus.delay_prog Basic_library.delay_env prog
    with (Delay.Error (loc,err)) as exc ->
    Location.print loc;
    eprintf "%a" Delay.pp_error err;
    Utils.track_exception ();
    raise exc
    end;
  *)
  (*
    eprintf "Causality analysis@.@?";
    (* Causality analysis *)
    begin
    try
    Causality.check_causal_prog prog
    with (Causality.Cycle v) as exc ->
    Causality.pp_error err_formatter v;
    raise exc
    end;
  *)
  (* Computes and stores generic calls for each node,
     only useful for ANSI C90 compliant generic node compilation *)
  if !Options.ansi then Causality.NodeDep.compute_generic_calls prog;
  (*Hashtbl.iter (fun id td -> match td.Corelang.top_decl_desc with Corelang.Node nd -> Format.eprintf "%s calls %a" id Causality.NodeDep.pp_generic_calls nd | _ -> ()) Corelang.node_table;*)

  (* Normalization phase *)
  report ~level:1 (fun fmt -> fprintf fmt ".. normalization@,@?");
  let normalized_prog = Normalization.normalize_prog prog in
  Typing.uneval_prog_generics normalized_prog;
  report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@,@?" Printers.pp_prog normalized_prog);
  (* Checking array accesses *)
  if !Options.check then
    begin
      report ~level:1 (fun fmt -> fprintf fmt ".. array access checks@,@?");
      Access.check_prog normalized_prog;
    end;

  (* DFS with modular code generation *)
  report ~level:1 (fun fmt -> fprintf fmt ".. machines generation@,@?");
  let machine_code = Machine_code.translate_prog normalized_prog in
  report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@,@?"
    (Utils.fprintf_list ~sep:"@ " Machine_code.pp_machine)
    machine_code);

  (* Checking the existence of a lusi (Lustre Interface file) *)
  let lusi_name = basename ^ ".lusi" in
  let _ = 
    try 
      let _ = open_in lusi_name in
      let _, declared_types_env, declared_clocks_env = load_lusi lusi_name in
      (* checking type compatibilty with computed types*)
      Typing.check_env_compat declared_types_env computed_types_env;
      (* checking clocks compatibilty with computed clocks*)
      Clock_calculus.check_env_compat declared_clocks_env computed_clocks_env;
      
    with Sys_error _ -> ( 
      (* Printing lusi file is necessary *)
      report ~level:1 
	(fun fmt -> 
	  fprintf fmt 
	    ".. generating lustre interface file %s@,@?" lusi_name);
      let lusi_out = open_out lusi_name in
      let lusi_fmt = formatter_of_out_channel lusi_out in
      Printers.pp_lusi_header lusi_fmt source_name normalized_prog
    )
    | (Types.Error (loc,err)) as exc ->
      Format.eprintf "Type mismatch between computed type and declared type in lustre interface file: %a@]@."
	Types.pp_error err;
      raise exc
  in

  (* Printing code *)
  let basename    = Filename.basename basename in
  let _ = match !Options.output with
      "C" -> 
	begin
	  let header_file = basename ^ ".h" in (* Could be changed *)
	  let source_file = basename ^ ".c" in (* Could be changed *)
	  let makefile_file = basename ^ ".makefile" in (* Could be changed *)
	  let spec_file_opt = if !Options.c_spec then 
	      (
		let spec_file = basename ^ "_spec.c" in
		report ~level:1 (fun fmt -> fprintf fmt ".. opening files %s, %s and %s@,@?" header_file source_file spec_file);
		Some spec_file 
	      ) else (
		report ~level:1 (fun fmt -> fprintf fmt ".. opening files %s and %s@,@?" header_file source_file);
		None 
	       )
	  in 
	  let header_out = open_out header_file in
	  let header_fmt = formatter_of_out_channel header_out in
	  let source_out = open_out source_file in
	  let source_fmt = formatter_of_out_channel source_out in
	  let makefile_out = open_out makefile_file in
	  let makefile_fmt = formatter_of_out_channel makefile_out in
	  let spec_fmt_opt = match spec_file_opt with
	      None -> None
	    | Some f -> Some (formatter_of_out_channel (open_out f))
	  in
	  report ~level:1 (fun fmt -> fprintf fmt ".. C code generation@,@?");
	  C_backend.translate_to_c header_fmt source_fmt makefile_fmt spec_fmt_opt basename normalized_prog machine_code;
	end
    | "java" -> begin
      failwith "Sorry, but not yet supported !"
    (*let source_file = basename ^ ".java" in
      report ~level:1 (fun fmt -> fprintf fmt ".. opening file %s@,@?" source_file);
      let source_out = open_out source_file in
      let source_fmt = formatter_of_out_channel source_out in
      report ~level:1 (fun fmt -> fprintf fmt ".. java code generation@,@?");
      Java_backend.translate_to_java source_fmt basename normalized_prog machine_code;*)
    end
    | "horn" -> begin
      let source_file = basename ^ ".smt2" in (* Could be changed *)
      let source_out = open_out source_file in
      let fmt = formatter_of_out_channel source_out in
      Horn_backend.translate fmt basename normalized_prog machine_code
    end
    | _ -> assert false
  in
  report ~level:1 (fun fmt -> fprintf fmt ".. done !@ @]@.");
  (* We stop the process here *)
  exit 0
  
let anonymous filename =
  let ok_ext, ext = List.fold_left (fun (ok, ext) ext' -> if not ok && Filename.check_suffix filename ext' then true, ext' else ok, ext) (false, "") extensions in
  if ok_ext then
    let basename = Filename.chop_suffix filename ext in
    compile basename ext
  else
    raise (Arg.Bad ("Can only compile *.lus or *.ec files"))

let _ =
  Corelang.add_internal_funs ();
  try
    Printexc.record_backtrace true;
    Arg.parse Options.options anonymous usage
  with
  | Parse.Syntax_err _ | Lexer_lustre.Error _ 
  | Types.Error (_,_) | Clocks.Error (_,_)
  | Corelang.Error _ (*| Task_set.Error _*) 
  | Causality.Cycle _ -> exit 1
  | exc -> (Utils.track_exception (); raise exc)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
