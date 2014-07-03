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

open LustreSpec

let usage = "Usage: lustrec [options] <source-file>"

let extensions = [".ec"; ".lus"; ".lusi"]

let check_stateless_decls decls =
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. checking stateless/stateful status@ ");
  try
    Stateless.check_prog decls
  with (Stateless.Error (loc, err)) as exc ->
    eprintf "Stateless status error %a%a@."
      Stateless.pp_error err
      Location.pp_loc loc;
    raise exc

let type_decls env decls =  
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. typing@ ");
  let new_env = 
    begin
      try
	Typing.type_prog env decls
      with (Types.Error (loc,err)) as exc ->
	eprintf "Typing error %a%a@."
	  Types.pp_error err
	  Location.pp_loc loc;
	raise exc
    end 
  in
  if !Options.print_types then
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>  %a@]@ " Corelang.pp_prog_type decls);
  new_env
      
let clock_decls env decls = 
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. clock calculus@ ");
  let new_env =
    begin
      try
	Clock_calculus.clock_prog env decls
      with (Clocks.Error (loc,err)) as exc ->
	eprintf "Clock calculus error %a%a@." Clocks.pp_error err Location.pp_loc loc;
	raise exc
    end
  in
  if !Options.print_clocks then
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>  %a@]@ " Corelang.pp_prog_clock decls);
  new_env

(* Loading Lusi file and filling type tables with parsed
   functions/nodes *)
let load_lusi own filename =
  Location.input_name := filename;
  let lexbuf = Lexing.from_channel (open_in filename) in
  Location.init lexbuf filename;
  (* Parsing *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. parsing header file %s@ " filename);
    try
      Parse.header own Parser_lustre.header Lexer_lustre.token lexbuf
    with
    | (Lexer_lustre.Error err) | (Parse.Syntax_err err) as exc -> 
      Parse.report_error err;
      raise exc
    | Corelang.Error (loc, err) as exc -> (
      eprintf "Parsing error %a%a@."
	Corelang.pp_error err
	Location.pp_loc loc;
      raise exc
    )


let check_lusi header =
  let new_tenv = type_decls Basic_library.type_env header in   (* Typing *)
  let new_cenv = clock_decls Basic_library.clock_env header in   (* Clock calculus *)
  header, new_tenv, new_cenv

let load_n_check_lusi source_name lusi_name prog computed_types_env computed_clocks_env= 
  try 
    let _ = open_in lusi_name in
    let header = load_lusi true lusi_name in
    let _, declared_types_env, declared_clocks_env = check_lusi header in
        
    (* checking stateless status compatibility *)
    Stateless.check_compat header;

    (* checking type compatibility with computed types*)
    Typing.check_env_compat header declared_types_env computed_types_env;
    Typing.uneval_prog_generics prog;
    
    (* checking clocks compatibility with computed clocks*)
    Clock_calculus.check_env_compat header declared_clocks_env computed_clocks_env;
    Clock_calculus.uneval_prog_generics prog

    with Sys_error _ -> ( 
      (* Printing lusi file is necessary *)
      Log.report ~level:1 
	(fun fmt -> 
	  fprintf fmt 
	    ".. generating lustre interface file %s@," lusi_name);
      let lusi_out = open_out lusi_name in
      let lusi_fmt = formatter_of_out_channel lusi_out in
      Typing.uneval_prog_generics prog;
      Clock_calculus.uneval_prog_generics prog;
      Printers.pp_lusi_header lusi_fmt source_name prog
    )
    | (Types.Error (loc,err)) as exc ->
      eprintf "Type mismatch between computed type and declared type in lustre interface file: %a@."
	Types.pp_error err;
      raise exc
    | Clocks.Error (loc, err) as exc ->
      eprintf "Clock mismatch between computed clock and declared clock in lustre interface file: %a@."
	Clocks.pp_error err;
      raise exc
    | Stateless.Error (loc, err) as exc ->
      eprintf "Stateless status mismatch between defined status and declared status in lustre interface file: %a@."
	Stateless.pp_error err;
      raise exc
    
let rec compile basename extension =

  (* Loading the input file *)
  let source_name = basename^extension in
  Location.input_name := source_name;
  let lexbuf = Lexing.from_channel (open_in source_name) in
  Location.init lexbuf source_name;

  (* Parsing *)
  Log.report ~level:1 
    (fun fmt -> fprintf fmt "@[<v>.. parsing file %s@," source_name);
  let prog =
    try
      Parse.prog Parser_lustre.prog Lexer_lustre.token lexbuf
    with
    | (Lexer_lustre.Error err) | (Parse.Syntax_err err) as exc -> 
      Parse.report_error err;
      raise exc
    | Corelang.Error (loc, err) as exc ->
      eprintf "Parsing error %a%a@."
	Corelang.pp_error err
	Location.pp_loc loc;
      raise exc
  in

  (* Extracting dependencies *)
  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>.. extracting dependencies@,");
  let dependencies = 
    List.fold_right 
      (fun d accu -> match d.top_decl_desc with 
      | Open (local, s) -> (s, local)::accu 
      | _ -> accu) 
      prog [] 
  in
  let dependencies, type_env, clock_env =
    List.fold_left (fun (compilation_dep, type_env, clock_env) (s, local) -> 
      try
	let basename = (if local then s else Version.prefix ^ "/include/lustrec/" ^ s ) ^ ".lusi" in 
	Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 0>Library %s@," basename);
	let comp_dep, lusi_type_env, lusi_clock_env = check_lusi (load_lusi false basename) in 
	Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");
	
	(s, local, comp_dep)::compilation_dep,
	Env.overwrite type_env lusi_type_env,
	Env.overwrite clock_env lusi_clock_env      
      with Sys_error msg -> (
	eprintf "Failure: impossible to load library %s.@.%s@." s msg;
	exit 1
      )
    )  ([], Basic_library.type_env, Basic_library.clock_env) dependencies
  in
  Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");
  
  (* Sorting nodes *)
  let prog = SortProg.sort prog in

  (* Checking stateless/stateful status *)
  check_stateless_decls prog;

  (* Typing *)
  let computed_types_env = type_decls type_env prog in
  
  (* Clock calculus *)
  let computed_clocks_env = clock_decls clock_env prog in

  (* Perform global inlining *)
  let prog =
    if !Options.global_inline && 
      (match !Options.main_node with | "" -> false | _ -> true) then
      Inliner.global_inline basename prog type_env clock_env
    else
      prog
  in

  (* Delay calculus *)
  (*
    if(!Options.delay_calculus)
    then
    begin
    Log.report ~level:1 (fun fmt -> fprintf fmt ".. initialisation analysis@?");
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

  (* Compatibility with Lusi *)
  (* Checking the existence of a lusi (Lustre Interface file) *)
  let lusi_name = basename ^ ".lusi" in
  load_n_check_lusi source_name lusi_name prog computed_types_env computed_clocks_env;

  (* Computes and stores generic calls for each node,
     only useful for ANSI C90 compliant generic node compilation *)
  if !Options.ansi then Causality.NodeDep.compute_generic_calls prog;
  (*Hashtbl.iter (fun id td -> match td.Corelang.top_decl_desc with Corelang.Node nd -> Format.eprintf "%s calls %a" id Causality.NodeDep.pp_generic_calls nd | _ -> ()) Corelang.node_table;*)

  (* Normalization phase *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. normalization@,");
  (* Special treatment of arrows in lustre backend. We want to keep them *)
  if !Options.output = "lustre" then
    Normalization.unfold_arrow_active := false;
  let prog = Normalization.normalize_prog prog in
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);

  (* Checking array accesses *)
  if !Options.check then
    begin
      Log.report ~level:1 (fun fmt -> fprintf fmt ".. array access checks@,");
      Access.check_prog prog;
    end;

  (* Computation of node equation scheduling. It also breaks dependency cycles
     and warns about unused input or memory variables *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. scheduling@,");
  let prog, node_schs = Scheduling.schedule_prog prog in
  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_warning_unused node_schs);
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_schedule node_schs);
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_fanin_table node_schs);
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);

 (* Optimization of prog: 
    - Unfold consts 
    - eliminate trivial expressions
 *)
  let prog = 
    if !Options.optimization >= 2 then 
      Optimize_prog.prog_unfold_consts prog 
    else
      prog
  in

  (* DFS with modular code generation *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. machines generation@,");
  let machine_code = Machine_code.translate_prog prog node_schs in
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@,"
    (Utils.fprintf_list ~sep:"@ " Machine_code.pp_machine)
    machine_code);

  (* experimental
  let machine_code = Machine_code.prog_reuse_var machine_code node_schs in
  *)
  (* Optimize machine code *)
  let machine_code = 
    if !Options.optimization >= 2 then
      Optimize_machine.optimize_machines machine_code
    else
      machine_code
  in
  
  (* Creating destination directory if needed *)
  if not (Sys.file_exists !Options.dest_dir) then (
    Log.report ~level:1 (fun fmt -> fprintf fmt ".. creating destination directory@,");
    Unix.mkdir !Options.dest_dir (Unix.stat ".").Unix.st_perm
  );
  if (Unix.stat !Options.dest_dir).Unix.st_kind <> Unix.S_DIR then (
    eprintf "Failure: destination %s is not a directory.@.@." !Options.dest_dir;
    exit 1
  );
  (* Printing code *)
  let basename    =  Filename.basename basename in
  let destname = !Options.dest_dir ^ "/" ^ basename in
  let _ = match !Options.output with
      "C" -> 
	begin
	  let header_file = destname ^ ".h" in (* Could be changed *)
	  let source_lib_file = destname ^ ".c" in (* Could be changed *)
	  let source_main_file = destname ^ "_main.c" in (* Could be changed *)
	  let makefile_file = destname ^ ".makefile" in (* Could be changed *)
	  (* let spec_file_opt = if !Options.c_spec then  *)
	  (*     ( *)
	  (* 	let spec_file = basename ^ "_spec.c" in *)
	  (* 	Log.report ~level:1 (fun fmt -> fprintf fmt ".. opening files %s, %s and %s@," header_file source_file spec_file); *)
	  (* 	Some spec_file  *)
	  (*     ) else ( *)
	  (* 	Log.report ~level:1 (fun fmt -> fprintf fmt ".. opening files %s and %s@," header_file source_file); *)
	  (* 	None  *)
	  (*      ) *)
	  (* in  *)
	  Log.report ~level:1 (fun fmt -> fprintf fmt ".. C code generation@,");
	  C_backend.translate_to_c 
	    header_file source_lib_file source_main_file makefile_file
	    basename prog machine_code dependencies
	end
    | "java" ->
      begin
	failwith "Sorry, but not yet supported !"
    (*let source_file = basename ^ ".java" in
      Log.report ~level:1 (fun fmt -> fprintf fmt ".. opening file %s@,@?" source_file);
      let source_out = open_out source_file in
      let source_fmt = formatter_of_out_channel source_out in
      Log.report ~level:1 (fun fmt -> fprintf fmt ".. java code generation@,@?");
      Java_backend.translate_to_java source_fmt basename normalized_prog machine_code;*)
      end
    | "horn" ->
      begin
	let source_file = destname ^ ".smt2" in (* Could be changed *)
	let source_out = open_out source_file in
	let fmt = formatter_of_out_channel source_out in
	Horn_backend.translate fmt basename prog machine_code
      end
    | "lustre" -> 
      begin
	let source_file = destname ^ ".lustrec.lus" in (* Could be changed *)
	let source_out = open_out source_file in
	let fmt = formatter_of_out_channel source_out in
	Printers.pp_prog fmt prog;
(*	Lustre_backend.translate fmt basename normalized_prog machine_code *)
	()
      end

    | _ -> assert false
  in
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. done !@ @]@.");
  (* We stop the process here *)
  exit 0
  
let anonymous filename =
  let ok_ext, ext = List.fold_left (fun (ok, ext) ext' -> if not ok && Filename.check_suffix filename ext' then true, ext' else ok, ext) (false, "") extensions in
  if ok_ext then
    let basename = Filename.chop_suffix filename ext in
    compile basename ext
  else
    raise (Arg.Bad ("Can only compile *.lusi, *.lus or *.ec files"))

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
