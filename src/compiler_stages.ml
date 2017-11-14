open Format
open Utils
open Compiler_common
open LustreSpec

exception StopPhase1 of program

let dynamic_checks () =
  match !Options.output, !Options.spec with
  | "C", "C" -> true
  | _ -> false
     

(* check whether a source file has a compiled header, if not, generate the
   compiled header *)
let compile_source_to_header prog computed_types_env computed_clocks_env dirname basename extension =
  let destname = !Options.dest_dir ^ "/" ^ basename in
  let lusic_ext = extension ^ "c" in
  let header_name = destname ^ lusic_ext in
  begin
    if not (Sys.file_exists header_name) then
      begin
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. generating compiled header file %s@," header_name);
	Lusic.write_lusic false (Lusic.extract_header dirname basename prog) destname lusic_ext;
	Lusic.print_lusic_to_h destname lusic_ext
      end
    else
      let lusic = Lusic.read_lusic destname lusic_ext in
      if not lusic.Lusic.from_lusi then
	begin
	  Log.report ~level:1 (fun fmt -> fprintf fmt ".. generating compiled header file %s@," header_name);
       	  Lusic.write_lusic false (Lusic.extract_header dirname basename prog) destname lusic_ext;
	  (*List.iter (fun top_decl -> Format.eprintf "lusic: %a@." Printers.pp_decl top_decl) lusic.Lusic.contents;*)
	  Lusic.print_lusic_to_h destname lusic_ext
	end
      else
	begin
	  Log.report ~level:1 (fun fmt -> fprintf fmt ".. loading compiled header file %s@," header_name);
	  Modules.check_dependency lusic destname;
	  let header = lusic.Lusic.contents in
	  let (declared_types_env, declared_clocks_env) = get_envs_from_top_decls header in
	  check_compatibility
	    (prog, computed_types_env, computed_clocks_env)
	    (header, declared_types_env, declared_clocks_env)
	end
  end


(* From prog to prog *)
let stage1 prog dirname basename =
  (* Removing automata *)
  let prog = expand_automata prog in
  Log.report ~level:4 (fun fmt -> fprintf fmt ".. after automata expansion:@,  @[<v 2>@,%a@]@ " Printers.pp_prog prog);

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
      Inliner.local_inline prog (* type_env clock_env *)
  in

  (* Checking stateless/stateful status *)
  if Plugins.check_force_stateful () then
    force_stateful_decls prog
  else
    check_stateless_decls prog;

  (* Typing *)
  let computed_types_env = type_decls type_env prog in

  (* Clock calculus *)
  let computed_clocks_env = clock_decls clock_env prog in

  (* Generating a .lusi header file only *)
  if !Options.lusi then
    (* We stop here the processing and produce the current prog. It will be
       exported as a lusi *)
    raise (StopPhase1 prog);

  (* Optimization of prog: 
     - Unfold consts 
     - eliminate trivial expressions
  *)
  (*
    let prog = 
    if !Options.const_unfold || !Options.optimization >= 5 then
    begin
    Log.report ~level:1 (fun fmt -> fprintf fmt ".. eliminating constants and aliases@,");
    Optimize_prog.prog_unfold_consts prog
    end
    else
    prog
    in
  *)
  (* Delay calculus *)
  (* TO BE DONE LATER (Xavier)
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

  (* Creating destination directory if needed *)
  create_dest_dir ();

  (* Compatibility with Lusi *)
  (* Checking the existence of a lusi (Lustre Interface file) *)
  let extension = ".lusi" in
  compile_source_to_header prog computed_types_env computed_clocks_env dirname basename extension;

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
  
  (* Computes and stores generic calls for each node,
     only useful for ANSI C90 compliant generic node compilation *)
  if !Options.ansi then Causality.NodeDep.compute_generic_calls prog;
  (*Hashtbl.iter (fun id td -> match td.Corelang.top_decl_desc with
    Corelang.Node nd -> Format.eprintf "%s calls %a" id
    Causality.NodeDep.pp_generic_calls nd | _ -> ()) Corelang.node_table;*)

  (* If some backend involving dynamic checks are active, then node annotations become runtime checks *)
  let prog =
    if dynamic_checks () then
      Spec.enforce_spec_prog prog
    else
      prog
  in
  
  (* Normalization phase *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. normalization@,");
  let prog = Normalization.normalize_prog ~backend:!Options.output prog in
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);

  let prog =
    if !Options.mpfr
    then
      begin
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. targetting MPFR library@,");
	Mpfr.inject_prog prog
      end
    else
      begin
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. keeping floating-point numbers@,");
	prog
      end in
  Log.report ~level:2 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);

  (* Checking array accesses *)
  if !Options.check then
    begin
      Log.report ~level:1 (fun fmt -> fprintf fmt ".. checking array accesses@,");
      Access.check_prog prog;
    end;

  prog, dependencies
