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
open Log
open Compiler_common

open Utils
open LustreSpec
 

let usage = "Usage: lustrec [options] \x1b[4msource file\x1b[0m"

let extensions = [".ec"; ".lus"; ".lusi"]

(* print a .lusi header file from a source prog *)
let print_lusi prog dirname basename extension =
  let header = Lusic.extract_header dirname basename prog in
  let header_name = dirname ^ "/" ^ basename ^ extension in
  let h_out = open_out header_name in
  let h_fmt = formatter_of_out_channel h_out in
  begin
    Typing.uneval_prog_generics header;
    Clock_calculus.uneval_prog_generics header;
    Printers.pp_lusi_header h_fmt basename header;
    close_out h_out
  end

(* compile a .lusi header file *)
let compile_header dirname  basename extension =
  let destname = !Options.dest_dir ^ "/" ^ basename in
  let header_name = basename ^ extension in
  let lusic_ext = extension ^ "c" in
  begin
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v>");
    let header = parse_header true (dirname ^ "/" ^ header_name) in
    ignore (Modules.load_header ISet.empty header);
    ignore (check_top_decls header);
    create_dest_dir ();
    Log.report ~level:1
      (fun fmt -> fprintf fmt ".. generating compiled header file %sc@," (destname ^ extension));
    Lusic.write_lusic true header destname lusic_ext;
    generate_lusic_header destname lusic_ext;
    Log.report ~level:1 (fun fmt -> fprintf fmt ".. done !@ @]@ ")
  end




(* from source to machine code, with optimization *)
let stage2 prog =    
  (* Computation of node equation scheduling. It also breaks dependency cycles
     and warns about unused input or memory variables *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. @[<v 2>scheduling@ ");
  let prog, node_schs =
    try 
      Scheduling.schedule_prog prog
    with Causality.Error _ -> (* Error is not kept. It is recomputed in a more
				 systemtic way in AlgebraicLoop module *)
      AlgebraicLoop.analyze prog
  in
  Log.report ~level:1 (fun fmt -> fprintf fmt "%a"              Scheduling.pp_warning_unused node_schs);
  Log.report ~level:3 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_schedule node_schs);
  Log.report ~level:3 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_fanin_table node_schs);
  Log.report ~level:5 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Scheduling.pp_dep_graph node_schs);
  Log.report ~level:3 (fun fmt -> fprintf fmt "@[<v 2>@ %a@]@," Printers.pp_prog prog);
  Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");

  (* TODO Salsa optimize prog: 
     - emits warning for programs with pre inside expressions
     - make sure each node arguments and memory is bounded by a local annotation
     - introduce fresh local variables for each real pure subexpression
  *)
  (* DFS with modular code generation *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. machines generation@,");
  let machine_code = Machine_code.translate_prog prog node_schs in

  Log.report ~level:3 (fun fmt -> fprintf fmt ".. generated machines (unoptimized):@ %a@ "Machine_code.pp_machines machine_code);

  (* Optimize machine code *)
  Optimize_machine.optimize prog node_schs machine_code


(* printing code *)
let stage3 prog machine_code dependencies basename =
  let basename    =  Filename.basename basename in
  match !Options.output with
    "C" -> 
      begin
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. C code generation@,");
	C_backend.translate_to_c
	  (* alloc_header_file source_lib_file source_main_file makefile_file *)
	  basename prog machine_code dependencies
      end
  | "java" ->
     begin
       (Format.eprintf "internal error: sorry, but not yet supported !"; assert false)
     (*let source_file = basename ^ ".java" in
       Log.report ~level:1 (fun fmt -> fprintf fmt ".. opening file %s@,@?" source_file);
       let source_out = open_out source_file in
       let source_fmt = formatter_of_out_channel source_out in
       Log.report ~level:1 (fun fmt -> fprintf fmt ".. java code generation@,@?");
       Java_backend.translate_to_java source_fmt basename normalized_prog machine_code;*)
     end
  | "horn" ->
     begin
       let destname = !Options.dest_dir ^ "/" ^ basename in
       let source_file = destname ^ ".smt2" in (* Could be changed *)
       let source_out = open_out source_file in
       let fmt = formatter_of_out_channel source_out in
       Log.report ~level:1 (fun fmt -> fprintf fmt ".. hornification@,");
       Horn_backend.translate fmt basename prog (Machine_code.arrow_machine::machine_code);
       (* Tracability file if option is activated *)
       if !Options.traces then (
	 let traces_file = destname ^ ".traces.xml" in (* Could be changed *)
	 let traces_out = open_out traces_file in
	 let fmt = formatter_of_out_channel traces_out in
         Log.report ~level:1 (fun fmt -> fprintf fmt ".. tracing info@,");
	 Horn_backend_traces.traces_file fmt basename prog machine_code;
       )
     end
  | "lustre" ->
     begin
       let destname = !Options.dest_dir ^ "/" ^ basename in
       let source_file = destname ^ ".lustrec.lus" in (* Could be changed *)
       let source_out = open_out source_file in
       let fmt = formatter_of_out_channel source_out in
       Printers.pp_prog fmt prog;
       Format.fprintf fmt "@.@?";
       (*	Lustre_backend.translate fmt basename normalized_prog machine_code *)
       ()
     end
  | "emf" ->
     begin
       let destname = !Options.dest_dir ^ "/" ^ basename in
       let source_file = destname ^ ".emf" in (* Could be changed *)
       let source_out = open_out source_file in
       let fmt = formatter_of_out_channel source_out in
       EMF_backend.translate fmt basename prog machine_code;
       ()
     end

  | _ -> assert false

(* compile a .lus source file *)
let rec compile_source dirname basename extension =
  let source_name = dirname ^ "/" ^ basename ^ extension in

  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 0>");

  (* Parsing source *)
  let prog = parse_source source_name in

  let prog =
    if !Options.mpfr then
      Mpfr.mpfr_module::prog
    else
      prog
  in
  let prog, dependencies = 
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>.. Phase 1 : Normalisation@,");
    try 
      Compiler_stages.stage1 prog dirname basename
    with Compiler_stages.StopPhase1 prog -> (
      if !Options.lusi then
	begin
	  let lusi_ext = extension ^ "i" in
	  Log.report ~level:1 (fun fmt -> fprintf fmt ".. generating interface file %s@ " (basename ^ lusi_ext));
	  print_lusi prog dirname basename lusi_ext;
	  Log.report ~level:1 (fun fmt -> fprintf fmt ".. done !@ @]@.");
	  exit 0
	end
      else
        assert false
    )
  in
  Log.report ~level:1 (fun fmt -> fprintf fmt "@]@,");
  Log.report ~level:3 (fun fmt -> fprintf fmt ".. Normalized program:@ %a@ "Printers.pp_prog prog);

  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>.. Phase 2 : Machines generation@,");

  let machine_code = 
    stage2 prog 
  in

  Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");
  Log.report ~level:3 (fun fmt -> fprintf fmt ".. Generated machines:@ %a@ "Machine_code.pp_machines machine_code);

  if Scopes.Plugin.show_scopes () then
    begin
      let all_scopes = Scopes.compute_scopes prog !Options.main_node in
      (* Printing scopes *)
      if !Options.verbose_level >= 1 then
	Format.printf "Possible scopes are:@   ";
      Format.printf "@[<v>%a@ @]@ @?" Scopes.print_scopes all_scopes;
      exit 0
	
    end;

  let machine_code = Plugins.refine_machine_code prog machine_code in
  
  stage3 prog machine_code dependencies basename;
  begin
    Log.report ~level:1 (fun fmt -> fprintf fmt ".. done !@ @]@.");
    (* We stop the process here *)
    exit 0
  end

let compile dirname basename extension =
  match extension with
  | ".lusi"  -> compile_header dirname basename extension
  | ".lus"   -> compile_source dirname basename extension
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
    compile dirname basename ext
  else
    raise (Arg.Bad ("Can only compile *.lusi, *.lus or *.ec files"))

let _ =
  Global.initialize ();
  Corelang.add_internal_funs ();
  try
    Printexc.record_backtrace true;

    let options = Options_management.lustrec_options @ (Plugins.options ()) in
    
    Arg.parse options anonymous usage
  with
  | Parse.Error _
  | Types.Error (_,_) | Clocks.Error (_,_) -> exit 1
  | Corelang.Error (_ (* loc *), kind) (*| Task_set.Error _*) -> exit (Error.return_code kind)
  (* | Causality.Error _  -> exit (Error.return_code Error.AlgebraicLoop) *)
  | Sys_error msg -> (eprintf "Failure: %s@." msg); exit 1
  | exc -> (track_exception (); raise exc) 

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
