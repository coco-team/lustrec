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

open Utils
open Format 
open Lustre_types
open Corelang

let check_main () =
  if !Options.main_node = "" then
    begin
      eprintf "Code generation error: %a@." Error.pp_error_msg Error.No_main_specified;
      raise (Error (Location.dummy_loc, Error.No_main_specified))
    end

let create_dest_dir () =
  begin
    if not (Sys.file_exists !Options.dest_dir) then
      begin
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. creating destination directory@ ");
	Unix.mkdir !Options.dest_dir (Unix.stat ".").Unix.st_perm
      end;
    if (Unix.stat !Options.dest_dir).Unix.st_kind <> Unix.S_DIR then
      begin
	eprintf "Failure: destination %s is not a directory.@.@." !Options.dest_dir;
	exit 1
      end
  end

(* Loading Lusi file and filling type tables with parsed
   functions/nodes *)
let parse_header own filename =
  Location.set_input filename;
  let h_in = open_in filename in
  let lexbuf = Lexing.from_channel h_in in
  Location.init lexbuf filename;
  (* Parsing *)
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. parsing header file %s@ " filename);
    try
      let header = Parse.header Parser_lustre.header Lexer_lustre.token lexbuf in
      (*ignore (Modules.load_header ISet.empty header);*)
      close_in h_in;
      header
    with
    | (Parse.Error err) as exc -> 
      Parse.report_error err;
      raise exc
    | Corelang.Error (loc, err) as exc -> (
      eprintf "Parsing error: %a%a@."
	Error.pp_error_msg err
	Location.pp_loc loc;
      raise exc
    )

let parse_source source_name =
  (* Loading the input file *)
  Location.set_input source_name;
  let s_in = open_in source_name in
  let lexbuf = Lexing.from_channel s_in in
  Location.init lexbuf source_name;

  (* Parsing *)
  Log.report ~level:1 
    (fun fmt -> fprintf fmt ".. parsing source file %s@ " source_name);
  try
    let prog = Parse.prog Parser_lustre.prog Lexer_lustre.token lexbuf in
    (*ignore (Modules.load_program ISet.empty prog);*)
    close_in s_in;
    prog
  with
  | (Parse.Error err) as exc -> 
    Parse.report_error err;
    raise exc
  | Corelang.Error (loc, err) as exc ->
    eprintf "Parsing error: %a%a@."
      Error.pp_error_msg err
      Location.pp_loc loc;
    raise exc

let expand_automata decls =
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. expanding automata@ ");
  try
    Automata.expand_decls decls
  with (Corelang.Error (loc, err)) as exc ->
    eprintf "Automata error: %a%a@."
      Error.pp_error_msg err
      Location.pp_loc loc;
    raise exc

let check_stateless_decls decls =
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. checking stateless/stateful status@ ");
  try
    Stateless.check_prog decls
  with (Stateless.Error (loc, err)) as exc ->
    eprintf "Stateless status error: %a%a@."
      Stateless.pp_error err
      Location.pp_loc loc;
    raise exc

let force_stateful_decls decls =
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. forcing stateful status@ ");
  try
    Stateless.force_prog decls
  with (Stateless.Error (loc, err)) as exc ->
    eprintf "Stateless status error: %a%a@."
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
	eprintf "Typing error: %a%a@."
	  Types.pp_error err
	  Location.pp_loc loc;
	raise exc
    end 
  in
  if !Options.print_types || !Options.verbose_level > 2 then
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>  %a@]@ " Corelang.pp_prog_type decls);
  new_env
      
let clock_decls env decls = 
  Log.report ~level:1 (fun fmt -> fprintf fmt ".. clock calculus@ ");
  let new_env =
    begin
      try
	Clock_calculus.clock_prog env decls
      with (Clocks.Error (loc,err)) as exc ->
	eprintf "Clock calculus error: %a%a@." Clocks.pp_error err Location.pp_loc loc;
	raise exc
    end
  in
  if !Options.print_clocks  || !Options.verbose_level > 2 then
    Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 2>  %a@]@ " Corelang.pp_prog_clock decls);
  new_env

(* Typing/Clocking with an empty env *)
let check_top_decls header =
  let new_tenv = type_decls Basic_library.type_env header in   (* Typing *)
  let new_cenv = clock_decls Basic_library.clock_env header in   (* Clock calculus *)
  header, new_tenv, new_cenv

let get_envs_from_const const_decl (ty_env, ck_env) =
  (Env.add_value ty_env const_decl.const_id const_decl.const_type,
   Env.add_value ck_env const_decl.const_id (Clocks.new_var true))

let get_envs_from_consts const_decls (ty_env, ck_env) =
  List.fold_right get_envs_from_const const_decls (ty_env, ck_env)

let rec get_envs_from_top_decl (ty_env, ck_env) top_decl =
 match top_decl.top_decl_desc with
 | Node nd          -> (Env.add_value ty_env nd.node_id nd.node_type,
			Env.add_value ck_env nd.node_id nd.node_clock)
 | ImportedNode ind -> (Env.add_value ty_env ind.nodei_id ind.nodei_type,
			Env.add_value ck_env ind.nodei_id ind.nodei_clock)
 | Const c          -> get_envs_from_const c (ty_env, ck_env)
 | TypeDef _        -> List.fold_left get_envs_from_top_decl (ty_env, ck_env) (consts_of_enum_type top_decl)
 | Open _           -> (ty_env, ck_env)

(* get type and clock environments from a header *)
let get_envs_from_top_decls header =
  List.fold_left get_envs_from_top_decl (Env.initial, Env.initial) header

(*
 List.fold_right
   (fun top_decl (ty_env, ck_env) ->
     match top_decl.top_decl_desc with
     | Node nd          -> (Env.add_value ty_env nd.node_id nd.node_type,
			    Env.add_value ck_env nd.node_id nd.node_clock)
     | ImportedNode ind -> (Env.add_value ty_env ind.nodei_id ind.nodei_type,
			    Env.add_value ck_env ind.nodei_id ind.nodei_clock)
     | Const c          -> get_envs_from_const c (ty_env, ck_env)
     | TypeDef _        -> List.fold_left (fun envs top -> consts_of_enum_type top_decl
     | Open _           -> (ty_env, ck_env))
   header
   (Env.initial, Env.initial)
 *)

let generate_lusic_header destname lusic_ext =	
  match !Options.output with
  | "C" -> C_backend_lusic.print_lusic_to_h destname lusic_ext
  | _ -> ()
	 

    
let check_compatibility (prog, computed_types_env, computed_clocks_env) (header, declared_types_env, declared_clocks_env) =
  try
    (* checking defined types are compatible with declared types*)
    Typing.check_typedef_compat header;

    (* checking type compatibility with computed types*)
    Typing.check_env_compat header declared_types_env computed_types_env;

    (* checking clocks compatibility with computed clocks*)
    Clock_calculus.check_env_compat header declared_clocks_env computed_clocks_env;

    (* checking stateless status compatibility *)
    Stateless.check_compat header
  with
  | (Types.Error (loc,err)) as exc ->
    eprintf "Type mismatch between computed type and declared type in lustre interface file: %a%a@."
      Types.pp_error err
      Location.pp_loc loc;
    raise exc
  | Clocks.Error (loc, err) as exc ->
    eprintf "Clock mismatch between computed clock and declared clock in lustre interface file: %a%a@."
      Clocks.pp_error err
      Location.pp_loc loc;
    raise exc
  | Stateless.Error (loc, err) as exc ->
    eprintf "Stateless status mismatch between defined status and declared status in lustre interface file: %a%a@."
      Stateless.pp_error err
      Location.pp_loc loc;
    raise exc

let is_stateful topdecl =
  match topdecl.top_decl_desc with
  | Node nd -> (match nd.node_stateless with Some b -> not b | None -> not nd.node_dec_stateless)
  | ImportedNode nd -> not nd.nodei_stateless 
  | _ -> false

(* Beware of the side effect: reads and modifies Global.(type_env/clock_env) *)
let rec import_dependencies prog : dep_t list =
  Log.report ~level:1 (fun fmt -> fprintf fmt "@[<v 4>.. extracting dependencies");
  let dependencies = Corelang.get_dependencies prog in
  let (compilation_deps, type_env, clock_env) =
  List.fold_left
    (fun (compilation_dep, type_env, clock_env) dep ->
      let (local, s) = Corelang.dependency_of_top dep in
      let basename = Options_management.name_dependency (local, s) in
      Log.report ~level:1 (fun fmt -> Format.fprintf fmt "@ Library %s@ " basename);
      let lusic = Modules.import_dependency dep.top_decl_loc (local, s) in
      (*Log.report ~level:1 (fun fmt -> Format.fprintf fmt "");*)
      let lusic_deps = import_dependencies lusic.Lusic.contents in
      let (lusi_type_env, lusi_clock_env) = get_envs_from_top_decls lusic.Lusic.contents in
      let is_stateful = List.exists is_stateful lusic.Lusic.contents in
      let new_dep = Dep (local, s, lusic.Lusic.contents, is_stateful ) in
      new_dep::lusic_deps@compilation_dep,
      Env.overwrite type_env lusi_type_env,
      Env.overwrite clock_env lusi_clock_env)
    ([], !Global.type_env, !Global.clock_env)
    dependencies in
  Global.type_env := type_env; 
  Global.clock_env := clock_env;
  begin
    Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");
    compilation_deps
  end

let track_exception () =
  if !Options.track_exceptions
  then (Printexc.print_backtrace stdout; flush stdout)
  else ()


let update_vdecl_parents_prog prog =
  let update_vdecl_parents parent v =
    v.var_parent_nodeid <- Some parent
  in
  List.iter (
    fun top -> match top.top_decl_desc with
    | Node nd ->
       List.iter
	 (update_vdecl_parents nd.node_id)
	 (nd.node_inputs @ nd.node_outputs @ nd.node_locals )  
    | ImportedNode ind -> 
       List.iter
	 (update_vdecl_parents ind.nodei_id)
	 (ind.nodei_inputs @ ind.nodei_outputs )  
    | _ -> ()
  ) prog
