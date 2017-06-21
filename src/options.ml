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

let version = Version.number
let codename = Version.codename
let include_dirs = ref ["."]
(* let include_path = *)
(* if (!include_dir <> ".") then Version.prefix ^ !include_dir *)
(* else Version.include_path *)




let print_version () =
  Format.printf "Lustrec compiler, version %s (%s)@." version codename;
  Format.printf "Standard lib: %s@." Version.include_path;
  Format.printf "User provided include directory: @[<h>%a@]@."
    (Utils.fprintf_list ~sep:"@ " Format.pp_print_string) !include_dirs

let main_node = ref ""
let static_mem = ref true
let print_types = ref true
let print_clocks = ref true
let delay_calculus = ref true
let track_exceptions = ref true
let ansi = ref false
let check = ref false
let spec = ref "acsl"
let output = ref "C"
let dest_dir = ref "."
let verbose_level = ref 1
let global_inline = ref false
let witnesses = ref false
let optimization = ref 2
let lusi = ref false
let print_reuse = ref false
let const_unfold = ref false
let mpfr = ref false
let mpfr_prec = ref 0

let traces = ref false
let horn_cex = ref false
let horn_query = ref true

let cpp       = ref false
let int_type  = ref "int"
let real_type = ref "double"

let sfunction = ref ""

let mauve = ref ""
(* test generation options *)
let nb_mutants = ref 1000
let gen_mcdc = ref false
let no_mutation_suffix = ref false

let add_include_dir dir =
  let removed_slash_suffix =
    let len = String.length dir in
    if dir.[len-1] = '/' then
      String.sub dir 0 (len - 1) 
    else
      dir
  in
  include_dirs := removed_slash_suffix :: !include_dirs

    
(** Solving the path of required library:
    If local: look in the folders described in !Options.include_dirs
    If non local: look first as a local, then in Version.include_path:
    ie. in Version.include_path::!Options.include_dirs
    Note that in options.ml, include folder are added as heads. One need to
    perform a fold_right to respect the order
*)
let search_lib_path (local, full_file_name) =
  let paths = (if local then !include_dirs else Version.include_path::!include_dirs) in
  let name =
    List.fold_right (fun dir res ->
      match res with Some _ -> res
      | None ->
	 let path_to_lib = dir ^ "/" ^ full_file_name in 
	 if Sys.file_exists path_to_lib then
	   Some dir
	 else
	   None
    )
      paths
      None
  in
  match name with
  | None -> Format.eprintf "Unable to find library %s in paths %a@.@?" full_file_name (Utils.fprintf_list ~sep:", " Format.pp_print_string) paths;raise Not_found
  | Some s -> s

(* Search for path of core libs (without lusic: arrow and io_frontend *)
let core_dependency lib_name =
  search_lib_path (false, lib_name ^ ".h")
    
let name_dependency (local, dep) =
  let dir = search_lib_path (false, dep ^ ".lusic") in
  dir ^ "/" ^ dep
  
let set_mpfr prec =
  if prec > 0 then (
    mpfr := true;
    mpfr_prec := prec;
    (* salsa_enabled := false; (* We deactivate salsa *) TODO *)
  )
  else
    failwith "mpfr requires a positive integer"

let common_options =
  [ "-d", Arg.Set_string dest_dir, "uses the specified \x1b[4mdirectory\x1b[0m as root for generated/imported object and C files <default: .>";
    "-I", Arg.String add_include_dir, "sets include \x1b[4mdirectory\x1b[0m";
    "-node", Arg.Set_string main_node, "specifies the \x1b[4mmain\x1b[0m node";
    "-print-types", Arg.Set print_types, "prints node types";
    "-print-clocks", Arg.Set print_clocks, "prints node clocks";
    "-verbose", Arg.Set_int verbose_level, "changes verbose \x1b[4mlevel\x1b[0m <default: 1>";
    "-version", Arg.Unit print_version, " displays the version";
  ]

let lustrec_options =
   common_options @
  [ 
    "-init", Arg.Set delay_calculus, "performs an initialisation analysis for Lustre nodes <default: no analysis>";
    "-dynamic", Arg.Clear static_mem, "specifies a dynamic allocation scheme for main Lustre node <default: static>";
    "-check-access", Arg.Set check, "checks at runtime that array accesses always lie within bounds <default: no check>";
    "-mpfr", Arg.Int set_mpfr, "replaces FP numbers by the MPFR library multiple precision numbers with a precision of \x1b[4mprec\x1b[0m bits <default: keep FP numbers>";
    "-lusi", Arg.Set lusi, "only generates a .lusi interface source file from a Lustre source <default: no generation>";
    "-no-spec", Arg.Unit (fun () -> spec := "no"), "do not generate any specification";
    "-acsl-spec", Arg.Unit (fun () -> spec := "acsl"), "generates an ACSL encoding of the specification. Only meaningful for the C backend <default>";
    "-c-spec", Arg.Unit (fun () -> spec := "c"), "generates a C encoding of the specification instead of ACSL contracts and annotations. Only meaningful for the C backend";
    (* "-java", Arg.Unit (fun () -> output := "java"), "generates Java output instead of C"; *)
    "-horn", Arg.Unit (fun () -> output := "horn"), "generates Horn clauses encoding output instead of C";
    "-horn-traces", Arg.Unit (fun () -> output := "horn"; traces:=true), "produces traceability file for Horn backend. Enable the horn backend.";
    "-horn-cex", Arg.Unit (fun () -> output := "horn"; horn_cex:=true), "generates cex enumeration. Enable the horn backend (work in progress)";
    "-horn-query", Arg.Unit (fun () -> output := "horn"; horn_query:=true), "generates queries in generated Horn file. Enable the horn backend (work in progress)";
    "-horn-sfunction", Arg.Set_string sfunction, "gets the endpoint predicate of the \x1b[4msfunction\x1b[0m";
    "-print-reuse", Arg.Set print_reuse, "prints variable reuse policy";
    "-lustre", Arg.Unit (fun () -> output := "lustre"), "generates Lustre output, performing all active optimizations";
   "-emf", Arg.Unit (fun () -> output := "emf"), "generates EMF output, to be used by CocoSim";
   "-inline", Arg.Unit (fun () -> global_inline := true; const_unfold := true), "inlines all node calls (require a main node). Implies constant unfolding";
    "-witnesses", Arg.Set witnesses, "enables production of witnesses during compilation";
    "-O", Arg.Set_int optimization, "changes optimization \x1b[4mlevel\x1b[0m <default: 2>";
    "-verbose", Arg.Set_int verbose_level, "changes verbose \x1b[4mlevel\x1b[0m <default: 1>";
    
    "-c++" , Arg.Set        cpp      , "c++ backend";
    "-int" , Arg.Set_string int_type , "specifies the integer type (default=\"int\")";
    "-real", Arg.Set_string real_type, "specifies the real type (default=\"double\" without mpfr option)";

    "-mauve", Arg.String (fun node -> mauve := node; cpp := true; static_mem := false), "generates the mauve code";
]

let lustret_options =
  common_options @
  [ "-nb-mutants", Arg.Set_int nb_mutants, "\x1b[4mnumber\x1b[0m of mutants to produce <default: 1000>";
    "-mcdc-cond", Arg.Set gen_mcdc, "generates MC/DC coverage";
    "-no-mutation-suffix", Arg.Set no_mutation_suffix, "does not rename node with the _mutant suffix"
  ]

let plugin_opt (name, activate, options) =
  ( "-" ^ name , Arg.Unit activate, "activate plugin " ^ name ) ::
    (List.map (fun (opt, act, desc) -> "-" ^ name ^ opt, act, desc) options)
 

let get_witness_dir filename =
  (* Make sure the directory exists *)
  let dir = !dest_dir ^ "/" ^ (Filename.basename filename) ^ "_witnesses" in
  let _ = try
	    if not (Sys.is_directory dir) then (
	      Format.eprintf "File of name %s exists. It should be a directory.@." dir;
	      exit 1
	    )
    with Sys_error _ -> Unix.mkdir dir 0o750
  in
  dir

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
