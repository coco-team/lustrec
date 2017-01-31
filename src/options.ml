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
let include_dir = ref "."
let include_path =
if (!include_dir != ".") then Version.prefix ^ !include_dir
else Version.include_path




let print_version () =
  Format.printf "Lustrec compiler, version %s (%s)@." version codename;
  Format.printf "Include directory: %s@." include_path;
  Format.printf "User selected Include directory: %s@." !include_dir

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

let salsa_enabled = ref true

let sfunction = ref ""

let set_mpfr prec =
  if prec > 0 then (
    mpfr := true;
    mpfr_prec := prec;
    salsa_enabled := false; (* We deactivate salsa *)
  )
  else
    failwith "mpfr requires a positive integer"
			
let options =
[ "-d", Arg.Set_string dest_dir,
"uses the specified directory \x1b[4mdir\x1b[0m as root for generated/imported object and C files <default: .>";
"-I", Arg.Set_string include_dir, "Include directory";
    "-node", Arg.Set_string main_node, "specifies the \x1b[4mmain\x1b[0m node";
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
    "-horn-traces", Arg.Unit (fun () -> output := "horn"; traces:=true), "produce traceability file for Horn backend. Enable the horn backend.";
    "-horn-cex", Arg.Unit (fun () -> output := "horn"; horn_cex:=true), "generate cex enumeration. Enable the horn backend (work in progress)";
    "-horn-query", Arg.Unit (fun () -> output := "horn"; horn_query:=true), "generate queries in generated Horn file. Enable the horn backend (work in progress)";
    "-horn-sfunction", Arg.Set_string sfunction, "Get the endpoint predicate of the sfunction";
    "-print_reuse", Arg.Set print_reuse, "prints variable reuse policy";
    "-lustre", Arg.Unit (fun () -> output := "lustre"), "generates Lustre output, performing all active optimizations";
    "-emf", Arg.Unit (fun () -> output := "emf"), "generates EMF output, to be used by CocoSim";
    "-inline", Arg.Unit (fun () -> global_inline := true; const_unfold := true), "inline all node calls (require a main node). Implies constant unfolding";
    "-witnesses", Arg.Set witnesses, "enable production of witnesses during compilation";
    "-print_types", Arg.Set print_types, "prints node types";
    "-print_clocks", Arg.Set print_clocks, "prints node clocks";
    "-O", Arg.Set_int optimization, "changes optimization \x1b[4mlevel\x1b[0m <default: 2>";
    "-verbose", Arg.Set_int verbose_level, "changes verbose \x1b[4mlevel\x1b[0m <default: 1>";
    "-version", Arg.Unit print_version, " displays the version";]


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
