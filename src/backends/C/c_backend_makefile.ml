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
open Lustre_types
open Corelang

let pp_dep fmt (Dep(b,id,tops,stateful)) =
  Format.fprintf fmt "%b, %s, {%a}, %b"
    b id Printers.pp_prog tops stateful
  
let pp_deps fmt deps = Format.fprintf fmt "@[<v 0>%a@ @]" (Utils.fprintf_list ~sep:"@ ," pp_dep) deps

let header_has_code header =
  List.exists 
    (fun top -> 
      match top.top_decl_desc with
      | Const _ -> true 
      | ImportedNode nd -> nd.nodei_in_lib = []
      | _ -> false
    )
    header

let header_libs header =
  List.fold_left (fun accu top ->
    match top.top_decl_desc with
      | ImportedNode nd -> Utils.list_union nd.nodei_in_lib accu
      | _ -> accu 
  ) [] header 
    

let compiled_dependencies dep = 
  List.filter (fun (Dep (_, _, header, _)) -> header_has_code header) dep

let lib_dependencies dep = 
  List.fold_left 
    (fun accu (Dep (_, _, header, _)) -> Utils.list_union (header_libs header) accu) [] dep
    
let fprintf_dependencies fmt (dep: dep_t list) =
  (* Format.eprintf "Deps: %a@." pp_deps dep; *)
  let compiled_dep = compiled_dependencies dep in
  (* Format.eprintf "Compiled Deps: %a@." pp_deps compiled_dep; *)
 
  List.iter (fun s -> Log.report ~level:1 (fun fmt -> fprintf fmt "Adding dependency: %s@." s);  
    fprintf fmt "\t${GCC} -I${INC} -c %s@." s)
    (("${INC}/io_frontend.c"):: (* IO functions when a main function is computed *)
	(List.map 
	   (fun (Dep (local, s, _, _)) -> 
	     (if local then s else Version.include_path ^ "/" ^ s) ^ ".c")
	   compiled_dep))

module type MODIFIERS_MKF =
sig (* dep was (bool * ident * top_decl list) *)
  val other_targets: Format.formatter -> string -> string -> dep_t list -> unit
end

module EmptyMod =
(struct
  let other_targets _ _ _ _ = ()
end: MODIFIERS_MKF)

module Main = functor (Mod: MODIFIERS_MKF) -> 
struct

(* TODO: BEWARE OF THE BUG !!  in case of very long nodename or maybe even
   basename, the string basename_nodename exceed a limit length for files on linux
   and prevent gcc to generate the binary of that name.

To be solved (later) with
- either the default name if it short
- a shorter version if it is long (md5?)
- a provided name given when calling the makefile so the user can control the 
  expected name of the binary

*)

  let print_makefile basename nodename (dependencies:  dep_t list) fmt =
    let binname =
      let s = basename ^ "_" ^ nodename in
      if String.length s > 100 (* seems that GCC fails from 144 characters and
				  on: File name too long collect2 ld error. *)
      then
	if String.length nodename > 100 then
	  basename ^ "_run" (* shorter version *)
	else
	  nodename ^ "run"
      else
	s
    in
    fprintf fmt "BINNAME?=%s@." binname;
    fprintf fmt "GCC=gcc -O0@.";
    fprintf fmt "LUSTREC=%s@." Sys.executable_name;
    fprintf fmt "LUSTREC_BASE=%s@." (Filename.dirname (Filename.dirname Sys.executable_name));
    fprintf fmt "INC=${LUSTREC_BASE}/include/lustrec@.";
    fprintf fmt "@.";

    (* Main binary *)
    fprintf fmt "%s_%s: %s.c %s_main.c@." basename "run"(*nodename*) basename basename;
    fprintf fmt "\t${GCC} -I${INC} -I. -c %s.c@." basename;  
    fprintf fmt "\t${GCC} -I${INC} -I. -c %s_main.c@." basename;   
    fprintf_dependencies fmt dependencies;    
    fprintf fmt "\t${GCC} -o ${BINNAME} io_frontend.o %a %s.o %s_main.o %a@." 
      (Utils.fprintf_list ~sep:" " (fun fmt (Dep (_, s, _, _)) -> Format.fprintf fmt "%s.o" s)) 
      (compiled_dependencies dependencies)
      basename (* library .o *)
      basename (* main function . o *) 
      (Utils.fprintf_list ~sep:" " (fun fmt lib -> fprintf fmt "-l%s" lib)) (lib_dependencies dependencies)
    ;
    fprintf fmt "@.";
    fprintf fmt "clean:@.";
    fprintf fmt "\t\\rm -f *.o ${BINNAME}@.";
    fprintf fmt "@.";
    Mod.other_targets fmt basename nodename dependencies;
    fprintf fmt "@.";

end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
