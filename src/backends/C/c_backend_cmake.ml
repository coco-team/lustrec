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
open LustreSpec
open Corelang

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
  let compiled_dep = compiled_dependencies dep in
  List.iter (fun s -> (* Format.eprintf "Adding dependency: %s@." s;  *)
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


  let print_cmake basename nodename (dependencies:  dep_t list) fmt =

    (* Printing the basic file CMakeLists.txt *)
    let fmt_CMakeLists_txt = formatter_of_out_channel (open_out (!Options.dest_dir ^ "/CMakeLists.txt")) in
    fprintf fmt_CMakeLists_txt "cmake_minimum_required(VERSION 3.0)@.";
    fprintf fmt_CMakeLists_txt "project(%s C)@." basename;
    fprintf mt_CMakeLists_txt "@.";
    fprintf mt_CMakeLists_txt "set(LUSTREC_DEFINE_TARGETS ON)@.";
    fprintf mt_CMakeLists_txt "include(lustrec-%s.cmake)" basename;

    
    fprintf fmt "GCC=gcc@.";
    fprintf fmt "LUSTREC=%s@." Sys.executable_name;
    fprintf fmt "LUSTREC_BASE=%s@." (Filename.dirname (Filename.dirname Sys.executable_name));
    fprintf fmt "INC=${LUSTREC_BASE}/include/lustrec@.";
    fprintf fmt "@.";

    (* Main binary *)
    fprintf fmt "%s_%s: %s.c %s_main.c@." basename nodename basename basename;
    fprintf fmt "\t${GCC} -O0 -I${INC} -I. -c %s.c@." basename;  
    fprintf fmt "\t${GCC} -O0 -I${INC} -I. -c %s_main.c@." basename;   
    fprintf_dependencies fmt dependencies;    
    fprintf fmt "\t${GCC} -O0 -o %s_%s io_frontend.o %a %s.o %s_main.o %a@." basename nodename 
      (Utils.fprintf_list ~sep:" " (fun fmt (Dep (_, s, _, _)) -> Format.fprintf fmt "%s.o" s)) 
      (compiled_dependencies dependencies)
      basename (* library .o *)
      basename (* main function . o *) 
      (Utils.fprintf_list ~sep:" " (fun fmt lib -> fprintf fmt "-l%s" lib)) (lib_dependencies dependencies)
    ;
    fprintf fmt "@.";
    fprintf fmt "clean:@.";
    fprintf fmt "\t\\rm -f *.o %s_%s@." basename nodename;
    fprintf fmt "@.";
    fprintf fmt ".PHONY: %s_%s@." basename nodename;
    fprintf fmt "@.";
    Mod.other_targets fmt basename nodename dependencies;
    fprintf fmt "@.";

end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
