open Format
open LustreSpec
open Corelang

let header_has_code header =
  List.exists 
    (fun top -> 
      match top.top_decl_desc with
      | Consts _ -> true 
      | ImportedNode nd -> nd.nodei_in_lib = None
      | _ -> false
    )
    header

let header_libs header =
  List.fold_left (fun accu top ->
    match top.top_decl_desc with
      | ImportedNode nd -> (match nd.nodei_in_lib with 
	| None -> accu 
	| Some lib -> Utils.list_union [lib] accu)
      | _ -> accu 
  ) [] header 
    

let compiled_dependencies dep = 
  List.filter (fun (_, _, header) -> header_has_code header) dep

let lib_dependencies dep = 
  List.fold_left 
    (fun accu (_, _, header) -> Utils.list_union (header_libs header) accu) [] dep
    
let fprintf_dependencies fmt dep =
  let compiled_dep = compiled_dependencies dep in
  List.iter (fun s -> (* Format.eprintf "Adding dependency: %s@." s;  *)
    fprintf fmt "\t${GCC} -I${INC} -c %s@." s)
    (("${INC}/io_frontend.c"):: (* IO functions when a main function is computed *)
	(List.map 
	   (fun (s, local, _) -> 
	     (if local then s else Version.prefix ^ "/include/lustrec/" ^ s) ^ ".c")
	   compiled_dep))

module type MODIFIERS_MKF =
sig
  val other_targets: Format.formatter -> string -> string -> (string * bool * top_decl list) list -> unit
end

module EmptyMod =
struct
  let other_targets _ _ _ _ = ()
end

module Main = functor (Mod: MODIFIERS_MKF) -> 
struct


let print_makefile basename nodename dependencies fmt =
  fprintf fmt "GCC=gcc@.";
  fprintf fmt "LUSTREC=%s@." Sys.executable_name;
  fprintf fmt "LUSTREC_BASE=%s@." (Filename.dirname (Filename.dirname Sys.executable_name));
  fprintf fmt "INC=${LUSTREC_BASE}/include/lustrec@.";
  fprintf fmt "@.";

  (* Main binary *)
  fprintf fmt "%s_%s:@." basename nodename;
  fprintf fmt "\t${GCC} -I${INC} -I. -c %s.c@." basename;  
  fprintf fmt "\t${GCC} -I${INC} -I. -c %s_main.c@." basename;   
  fprintf_dependencies fmt dependencies;    
  fprintf fmt "\t${GCC} -o %s_%s io_frontend.o %a %s.o %s_main.o %a@." basename nodename 
    (Utils.fprintf_list ~sep:" " (fun fmt (s, _, _) -> Format.fprintf fmt "%s.o" s)) (compiled_dependencies dependencies)
    basename (* library .o *)
    basename (* main function . o *) 
    (Utils.fprintf_list ~sep:" " (fun fmt lib -> fprintf fmt "-l%s" lib)) (lib_dependencies dependencies)
    ;
 fprintf fmt "@.";
 fprintf fmt "clean:@.";
 fprintf fmt "\t\\rm -f *.o %s_%s@." basename nodename;
 fprintf fmt "@.";
 Mod.other_targets fmt basename nodename dependencies;
 fprintf fmt "@.";

end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
