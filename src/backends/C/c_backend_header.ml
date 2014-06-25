open Format 
open LustreSpec
open Corelang
open Machine_code
open C_backend_common

(********************************************************************************************)
(*                         Header Printing functions                                        *)
(********************************************************************************************)


module type MODIFIERS_HDR =
sig
  val print_machine_decl_prefix: Format.formatter -> Machine_code.machine_t -> unit
end

module EmptyMod =
struct
  let print_machine_decl_prefix = fun fmt x -> ()
end

module Main = functor (Mod: MODIFIERS_HDR) -> 
struct

let print_import_standard fmt =
  fprintf fmt "#include \"%s/include/lustrec/arrow.h\"@.@." Version.prefix

    
let pp_registers_struct fmt m =
  if m.mmemory <> []
  then
    fprintf fmt "@[%a {@[%a; @]}@] _reg; "
      pp_machine_regtype_name m.mname.node_id
      (Utils.fprintf_list ~sep:"; " pp_c_decl_struct_var) m.mmemory
  else
    ()

let print_machine_struct fmt m =
  if fst (get_stateless_status m) then
    begin
    end
  else
    begin
      (* Define struct *)
      fprintf fmt "@[%a {@[%a%a%t@]};@]@."
	pp_machine_memtype_name m.mname.node_id
	pp_registers_struct m
	(Utils.fprintf_list ~sep:"; " pp_c_decl_instance_var) m.minstances
	(Utils.pp_final_char_if_non_empty "; " m.minstances)
    end

let print_static_declare_instance attr fmt (i, (m, static)) =
  fprintf fmt "%a(%s, %a%t%s)"
    pp_machine_static_declare_name (node_name m)
    attr
    (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) static
    (Utils.pp_final_char_if_non_empty ", " static)
    i

let print_static_declare_macro fmt m =
  let array_mem = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  let inst = mk_instance m in
  let attr = mk_attribute m in
  fprintf fmt "@[<v 2>#define %a(%s, %a%t%s)\\@,%s %a %s;\\@,%a%t%a;@,@]"
    pp_machine_static_declare_name m.mname.node_id
    attr
    (Utils.fprintf_list ~sep:", " (pp_c_var_read m)) m.mstatic
    (Utils.pp_final_char_if_non_empty ", " m.mstatic)
    inst
    attr
    pp_machine_memtype_name m.mname.node_id
    inst
    (Utils.fprintf_list ~sep:";\\@," pp_c_decl_local_var) array_mem
    (Utils.pp_final_char_if_non_empty ";\\@," array_mem)
    (Utils.fprintf_list ~sep:";\\@,"
       (fun fmt (i',m') ->
	 let path = sprintf "inst ## _%s" i' in
	 fprintf fmt "%a"
	   (print_static_declare_instance attr) (path,m')
       )) m.minstances

      
let print_static_link_instance fmt (i, (m, _)) =
 fprintf fmt "%a(%s)" pp_machine_static_link_name (node_name m) i

(* Allocation of a node struct:
   - if node memory is an array/matrix/etc, we cast it to a pointer (see pp_registers_struct)
*)
let print_static_link_macro fmt m =
  let array_mem = List.filter (fun v -> Types.is_array_type v.var_type) m.mmemory in
  fprintf fmt "@[<v>@[<v 2>#define %a(inst) do {\\@,%a%t%a;\\@]@,} while (0)@.@]"
    pp_machine_static_link_name m.mname.node_id
    (Utils.fprintf_list ~sep:";\\@,"
       (fun fmt v ->
	 fprintf fmt "inst._reg.%s = (%a*) &%s"
	   v.var_id
           (fun fmt v -> pp_c_type "" fmt (Types.array_base_type v.var_type)) v
	   v.var_id
       )) array_mem
    (Utils.pp_final_char_if_non_empty ";\\@," array_mem)
    (Utils.fprintf_list ~sep:";\\@,"
       (fun fmt (i',m') ->
	 let path = sprintf "inst ## _%s" i' in
	 fprintf fmt "%a;\\@,inst.%s = &%s"
	   print_static_link_instance (path,m')
	   i'
	   path
       )) m.minstances
      
let print_static_alloc_macro fmt m =
  fprintf fmt "@[<v>@[<v 2>#define %a(attr,%a%tinst)\\@,%a(attr,%a%tinst);\\@,%a(inst);@]@,@]@."
    pp_machine_static_alloc_name m.mname.node_id
    (Utils.fprintf_list ~sep:", " (pp_c_var_read m)) m.mstatic
    (Utils.pp_final_char_if_non_empty ", " m.mstatic)
    pp_machine_static_declare_name m.mname.node_id
    (Utils.fprintf_list ~sep:", " (pp_c_var_read m)) m.mstatic
    (Utils.pp_final_char_if_non_empty ", " m.mstatic)
    pp_machine_static_link_name m.mname.node_id

 
let print_machine_decl fmt m =
  Mod.print_machine_decl_prefix fmt m;
  if fst (get_stateless_status m) then
    begin
      fprintf fmt "extern %a;@.@."
	print_stateless_prototype
	(m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
    end
  else
    begin
      (* Static allocation *)
      if !Options.static_mem
      then (
	fprintf fmt "%a@.%a@.%a@."
	  print_static_declare_macro m
	  print_static_link_macro m
	  print_static_alloc_macro m
      )
      else ( 
        (* Dynamic allocation *)
	fprintf fmt "extern %a;@.@."
	  print_alloc_prototype (m.mname.node_id, m.mstatic)
      );
      let self = mk_self m in
      fprintf fmt "extern %a;@.@."
	(print_reset_prototype self) (m.mname.node_id, m.mstatic);

      fprintf fmt "extern %a;@.@."
	(print_step_prototype self)
	(m.mname.node_id, m.mstep.step_inputs, m.mstep.step_outputs)
    end

let print_const_decl fmt cdecl =
  fprintf fmt "extern %a;@." 
    (pp_c_type cdecl.const_id) cdecl.const_type

(********************************************************************************************)
(*                      Struct/TypeDef Printing functions                                   *)
(********************************************************************************************)


let rec pp_c_struct_type_field filename cpt fmt (label, tdesc) =
  fprintf fmt "%a;" (pp_c_type_decl filename cpt label) tdesc
and pp_c_type_decl filename cpt var fmt tdecl =
  match tdecl with
  | Tydec_any           -> assert false
  | Tydec_int           -> fprintf fmt "int %s" var
  | Tydec_real          -> fprintf fmt "double %s" var
  | Tydec_float         -> fprintf fmt "float %s" var
  | Tydec_bool          -> fprintf fmt "_Bool %s" var
  | Tydec_clock ty      -> pp_c_type_decl filename cpt var fmt ty
  | Tydec_const c       -> fprintf fmt "%s %s" c var
  | Tydec_array (d, ty) -> fprintf fmt "%a[%a]" (pp_c_type_decl filename cpt var) ty pp_c_dimension d
  | Tydec_enum tl ->
    begin
      incr cpt;
      fprintf fmt "enum _enum_%s_%d { %a } %s" filename !cpt (Utils.fprintf_list ~sep:", " pp_print_string) tl var
    end
  | Tydec_struct fl ->
    begin
      incr cpt;
      fprintf fmt "struct _struct_%s_%d { %a } %s" filename !cpt (Utils.fprintf_list ~sep:" " (pp_c_struct_type_field filename cpt)) fl var
    end

let print_type_definitions fmt filename =
  let cpt_type = ref 0 in
  Hashtbl.iter (fun typ def ->
    match typ with
    | Tydec_const var ->
      fprintf fmt "typedef %a;@.@."
	(pp_c_type_decl filename cpt_type var) def
    | _        -> ()) type_table

(********************************************************************************************)
(*                         MAIN Header Printing functions                                   *)
(********************************************************************************************)
let print_header header_fmt basename prog machines =
  (* Include once: start *)
  let baseNAME = String.uppercase basename in
  let baseNAME = Str.global_replace (Str.regexp "\\.\\|\\ ") "_" baseNAME in
  (* Print the svn version number and the supported C standard (C90 or C99) *)
  print_version header_fmt;
  fprintf header_fmt "#ifndef _%s@.#define _%s@." baseNAME baseNAME;
  pp_print_newline header_fmt ();
  fprintf header_fmt "/* Imports standard library */@.";
  (* imports standard library definitions (arrow) *)
  print_import_standard header_fmt;
  pp_print_newline header_fmt ();
  fprintf header_fmt "/* Types definitions */@.";
  (* Print the type definitions from the type table *)
  print_type_definitions header_fmt basename;
  pp_print_newline header_fmt ();
  (* Print the global constant declarations. *)
  fprintf header_fmt "/* Global constant (declarations, definitions are in C file) */@.";
  List.iter (fun c -> print_const_decl header_fmt c) (get_consts prog);
  pp_print_newline header_fmt ();
  (* Print the struct declarations of all machines. *)
  fprintf header_fmt "/* Struct declarations */@.";
  List.iter (print_machine_struct header_fmt) machines;
  pp_print_newline header_fmt ();
  (* Print the prototypes of all machines *)
  fprintf header_fmt "/* Nodes declarations */@.";
  List.iter (print_machine_decl header_fmt) machines;
  pp_print_newline header_fmt ();
  (* Include once: end *)
  fprintf header_fmt "#endif@.";
  pp_print_newline header_fmt ()
end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
