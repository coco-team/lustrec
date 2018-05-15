open Format 
open Lustre_types

let salsa_enabled = ref false
    (* "-salsa", Arg.Set salsa_enabled, "activate Salsa optimization <default>"; *)
    (* "-no-salsa", Arg.Clear salsa_enabled, "deactivate Salsa optimization"; *)


module Plugin =
(struct
  include PluginType.Default
  let name = "salsa"
  
  let options = [
        "-debug", Arg.Set SalsaDatatypes.debug, "debug salsa plugin";
        "-slice-depth", Arg.Set_int Salsa.Prelude.sliceSize, "salsa slice depth (default is 5)";
      ]

  let activate () = salsa_enabled := true

  let refine_machine_code prog machine_code = 
    if !salsa_enabled then
      begin
	Compiler_common.check_main ();
	Log.report ~level:1 (fun fmt -> fprintf fmt ".. @[<v 0>salsa machines optimization@ ");
	(* Selecting float constants for Salsa *)
	let constEnv = List.fold_left (
	  fun accu c_topdecl ->
	    match c_topdecl.top_decl_desc with
	    | Const c when Types.is_real_type c.const_type  ->
	      (c.const_id, c.const_value) :: accu
	    | _ -> accu
	) [] (Corelang.get_consts prog) 
	in
	let res =
	  List.map 
	    (Machine_salsa_opt.machine_t2machine_t_optimized_by_salsa constEnv) 
	    machine_code
	in
	Log.report ~level:1 (fun fmt -> fprintf fmt "@]@ ");
	res
      end
    else
      machine_code
  
  
 end: PluginType.PluginType)
