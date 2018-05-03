open Lustre_types

open PluginList


let options () = 
  List.flatten (
    List.map Options_management.plugin_opt (
      List.map (fun m ->
	let module M = (val m : PluginType.PluginType) in
	(M.name, M.activate, M.options)
      ) plugins
    ))

let check_force_stateful () =
  List.exists (fun m ->
	let module M = (val m : PluginType.PluginType) in
	M.check_force_stateful ()
  ) plugins

let refine_machine_code prog machine_code =
  List.fold_left (fun accu m ->
    let module M = (val m : PluginType.PluginType) in
    M.refine_machine_code prog accu
  ) machine_code plugins


let c_backend_main_loop_body_prefix basename mname fmt () = 
  List.iter (fun (m: (module PluginType.PluginType)) -> 
    let module M = (val m : PluginType.PluginType) in
    M.c_backend_main_loop_body_prefix basename mname fmt ()) plugins

let c_backend_main_loop_body_suffix fmt () = 
  List.iter (fun (m: (module PluginType.PluginType)) -> 
    let module M = (val m : PluginType.PluginType) in
    M.c_backend_main_loop_body_suffix fmt ()) plugins

(* Specific treatment of annotations when inlining, specific of declared plugins *)

let inline_annots rename_var_fun annot_list =
  List.map (
    fun ann -> 
      { ann with 
	annots = List.fold_left (
	  fun accu (sl, eexpr) -> 
	    let items = 
	      match sl with 
	      | plugin_name::args -> 
		if plugin_name = "salsa" then
		  match args with
		  | ["ranges";varname] -> 
		    [["salsa";"ranges";(rename_var_fun varname)], eexpr]
		  | _ -> [(sl, eexpr)]
		else
		  [(sl, eexpr)]
	    | _ -> assert false
	    in
	    items@accu
	) [] ann.annots
      }
  ) annot_list

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
