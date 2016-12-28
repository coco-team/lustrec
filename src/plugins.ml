open LustreSpec

module type PluginType =
sig

end



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
