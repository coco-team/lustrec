open Format

type ident = Lustre_types.ident
type error_kind =
    Main_not_found
  | Main_wrong_kind
  | No_main_specified
  | Unbound_symbol of ident
  | Already_bound_symbol of ident
  | Unknown_library of ident
  | Wrong_number of ident
  | AlgebraicLoop

let return_code kind =
  match kind with
  | Main_not_found -> 2
  | Main_wrong_kind -> 3
  | No_main_specified -> 4
  | Unbound_symbol _ -> 5
  | Already_bound_symbol _ -> 6
  | Unknown_library _ -> 7
  | Wrong_number _ -> 8
  | AlgebraicLoop -> 9


  let pp_error_msg fmt = function
  | Main_not_found ->
      fprintf fmt "Could not find the definition of main node %s.@."
	!Global.main_node
  | Main_wrong_kind ->
    fprintf fmt
      "Node %s does not correspond to a valid main node definition.@." 
      !Global.main_node 
  | No_main_specified ->
    fprintf fmt "No main node specified (use -node option)@."
  | Unbound_symbol sym ->
    fprintf fmt
      "%s is undefined.@."
      sym
  | Already_bound_symbol sym -> 
    fprintf fmt
      "%s is already defined.@."
      sym
  | Unknown_library sym ->
    fprintf fmt
      "impossible to load library %s.lusic.@.Please compile the corresponding interface or source file.@."
      sym
  | Wrong_number sym ->
    fprintf fmt
      "library %s.lusic has a different version number and may crash compiler.@.Please recompile the corresponding interface or source file.@."
      sym
  | AlgebraicLoop  -> assert false (* should have been handled yet *)
     
let pp_warning loc pp_msg =
  Format.eprintf "%a@.Warning: %t@."
    Location.pp_loc loc
    pp_msg

let pp_error loc pp_msg =
  Format.eprintf "@.%a@.Error: @[<v 0>%t@]@."
    Location.pp_loc loc
    pp_msg

    
