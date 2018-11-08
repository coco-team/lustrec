module Types = Types.Main

let type_env : (Types.type_expr Env.t) ref = ref Basic_library.type_env
let clock_env : (Clocks.clock_expr Env.t) ref = ref Basic_library.clock_env
let basename = ref ""
let main_node = ref ""

module TypeEnv =
struct
let lookup_value ident = Env.lookup_value !type_env ident
let exists_value ident = Env.exists_value !type_env ident
let iter f = Env.iter !type_env f
let pp pp_fun fmt () = Env.pp_env pp_fun fmt !type_env
end

let initialize () =
  begin
    main_node := !Options.main_node;
  end

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
