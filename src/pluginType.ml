module type PluginType =
sig
  val name: string
  val activate: unit -> unit
  val options: (string * Arg.spec * string) list
  val check_force_stateful : unit -> bool
  val refine_machine_code: LustreSpec.top_decl list ->
    Machine_code.machine_t list -> Machine_code.machine_t list
  val c_backend_main_loop_body_prefix : string -> string -> Format.formatter ->  unit -> unit
  val c_backend_main_loop_body_suffix : Format.formatter ->  unit -> unit
end

module Default =
struct
  let check_force_stateful () = false
  let refine_machine_code prog machines = machines
  let c_backend_main_loop_body_prefix basename mname fmt () = ()
  let c_backend_main_loop_body_suffix fmt () = ()
end
