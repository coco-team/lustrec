(* Backend-specific options *)
let join_guards = ref true

let setup () =
  match !Options.output with
  | "emf" ->
     (* In case of a default "int" type, substitute it with the legal int32 value *)
     if !Options.int_type = "int" then
       Options.int_type := "int32"
  | _ -> ()

let is_functional () = 
  match !Options.output with
  | "horn" | "lustre" | "acsl" | "emf" -> true
  | _ -> false


(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
