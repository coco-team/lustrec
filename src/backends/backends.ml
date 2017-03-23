(* Backend-specific options *)
let join_guards = ref true

let setup s =
  match s with
  | "emf" -> join_guards := false
  | _ -> ()
