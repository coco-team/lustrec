(* Backend-specific options *)
let join_guards = ref true

let setup () =
  match !Options.output with
  (* | "emf" -> *)
  (*    join_guards := true; (\* guards should not be joined, in order to have only *)
  (* 			      if c then x = e1 else x = e2 to ease *)
  (* 			      reconstruction of flows. *\) *)
  (*   Options.optimization := 0; (\* Optimization=0 prevents expression *)
  (* 				  elimination. This simplifies largely the *)
  (* 				  association of lustre expression to *)
  (* 				  instructions *\) *)
  | _ -> ()

let is_functional () = 
  match !Options.output with
  | "horn" | "lustre" | "acsl" | "emf" -> true
  | _ -> false

  
(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
