
  
type backend = GenLus | GenImp

(* Model choice *)
let model_name = ref "stopwatch" (*"simple"*)

let models = [(module Model_simple : Datatype.MODEL_T);
	      (module Model_stopwatch : Datatype.MODEL_T);
	      (module Model_medium : Datatype.MODEL_T)
	     ]
let get_model_name m = let module M = (val m : Datatype.MODEL_T) in M.name
let set_model name = 
  if List.exists (fun n -> get_model_name n = name) models then
    model_name := name
  else failwith ("incorrect model name. Use " ^
		    (List.fold_left (fun r n -> r ^ " or " ^ get_model_name n) "" models))

(* Backend selection *)
let modular = ref 0
let set_modular i = modular := i

let mode = ref GenLus
  
     
let set_mode m =
  mode := m

(* Main *)
    
let options = [
  "-model", Arg.String set_model, "model in {simple, stopwatch} (default: simple)";
  (* "-eval", Arg.Int set_trace_run_mode, "execute the model on trace <int>"; *)
  (* "-eval-mode", Arg.String set_eval_mode, "select evaluator: cps"; *)
  "-gen_c", Arg.Unit (fun _ -> set_mode GenImp), "generate imperative code";
  "-gen_lustre", Arg.Unit (fun _ -> set_mode GenLus), "generate lustre model";
  "-modular", Arg.Int set_modular, "generate modular code (either for imperative or lustre backend) 0 is not modular, 1 modularize nodes, 2 modularize entry, during and exit actions (default 0)"
]

let usage =
  "lustresf [JSON file] takes as input a stateflow model in the JSON format and a backend.\n"^
    "Backends are eother the C code generator or the lustre code generator."

    
let _ =
  Arg.parse options (fun _ -> ()) usage;
  let model = List.find (fun m -> get_model_name m = !model_name) models in
  let modularmode =
    match !modular with
    | 2 -> true, true, true
    | 1 -> false, true, false
    | _ (* 0 *) -> false, false ,false
  in
  match !mode with
  | GenImp -> (
    let module Model = (val model) in
    let module T = CPS_ccode_generator.CodeGenerator in
    let module Sem = CPS.Semantics (T) (Model) in
    Sem.code_gen Format.std_formatter modularmode
  )				     
  | GenLus ->
     let module Model = (val model) in
     let state_vars = Datatype.SF.states Model.model in
     let global_vars = Datatype.SF.global_vars Model.model in
     
     let module T = CPS_lustre_generator.LustrePrinter (struct
       let state_vars = state_vars
       let global_vars = global_vars 
     end) in
     let module Sem = CPS.Semantics (T) (Model) in
     Sem.code_gen Format.std_formatter modularmode


  
