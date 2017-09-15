
(*
(* CPS semantics *)
module CPS_Interpreter = CPS.Evaluator

(* Hamon's original semantics, EMSOFT'05 *)
module Orig_Interpreter
*)

type backend = Eval | GenLus | GenImp

(* Model choice *)
let model_name = ref "simple"

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

let mode = ref Eval
type eval_mode_t = CPSEval | EMSOFT05Eval | CompareEval
let eval_mode = ref CPSEval
let trace_id = ref 0
  
let set_trace_run_mode i =
  mode := Eval; trace_id := i

let set_eval_mode s =
  match s with
  | "cps" -> eval_mode := CPSEval
  | "emsoft05" -> eval_mode := EMSOFT05Eval
  | "compare" ->  eval_mode := CompareEval
  | _ -> assert false
     
let set_mode m =
  mode := m

(* Main *)
    
let options = [
  "-model", Arg.String set_model, "model in {simple, stopwatch} (default: simple)";
  "-eval", Arg.Int set_trace_run_mode, "execute the model on trace <int>";
  "-eval-mode", Arg.String set_eval_mode, "select evaluator: cps, emsoft05 or compare";
  "-gen_imp", Arg.Unit (fun _ -> set_mode GenImp), "generate imperative code";
  "-gen_lustre", Arg.Unit (fun _ -> set_mode GenLus), "generate lustre model";
  "-modular", Arg.Int set_modular, "generate modular code (either for imperative or lustre backend) 0 is not modular, 1 modularize nodes, 2 modularize entry, during and exit actions (default 0)"
]

let usage =
  "sf_sem takes as input a model name and a backend.\n"^
    "Backends are interpretor, imperative code generator, lustre code generator."

let run_trace model func t =
  let init_env = Datatype.SF.init_env model in
  let _ = Format.printf "Model definitions@.%a@.Initial state: %s @.####" Datatype.SF.pp_src (snd model) (fst model) in 
  
  let final_env, cpt =
    List.fold_left (fun (env, cpt) event ->
      Format.printf "#### %i@.%a@." cpt ActiveStates.Env.pp_env env;
      Format.printf "-- Event %a --@." Basetypes.pp_event event;
      let env', actions_performed = func (event, env) in
      let _ =
	match actions_performed with
      | [] -> Format.printf "-- no action performed --@."
      | _ -> (
	Format.printf "@[<v 2>-- action performed --@ ";
	List.iter (fun a -> Format.printf "%s@ " a) actions_performed;
	Format.printf "@]@."
      ) in
      (* we do not consider produced events *)
      env', cpt+1
    ) (init_env, 1) t
  in
  Format.printf "#### %i@.%a@." cpt ActiveStates.Env.pp_env final_env;
  ()
    
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
  | Eval ->
     let module Model = (val model) in
     let module T = CPS_transformer.Evaluator in
     let module Sem = CPS.Semantics (T) (Model) in
     let nb_traces = List.length Model.traces in
     if nb_traces = 0 then
       failwith ("no available traces for model " ^ Model.name);
     if !trace_id >= 0 && !trace_id < nb_traces then
       let eval_func = 
	 match !eval_mode with
	 | CPSEval -> 
	    fun (evt, env) ->
	      let _, env', actions_performed = Sem.compute modularmode (evt, env, []) in
	      env', actions_performed
	 | EMSOFT05Eval ->
	    Orig_Interpreter.eval_prog Model.model
	 | CompareEval ->
	    let evalCPS = Sem.compute modularmode in
	    let evalEMSOFT05 = (Orig_Interpreter.eval_prog Model.model) in
	    fun ((evt, env) as arg) ->
	      let _, env1, actionl1 = evalCPS (evt, env, []) in
	      let env2, actionl2 = evalEMSOFT05 arg in
	      if ActiveStates.Env.equal (=) env1 env2 then
	 	if actionl1 = actionl2 then
		  env1, actionl1
		else (
		  Format.printf "##### ERROR: different behavior for interpreters ######@.";
		  Format.printf "CPS Eval actions:@.%a@." (Utils.fprintf_list ~sep:"; " Format.pp_print_string) actionl1;
		  Format.printf "EMSOFT05 Eval actions:@.%a@." (Utils.fprintf_list ~sep:"; " Format.pp_print_string) actionl2;
	 	  Format.printf "@?";
		  Format.printf "##### END OF ERROR ######@.";
	 	  env1, actionl1
		)
	      else (
	 	Format.printf "##### ERROR: different behavior for interpreters ######@.";
	 	Format.printf "CPS Eval env:@.%a@." ActiveStates.Env.pp_env env1;
	 	Format.printf "EMSOFT05 Eval env:@.%a@." ActiveStates.Env.pp_env env2;
	 	Format.printf "@?";
	 	exit 1
	      )
       in
       run_trace Model.model eval_func (List.nth Model.traces !trace_id)
	 
     else
       failwith (string_of_int !trace_id ^
		   " is not a valid trace index in [0.." ^ string_of_int nb_traces ^ "]")
  | GenImp -> (
    let module Model = (val model) in
    let module T = CPS_transformer.CodeGenerator in
    let module Sem = CPS.Semantics (T) (Model) in
    Sem.code_gen Format.std_formatter modularmode
  )				     
  | GenLus ->
     let module Model = (val model) in
     let vars = Datatype.SF.states Model.model in
     let module T = CPS_transformer.LustrePrinter (struct let vars = vars end) in
     let module Sem = CPS.Semantics (T) (Model) in
     Sem.code_gen Format.std_formatter modularmode


  
