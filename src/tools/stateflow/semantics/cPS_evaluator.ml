  (* this file is no longer used. It contains the code that enable the use of
     the CPS to build an evaluator.
     
     Three pieces of code:
     - excerpt from the main that instanciate the functor to the evaluator and run the provided trace 
     - run_trace function
     - Evaluator module
 *)

  let _main_ _ = 
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
       in
       run_trace Model.model eval_func (List.nth Model.traces !trace_id)
	 
     else
       failwith (string_of_int !trace_id ^
		   " is not a valid trace index in [0.." ^ string_of_int nb_traces ^ "]")



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


    type truc = A of base_action_t | C of base_condition_t
module Evaluator : TransformerType with type t = (event_t * bool ActiveStates.Env.t * truc list) -> (event_t * bool ActiveStates.Env.t * truc list ) =
struct
  include TransformerStub
  type env_t = event_t * bool ActiveStates.Env.t * truc list (* Don't care for values yet *)
  type t = env_t -> env_t
 
  let null rho = rho
  let add_action a (evt, rho, al) = (evt, rho, al@[A a]) (* not very efficient but
							  avoid to keep track of
							  action order *)
  let add_condition c (evt, rho, al) = (evt, rho, al@[C c]) (* not very efficient but
							  avoid to keep track of
							  action order *)

  let bot _ = assert false
 
  let ( >> ) tr1 tr2 = fun rho -> rho |> tr1 |> tr2

  let ( ?? ) b tr = if b then tr else null

  let eval_open p (evt, rho, al)  = (evt, ActiveStates.Env.add p true rho, al)
  let eval_close p (evt, rho, al) = (evt, ActiveStates.Env.add p false rho, al)
  let eval_call : type c. (module ThetaType with type t = t) -> c call_t -> c -> t =
    fun kenv ->
    let module Theta = (val kenv : ThetaType with type t = t) in	      
    fun call -> match call with
    | Ecall -> (fun (p, p', f) -> Theta.theta E p p' f)
    | Dcall -> (fun p          -> Theta.theta D p)
    | Xcall -> (fun (p, f)     -> Theta.theta X p f)

  let eval_act kenv action =
    (* Format.printf "----- action = %a@." Action.pp_act action; *)
    match action with
    | Action.Call (c, a)      -> eval_call kenv c a
    | Action.Quote a          -> add_action a
    | Action.Open p           -> eval_open p
    | Action.Close p          -> eval_close p
    | Action.Nil              -> null

  (*if (match trans.event with None -> true | _ -> e = trans.event) && trans.condition rho*)
  let rec eval_cond condition ok ko : t =
    (* Format.printf "----- cond = %a@." Condition.pp_cond condition; *)
    match condition with
    | Condition.True               -> ok
    | Condition.Active p           -> (fun ((evt, env, al) as rho) -> if ActiveStates.Env.find p env then ok rho else ko rho)
    | Condition.Event e            -> (fun ((evt, env, al) as rho) -> match evt with None -> ko rho | Some e' -> if e=e' then ok rho else ko rho)
    | Condition.Neg cond           -> eval_cond cond ko ok
    | Condition.And (cond1, cond2) -> eval_cond cond1 (eval_cond cond2 ok ko) ko
    | Condition.Quote c            -> add_condition c >> ok (* invalid behavior but similar to the other: should evaluate condition *)

  let pp_transformer fmt tr =
    Format.fprintf fmt "<transformer>"

  let pp_principal fmt tr =
    Format.fprintf fmt "principal =@.%a" pp_transformer tr

  let pp_component : type c. Format.formatter -> c call_t -> c -> t -> unit =
    fun fmt call -> match call with
    | Ecall -> (fun (p, p', f) tr ->
      Format.fprintf fmt "component %a(%a, %a, %a) =@.%a" pp_call call pp_path p pp_path p' pp_frontier f pp_transformer tr)
    | Dcall -> (fun p tr ->
      Format.fprintf fmt "component %a(%a) =@.%a" pp_call call pp_path p pp_transformer tr)
    | Xcall -> (fun (p, f) tr ->
      Format.fprintf fmt "component %a(%a, %a) =@.%a" pp_call call pp_path p pp_frontier f pp_transformer tr)
end

