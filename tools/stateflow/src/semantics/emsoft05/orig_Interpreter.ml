open Basetypes
open Datatype
open Format
open Log
open SF

let actions_performed = ref []

    
(*******************************)

type env_t = bool ActiveStates.Env.t (* Don't care for values yet *)
let pp_env = ActiveStates.Env.pp_env
  
type success_t = env_t -> path_t -> env_t
type fail_t = env_t -> env_t

type kenv_t = {
  cont_node : (
    path_t * (
      (kenv_t -> env_t -> path_t -> event_t -> env_t) *
	(kenv_t -> env_t -> event_t -> env_t) *
	(kenv_t -> env_t -> event_t -> env_t)
    )
  ) list;
  cont_junc : (
    junction_name_t * (
      kenv_t -> env_t -> success_t -> fail_t -> event_t -> env_t)
  ) list
}

let theta_j dest theta =
  (try
     List.assoc dest theta.cont_junc
   with Not_found ->
     (eprintf "Lost junction %a@ " pp_junction_name dest;
      assert false
     )
  ) theta

let theta_e, theta_d, theta_x =
  let theta_node id theta =
    try
      List.assoc id theta.cont_node
    with Not_found ->
      (eprintf "Lost path [%a]@." pp_path id;
       assert false
      )
  in
  (fun id theta -> let (e,d,x) = theta_node id theta in e theta),
  (fun id theta -> let (e,d,x) = theta_node id theta in d theta),
  (fun id theta -> let (e,d,x) = theta_node id theta in x theta)

let add_action a = actions_performed := !actions_performed @ [a]

let eval_act action theta rho =
  (match action with
  | Action.Call _  -> assert false
  | Action.Quote s -> add_action s; () (*Format.eprintf "----- action = %s@." s*)
  | Action.Nil  -> () (* no action *)
  | _ -> assert false (* other constructs should not appear here *)
  );
  rho

let rec eval_cond condition (evt, rho) =
  match condition, evt with
  | Condition.True, _ -> true
  | Condition.Active p, _ -> ActiveStates.Env.find p rho
  | Condition.Event e, Some evt -> e = evt
  | Condition.Event e, None -> false
  | Condition.Neg cond, _ -> not (eval_cond cond (evt, rho))
  | Condition.And (cond1, cond2), _ -> (eval_cond cond1 (evt, rho)) && (eval_cond cond2 (evt, rho))
  | Condition.Quote c, _ -> add_action c; true  (* invalid behavior but similar to the other: should evaluate condition *)
    
  (* (match condition with Some s -> printf "----- cond = %s@." s | _ -> ()); *)
  (* true *)

let bot _ = assert false

let eval_dest dest theta rho success fail e =
  log (fun fmt -> fprintf fmt "@[<v 2>D[[%a]] (%a)?@ " SF.pp_dest dest pp_env rho);
  let res =
    match dest with
    | DPath p -> success rho p
    | DJunction j -> theta_j j theta rho success fail e
  in
  log (fun fmt -> fprintf fmt "@]@ D[[%a]]: %a@ " SF.pp_dest dest pp_env res);
  res

let eval_tau trans theta rho success fail e =
  (* eprintf "%t@." (fun fmt -> fprintf fmt "@[<v 2>tau[[%a]] (%a)?@ " SF.pp_trans trans pp_env rho); *)
  let res =
    if (match trans.event with None -> true | _ -> e = trans.event) && eval_cond trans.condition (trans.event, rho) then
      let success' rho_s p =
	(* eprintf "succes de tau %a: p = %a@." pp_trans trans pp_path p; *)
	if p = [] then (
	  (* eprintf "cas1@."; *)
	  success rho_s [] (* was p but p is [] *)
	)
	else (
	  	  (* eprintf "cas2@."; *)
		    success (eval_act trans.transition_act theta rho_s) p
	)
      in
      eval_dest trans.dest theta (eval_act trans.condition_act theta rho) success' fail e
    else
      fail rho

  in
  (* eprintf "%t@."  (fun fmt -> fprintf fmt "@]@ tau[[%a]]: %a@ " pp_trans trans pp_env res); *)
  res

let rec eval_T tl theta rho success fail e =
  log (fun fmt -> fprintf fmt "@[<v 2>T[[%a]] (%a)?@ " pp_transitions tl pp_env rho);
  let res =
    match tl with
    | [] -> success rho []
    | t::[] -> eval_tau t theta rho success fail e
    | t::t'::tl ->
       let fail' rho_f = eval_T (t'::tl) theta rho_f success fail e in
       eval_tau t theta rho success fail' e
  in
  log (fun fmt -> fprintf fmt "@]@ T[[%a]]: %a@ " pp_transitions tl pp_env res);
  res

let eval_open rho p = ActiveStates.Env.add p true rho
let eval_close rho p = ActiveStates.Env.add p false rho

let eval_Ce prefix comp theta rho e =
  log (fun fmt -> fprintf fmt "@[<v 2>Ce[[%a, %a]] (%a)?@ " pp_path prefix pp_comp comp pp_env rho);
  let res =
    match comp with
    | Or (_T, []) -> log (fun fmt -> fprintf fmt "no content: = no impact@ "); rho
    | Or (_T, _S) -> eval_T _T theta rho (fun rho_s p -> theta_e p theta rho_s [] e) bot e
    | And (_S) -> List.fold_left (fun accu p -> theta_e (prefix@[p]) theta rho [] e) rho _S
  in
  log (fun fmt -> fprintf fmt "@]@ Ce[[%a, %a]]: %a@ " pp_path prefix pp_comp comp pp_env res);
  res

let rec eval_Cd prefix comp theta rho e =
  log (fun fmt -> fprintf fmt "@[<v 0>Cd[[%a, %a]] (%a)?@ " pp_path prefix pp_comp comp pp_env rho);
  let res =
    match comp with
    | Or (_T, []) -> rho
    | Or (_T, p::_S) ->
       if ActiveStates.Env.find (prefix@[p]) rho then (
	 log (fun fmt -> fprintf fmt "active node %a: running it@ " pp_path (prefix@[p]));

	 (* (\********** QUESTION SUR L'ORDRE A HAMZA **************\) *)
	 (* let rho', success = *)
	 (*   (\* if true (\\* cas 1: during_act effectué avant execution des components *\\) then *\) *)
	 (*     theta_d (prefix@[p]) theta rho e, (\* <-- update rho with theta_d of node (prefix@[p]) *\) *)
	 (*     (fun rho_s p' -> theta_d p' theta rho_s e) (\* <-- default success function *\) *)
	 (*     (\* je ne suis pas sur de ce p', peut etre faut il rajouter prefix@[p] devant ? *\) *)
	 (*   (\* else (\\* cas 2: during_act postponé apres execution des components *\\) *\) *)
	 (*   (\*   rho, (\\* <-- on ne touche pas a rho *\\) *\) *)
	 (*   (\*   (fun rho_s p' -> theta_d p' theta rho_s e) *\) *)
	 (*   (\*   (\\* je ne suis pas sur de ce p', peut etre faut il rajouter prefix@[p] devant ? *\\) *\) *)
	 (* in *)
	 (* eval_T _T theta rho' success bot e *)
	 (* (\********** QUESTION SUR L'ORDRE A HAMZA **************\) *)

       (* CODE PRECEDENT TEL QUE DECRIT DANS L'ARTICLE *)
        	 theta_d (prefix@[p]) theta rho e
       (* CODE PRECEDENT TEL QUE DECRIT DANS L'ARTICLE *)
       )
       else (
	 log (fun fmt -> fprintf fmt "inactive node %a: continuing on the list %a@ " pp_path (prefix@[p]) (Utils.fprintf_list ~sep:"; " pp_state_name) _S);
	 eval_Cd prefix (Or (_T, _S)) theta rho e
       )
    | And (_S) -> List.fold_left (fun accu p -> theta_d (prefix@[p]) theta rho e) rho _S
  in
  log (fun fmt -> fprintf fmt "@]@ Cd[[%a, %a]]: %a@ " pp_path prefix pp_comp comp pp_env res);
  res

let eval_Cx prefix comp theta rho e : env_t =
  log (fun fmt -> fprintf fmt "@[<v 2>Cx[[%a, %a]] (%a)?@ " pp_path prefix pp_comp comp pp_env rho);
  let res =
    match comp with
    | Or (_T, _S) ->
       List.fold_left
	 (fun rho p -> if ActiveStates.Env.find (prefix @ [p]) rho then theta_x (prefix@[p]) theta rho e else rho)
	 rho
	 _S
    | And (_S) -> List.fold_left (fun accu p -> theta_x (prefix@[p]) theta rho e) rho _S
  in
  log (fun fmt -> fprintf fmt "@]@ Cx[[%a, %a]]: %a@ " pp_path prefix pp_comp comp pp_env res);
  res

let eval_Se (p:path_t) p_def theta rho path e =
  log (fun fmt -> fprintf fmt "@[<v 2>Se[[node %a, path %a]] (%a)?@ " pp_path p pp_path path pp_env rho);
  let res =
    let rho' = eval_act p_def.state_actions.entry_act theta rho in
    match path with
    | [] -> eval_open (eval_Ce p p_def.internal_composition theta rho' e) p
    | s::path_tl -> eval_open (theta_e (p@[s]) theta rho' path_tl e) p
  in
  log (fun fmt -> fprintf fmt "@]@ Se[[node %a, path %a]]: %a@ " pp_path p pp_path path pp_env res);
  res

let eval_Sx p p_def theta rho e =
  log (fun fmt -> fprintf fmt "@[<v 2>Sx[[node %a]] (%a)?@ " pp_path p pp_env rho);
  let res =
  eval_close
    (eval_act p_def.state_actions.exit_act theta
       (eval_Cx p p_def.internal_composition theta rho e)
    )
    p
  in
  log (fun fmt -> fprintf fmt "@]@ Sx[[node %a]]: %a@ " pp_path p pp_env res);
  res

let rec eval_open_path theta rho p p1 p2 term e =
  match p1, p2 with
  | _, [] -> term rho
  | x::ps, y::pd ->
     if x = y then
       eval_open_path theta rho (p@[x]) ps pd term e
     else
       let rho_x = theta_x (p@[x]) theta rho e in
       theta_e (p@[y]) theta rho_x pd e
  | _ -> assert false

let eval_Sd p p_def theta rho e =
  log (fun fmt -> fprintf fmt "@[<v 2>Sd[[node %a]] (%a)?@ " pp_path p pp_env rho);
  let res =
    let fail rho_f =
      let faili (rho_fi: env_t) : env_t =
	eval_Cd p p_def.internal_composition theta (eval_act p_def.state_actions.during_act theta rho_fi) e
      in
      let successi rho_si p_d =
	eval_open_path theta rho_si [] p p_d faili e in
      log (fun fmt -> fprintf fmt "eval_inner %a@ " pp_transitions p_def.inner_trans);
      eval_T p_def.inner_trans theta rho_f successi faili e
    in
    let success rho_s p_d =
      eval_open_path theta rho_s [] p p_d fail e in
    log (fun fmt -> fprintf fmt "eval_outer %a@ " pp_transitions p_def.outer_trans);
    eval_T p_def.outer_trans theta rho success fail e
  in
  log (fun fmt -> fprintf fmt "@]@ Sd[[node %a]]: %a@ " pp_path p pp_env res);
  res


let eval_prog (s, defs) (e, rho) =
  (* Action list is uglily encoded using global variables. Shame on me! *)
  actions_performed := [];
  (* (match e with Some s -> printf "EVENT = %s@." s | _ -> ()); *)
  let theta = List.fold_left (
    fun accu d -> match d with
    | State (p, defp) -> { accu with cont_node = (p, (eval_Se p defp, eval_Sd p defp, eval_Sx p defp))::accu.cont_node  }
    | Junction (j, defj) -> { accu with cont_junc = (j, (eval_T defj))::accu.cont_junc  }
  ) {cont_node = []; cont_junc = []} defs in
  let res =
    if ActiveStates.Env.find [s] rho then
      theta_d [s] theta rho e
    else
      theta_e [s] theta rho [] e
  in
  res, !actions_performed

