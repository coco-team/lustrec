open Basetypes
open ActiveStates

type mode_t =
  | Outer
  | Inner
  | Enter


type 't success_t = path_t -> 't
type 't fail_t = { local: 't; global: 't }
type 't wrapper_t = path_t -> 't -> 't

type ('a, 'b, 't) tag_t =
  | E : (path_t, path_t -> frontier_t -> 't, 't) tag_t
  | D : (path_t, 't, 't) tag_t
  | X : (path_t, frontier_t -> 't, 't) tag_t
  | J : (junction_name_t, 't wrapper_t -> 't success_t -> 't fail_t -> 't, 't) tag_t


type ('a, 'b, 't) theta_t = ('a, 'b, 't) tag_t -> 'a -> 'b

module type ThetaType =
sig
  type t
  val theta : ('a, 'b, t) theta_t
end

let pp_mode fmt mode =
  match mode with
  | Outer -> Format.fprintf fmt "Outer"
  | Inner -> Format.fprintf fmt "Inner"
  | Enter -> Format.fprintf fmt "Enter"


let pp_tag : type a b t. Format.formatter -> (a, b, t) tag_t -> unit =
  fun fmt tag ->
    match tag with
    | E -> Format.fprintf fmt "e"
    | D -> Format.fprintf fmt "d"
    | X -> Format.fprintf fmt "x"
    | J -> Format.fprintf fmt "j"

(*
module Proj1Theta (T : sig type t1 val bot1 : t1 type t2 val bot2 : t2 end) (Theta : ThetaType with type t = T.t1 * T.t2) : ThetaType with type t = T.t1 =
struct
  type t = T.t1

  let f f1 = (f1, T.bot2)
  let s s1 p = (s1 p, T.bot2)
  let w w1 p (tr1, _) = (w1 p tr1, T.bot2)

  let theta : type a b. (a, b, t) theta_t =
    fun tag ->
    match tag with
    | E -> (fun p p' f     -> fst (Theta.theta E p p' f))
    | D -> (fun p          -> fst (Theta.theta D p))
    | X -> (fun p f        -> fst (Theta.theta X p f))
    | J -> (fun j w1 s1 f1 -> fst (Theta.theta J j (w w1) (s s1) (f f1)))
end

module Proj2Theta (T : sig type t1 val bot1 : t1 type t2 val bot2 : t2 end) (Theta : ThetaType with type t = T.t1 * T.t2) : ThetaType with type t = T.t2 =
struct
  type t = T.t2

  let f f2 = (T.bot1, f2)
  let s s2 p = (T.bot1, s2 p)
  let w w2 p (_, tr2) = (T.bot1, w2 p tr2)

  let theta : type a b. (a, b, t) theta_t =
    fun tag ->
    match tag with
    | E -> (fun p p' f     -> snd (Theta.theta E p p' f))
    | D -> (fun p          -> snd (Theta.theta D p))
    | X -> (fun p f        -> snd (Theta.theta X p f))
    | J -> (fun j w2 s2 f2 -> snd (Theta.theta J j (w w2) (s s2) (f f2)))
end
*)

module TransformerStub =
struct
  type act_t = Action.t
  type cond_t = Condition.t

  let nil = Action.nil
  let aquote = Action.aquote
  let open_path = Action.open_path
  let close_path = Action.close_path
  let call = Action.call
  let pp_act = Action.pp_act

  let cquote = Condition.cquote
  let tru = Condition.tru
  let event = Condition.event
  let active = Condition.active
  let ( && ) = Condition.( && )
  let neg = Condition.neg
  let pp_cond = Condition.pp_cond
end

module type TransformerType =
sig
  type act_t = Action.t
  type cond_t = Condition.t
  type t

  include ActionType with type t := act_t
  include ConditionType with type t := cond_t

  val null : t
  val bot : t
  val ( >> ) : t -> t -> t
  val eval_act : (module ThetaType with type t = t) -> act_t -> t
  val eval_cond : cond_t -> t -> t -> t
  val pp_transformer : Format.formatter -> t -> unit
  val pp_principal : Format.formatter -> t -> unit
  val pp_component : Format.formatter -> 'c call_t -> 'c -> t -> unit
end

module type ComparableTransformerType =
sig
  include TransformerType

  val ( == ) : t -> t -> bool
end
(*
module Pair (T1 : ComparableTransformerType) (T2 : TransformerType) : ComparableTransformerType with type t = T1.t * T2.t =
struct
  include TransformerStub

  type t = T1.t * T2.t

  module T =
  struct
    type t1 = T1.t
    type t2 = T2.t
    let bot1 = T1.bot
    let bot2 = T2.bot
  end

  let null = T1.null, T2.null

  let bot = T1.bot, T2.bot

  let ( >> ) (tr11, tr12) (tr21, tr22) =
    T1.(tr11 >> tr21), T2.(tr12 >> tr22)

  let eval_act theta action =
    let module Theta = (val theta : ThetaType with type t = T1.t * T2.t) in
    let theta1 = (module Proj1Theta (T) (Theta) : ThetaType with type t = T1.t) in
    let theta2 = (module Proj2Theta (T) (Theta) : ThetaType with type t = T2.t) in
    T1.eval_act theta1 action, T2.eval_act theta2 action

  let eval_cond cond (trt1, trt2) (tre1, tre2) =
    T1.eval_cond cond trt1 tre1, T2.eval_cond cond trt2 tre2

  let ( == ) (tr1, _) (tr2, _) = T1.(tr1 == tr2)

  let pp_transformer fmt (tr1, tr2) =
    Format.fprintf fmt "< %a , %a >" T1.pp_transformer tr1 T2.pp_transformer tr2

  let pp_principal fmt (tr1, tr2) =
    Format.fprintf fmt "< %a , %a >" T1.pp_principal tr1 T2.pp_principal tr2

  let pp_component : type c. Format.formatter -> c call_t -> c -> t -> unit =
    fun fmt call ->
    match call with
    | Ecall -> (fun c (tr1, tr2) ->
      Format.fprintf fmt "< %t , %t >"
	(fun fmt -> T1.pp_component fmt Ecall c tr1)
	(fun fmt -> T2.pp_component fmt Ecall c tr2))
    | Dcall -> (fun c (tr1, tr2) ->
      Format.fprintf fmt "< %t , %t >"
	(fun fmt -> T1.pp_component fmt Dcall c tr1)
	(fun fmt -> T2.pp_component fmt Dcall c tr2))
    | Xcall -> (fun c (tr1, tr2) ->
      Format.fprintf fmt "< %t , %t >"
	(fun fmt -> T1.pp_component fmt Xcall c tr1)
	(fun fmt -> T2.pp_component fmt Xcall c tr2))
end
*)
  
module Evaluator : TransformerType with type t = (event_t * bool ActiveStates.Env.t * base_action_t list -> event_t * bool ActiveStates.Env.t * base_action_t list ) =
struct
  include TransformerStub

  type env_t = event_t * bool ActiveStates.Env.t * base_action_t list (* Don't care for values yet *)
  type t = env_t -> env_t
 
  let null rho = rho
  let add_action a (evt, rho, al) = (evt, rho, al@[a]) (* not very efficient but
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
  let rec eval_cond condition ok ko =
    (* Format.printf "----- cond = %a@." Condition.pp_cond condition; *)
    match condition with
    | Condition.True               -> ok
    | Condition.Active p           -> (fun ((evt, env, al) as rho) -> if ActiveStates.Env.find p env then ok rho else ko rho)
    | Condition.Event e            -> (fun ((evt, env, al) as rho) -> match evt with None -> ko rho | Some e' -> if e=e' then ok rho else ko rho)
    | Condition.Neg cond           -> eval_cond cond ko ok
    | Condition.And (cond1, cond2) -> eval_cond cond1 (eval_cond cond2 ok ko) ko
    | Condition.Quote c            -> add_action c >> ok (* invalid behavior but similar to the other: should evaluate condition *)

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

module CodeGenerator : ComparableTransformerType =
struct
  include TransformerStub

  type t =
  | Bot
  | Act of act_t
  | Seq of t list
  | Ite of cond_t * t * t

  let null = Seq []

  let bot = Bot
 
  let ( >> ) tr1 tr2 =
    match tr1, tr2 with
    | Seq trl1, Seq trl2 -> Seq (trl1@trl2)
    | Seq trl1, _        -> Seq (trl1@[tr2])
    | _       , Seq trl2 -> Seq (tr1::trl2)
    | _                  -> Seq ([tr1;tr2])

  let rec ( == ) tr1 tr2 = tr1 = tr2

  let eval_act kenv (action : act_t) =
    (*Format.printf "----- action = %a@." Action.pp_act action;*)
    Act action

  (*if (match trans.event with None -> true | _ -> e = trans.event) && trans.condition rho*)
  let rec eval_cond condition ok ko =
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    Ite (condition, ok, ko)
    
  let rec pp_transformer fmt tr =
    match tr with
    | Bot           -> Format.fprintf fmt "bot"
    | Act a         ->
       Format.fprintf fmt "<%a>" pp_act a
    | Seq trl       ->
       Format.fprintf fmt "@[<v 0>%a@]"
	 (Utils.fprintf_list ~sep:";@ " pp_transformer)
	 trl
    | Ite (c, t, e) ->
       Format.fprintf fmt "@[<v 0>if %a@ @[<v 2>then@ %a@]@ @[<v 2>else@ %a@]@ endif@]" pp_cond c pp_transformer t pp_transformer e

  let pp_principal fmt tr =
    Format.fprintf fmt "principal =@.%a" pp_transformer tr
      
  let pp_component : type c. Format.formatter -> c call_t -> c -> t -> unit =
    fun fmt call -> match call with
    | Ecall -> (fun (p, p', f) tr ->
      Format.fprintf fmt "component %a(%a, %a, %a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_path p' pp_frontier f pp_transformer tr)
    | Dcall -> (fun p tr ->
      Format.fprintf fmt "component %a(%a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_transformer tr)
    | Xcall -> (fun (p, f) tr ->
      Format.fprintf fmt "component %a(%a, %a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_frontier f pp_transformer tr)
end

module LustrePrinter (Vars : sig val vars : ActiveStates.Vars.t end) : TransformerType =
struct
  include TransformerStub

  type name_t = string
  type t = name_t -> name_t -> (ActiveStates.Vars.t * (Format.formatter -> unit))

  let new_loc, reset_loc =
    let cpt = ref 0 in
    ((fun () -> incr cpt; Format.sprintf "loc_%i" !cpt),
     (fun () -> cpt := 0))

  let new_aut, reset_aut =
    let cpt = ref 0 in
    ((fun () -> incr cpt; Format.sprintf "aut_%i" !cpt),
     (fun () -> cpt := 0))
      
  let pp_path sin fmt path =
    Format.fprintf fmt "%s%t" sin (fun fmt -> Utils.fprintf_list ~sep:"_" Format.pp_print_string fmt path)

  let pp_typed_path sin fmt path =
    Format.fprintf fmt "%a : bool" (pp_path sin) path

  let pp_vars sin fmt vars =
    Format.fprintf fmt "%t" (fun fmt -> Utils.fprintf_list ~sep:", " (pp_path sin) fmt (ActiveStates.Vars.elements vars))

  let pp_vars_decl sin fmt vars =
    Format.fprintf fmt "%t" (fun fmt -> Utils.fprintf_list ~sep:"; " (pp_typed_path sin) fmt (ActiveStates.Vars.elements vars))

  let pp_locals fmt locs =
    ActiveStates.Vars.iter (fun v -> Format.fprintf fmt "%a;@ " (pp_vars_decl (List.hd v)) Vars.vars) locs

  let null sin sout =
    (ActiveStates.Vars.empty,
     fun fmt -> Format.fprintf fmt "(%a) = (%a);" (pp_vars sout) Vars.vars (pp_vars sin) Vars.vars)

  let bot sin sout =
    let (vars, tr) = null sin sout in
    (ActiveStates.Vars.empty,
     (fun fmt -> Format.fprintf fmt "assert false;@ %t" tr))
 
  let ( >> ) tr1 tr2 sin sout =
    let l = new_loc () in
    let (vars1, tr1) = tr1 sin l in
    let (vars2, tr2) = tr2 l sout in
    (ActiveStates.Vars.add [l] (ActiveStates.Vars.union vars1 vars2),
     fun fmt -> Format.fprintf fmt "%t@ %t" tr1 tr2)

  let pp_call' : type c. name_t -> name_t -> Format.formatter -> c call_t -> c -> unit =
    fun sin sout fmt call ->
    match call with
    | Ecall -> (fun (p, p', f) ->
      Format.fprintf fmt "(%a) = theta%a%a%a_%a(event, %a);"
	(pp_vars sout) Vars.vars
	pp_call call
	(pp_path "_from_") p
	(pp_path "_to_") p'
	pp_frontier f
	(pp_vars sin) Vars.vars)
    | Dcall -> (fun p          ->
      Format.fprintf fmt "(%a) = theta%a%a(event, %a);"
	(pp_vars sout) Vars.vars
	pp_call call
	(pp_path "_from_") p
	(pp_vars sin) Vars.vars)
    | Xcall -> (fun (p, f)     ->
      Format.fprintf fmt "(%a) = theta%a%a_%a(event, %a);"
	(pp_vars sout) Vars.vars
	pp_call call
	(pp_path "_from_") p
	pp_frontier f
	(pp_vars sin) Vars.vars)

  let pp_act' action sin sout fmt =
    match action with
    | Action.Call (c, a) -> pp_call' sin sout fmt c a
    | Action.Quote a     -> Format.fprintf fmt "(%a) = (* %s *)(%a);" (pp_vars sout) Vars.vars a (pp_vars sin) Vars.vars
    | Action.Open p      -> let vars' = ActiveStates.Vars.remove p Vars.vars in
			    Format.fprintf fmt "%a = true;@ (%a) = (%a);" (pp_path sout) p (pp_vars sout) vars' (pp_vars sin) vars'
    | Action.Close p     -> let vars' = ActiveStates.Vars.remove p Vars.vars in
			    Format.fprintf fmt "%a = false;@ (%a) = (%a);" (pp_path sout) p (pp_vars sout) vars' (pp_vars sin) vars'
    | Action.Nil         -> Format.fprintf fmt "(%a) = (%a);" (pp_vars sout) Vars.vars (pp_vars sin) Vars.vars

  let eval_act kenv (action : act_t) =
    (*Format.printf "----- action = %a@." Action.pp_act action;*)
    (fun sin sout -> (ActiveStates.Vars.empty, pp_act' action sin sout))
       
  let rec pp_cond' sin fmt condition =
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    match condition with
    | Condition.True               -> Format.fprintf fmt "true"
    | Condition.Active p           -> Format.fprintf fmt "%a" (pp_path sin) p
    | Condition.Event e            -> Format.fprintf fmt "(event = %s)" e
    | Condition.Neg cond           -> Format.fprintf fmt "not (%a)" (pp_cond' sin) cond
    | Condition.And (cond1, cond2) -> Format.fprintf fmt "%a && %a" (pp_cond' sin) cond1 (pp_cond' sin) cond2
    | Condition.Quote c            -> Format.fprintf fmt "(* %s *) true" c

  let rec eval_cond condition ok ko sin sout =
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    let (vars1, tr1) = ok sin sout in
    let (vars2, tr2) = ko sin sout in
    let (vars0, tr0) = bot sin sout in
    let aut = new_aut () in
    (ActiveStates.Vars.empty,
     (fun fmt -> 
    Format.fprintf fmt "@[<v 1>automaton %s@ @[<v 2>state CenterPoint_%s:@ unless %a restart Cond_%s@ unless %a restart NotCond_%s@ let@ %t@ tel@]@ @[<v 2>state Cond_%s:@ %t%a@ let@ %t@ tel@ until true restart CenterPoint_%s@]@ @[<v 2>state NotCond_%s:@ %t%a@ let@ %t@ tel@ until true restart CenterPoint_%s@]@]"
      aut
      aut
      (pp_cond' sin) condition
      aut
      (pp_cond' sin) (Condition.Neg condition)
      aut
      tr0
      aut
      (fun fmt -> if ActiveStates.Vars.is_empty vars1 then () else Format.fprintf fmt "var@ ")
      pp_locals vars1
      tr1
      aut
      aut
      (fun fmt -> if ActiveStates.Vars.is_empty vars2 then () else Format.fprintf fmt "var@ ")
      pp_locals vars2
      tr2
      aut))

  let pp_transformer fmt tr =
    let (vars, tr) = tr "sin_" "sout_"
    in tr fmt

  let pp_component : type c. Format.formatter -> c call_t -> c -> t -> unit =
    fun fmt call -> match call with
    | Ecall -> (fun (p, p', f) tr ->
      reset_loc ();
      let (vars', tr') = tr "sin_" "sout_" in
      Format.fprintf fmt "node theta%a%a%a_%a(event : event_type; %a) returns (%a);@.%t%a@.let@.%t@.tel@."
	pp_call call
	(pp_path "_from_") p
	(pp_path "_to_") p'
	pp_frontier f
	(pp_vars_decl "sin_") Vars.vars
	(pp_vars_decl "sout_") Vars.vars
	(fun fmt -> if ActiveStates.Vars.is_empty vars' then () else Format.fprintf fmt "var@.")
	pp_locals vars'
	tr')
    | Dcall -> (fun p tr ->
      reset_loc ();
      let (vars', tr') = tr "sin_" "sout_" in
      Format.fprintf fmt "node theta%a%a(event : event_type; %a) returns (%a);@.%t%a@.let@.%t@.tel@."
	pp_call call
	(pp_path "_from_") p
	(pp_vars_decl "sin_") Vars.vars
	(pp_vars_decl "sout_") Vars.vars
	(fun fmt -> if ActiveStates.Vars.is_empty vars' then () else Format.fprintf fmt "var@.")
	pp_locals vars'
	tr')
    | Xcall -> (fun (p, f) tr ->
      reset_loc ();
      let (vars', tr') = tr "sin_" "sout_" in
      Format.fprintf fmt "node theta%a%a_%a(event : event_type; %a) returns (%a);@.%t%a@.let@.%t@.tel@."
	pp_call call
	(pp_path "_from_") p
	pp_frontier f
	(pp_vars_decl "sin_") Vars.vars
	(pp_vars_decl "sout_") Vars.vars
	(fun fmt -> if ActiveStates.Vars.is_empty vars' then () else Format.fprintf fmt "var@.")
	pp_locals vars'
	tr')

  let pp_main_loop fmt () =
    Format.fprintf fmt "type event_type = int;@.node principal_loop(event : event_type) returns (%a);@.let@.(%a) = (%t) -> pre (thetaCallD_from_principal (event, %a));@.tel@."
      (pp_vars_decl "sout_") Vars.vars
      (pp_vars "sout_") Vars.vars
      (fun fmt -> Utils.fprintf_list ~sep:", " (fun fmt _ -> Format.fprintf fmt "false") fmt (ActiveStates.Vars.elements Vars.vars))
      (pp_vars "sout_") Vars.vars

  let pp_principal fmt tr =
    Format.fprintf fmt "%a@.%a@."
      pp_main_loop ()
      (fun fmt -> pp_component fmt Dcall ["principal"]) tr

end
