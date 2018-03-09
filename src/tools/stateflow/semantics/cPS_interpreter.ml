open Basetypes
open Datatype
(* open ActiveEnv *)
open CPS_transformer
(* open Simulink *)
open Theta

module Interpreter (Transformer : TransformerType) =
struct
  module KT = KenvTheta (Transformer)
  open KT

  let ( >? ) cond tr =
    if cond then tr else Transformer.null

  module type DenotationType =
  sig
    module Theta : MemoThetaType

    val eval_dest : destination_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_tau : trans_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_T : transitions_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_C : (path_t, 'b, Transformer.t) tag_t -> path_t -> composition_t -> Transformer.t
    val eval_open_path : mode_t -> path_t -> path_t -> Transformer.t wrapper_t
    val eval_S : (path_t, 'b, Transformer.t) tag_t -> path_t -> state_def_t -> 'b
  end

  module type AbsDenotationType =
  sig
    val eval_dest : kenv_t -> destination_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_tau : kenv_t -> trans_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_T : kenv_t -> transitions_t -> Transformer.t wrapper_t -> Transformer.t success_t -> Transformer.t fail_t -> Transformer.t
    val eval_C : kenv_t -> (path_t, 'b, Transformer.t) tag_t -> path_t -> composition_t -> Transformer.t
    val eval_open_path : kenv_t -> mode_t -> path_t -> path_t -> Transformer.t wrapper_t
    val eval_S : kenv_t -> (path_t, 'b, Transformer.t) tag_t -> path_t -> state_def_t -> 'b
  end

  module AbstractKenv (Denot : functor (Kenv : KenvType) -> DenotationType) : AbsDenotationType =
  struct
    let eval_dest kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_dest

    let eval_tau kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_tau

    let eval_T kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_T

    let eval_C kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_C

    let eval_open_path kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_open_path

    let eval_S kenv =
      let module Kenv =
	  struct
	    let kenv = kenv
	  end in
      let module D = Denot (Kenv) in
      D.eval_S
  end

  module Denotation : functor (Thetaify : ThetaifyType) (Kenv : KenvType) -> DenotationType =
  functor (Thetaify : ThetaifyType) (Kenv : KenvType) ->
  struct
    module Theta = Thetaify (Kenv)

    let eval_dest dest wrapper success fail =
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>D[[%a]]@ " SF.pp_dest dest);
      match dest with
      | DPath p -> wrapper p (success p)
      | DJunction j -> Theta.theta J j wrapper success fail

    let eval_tau trans wrapper success fail =
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>tau[[%a]]@ " SF.pp_trans trans);
      let success' p =
	Transformer.(success p >> eval_act (module Theta) trans.transition_act)
      in
      let cond = Transformer.(event trans.event && trans.condition) in
      Transformer.(eval_cond cond
		     (eval_act (module Theta) trans.condition_act >> eval_dest trans.dest wrapper success' fail)
		     fail.local)

    let rec eval_T tl wrapper success fail =
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>T[[%a]]@ " SF.pp_transitions tl);
      match tl with
      | [] -> fail.global
      | t::[] ->  eval_tau t wrapper success fail
      | t::tl ->
	 let fail' = { fail with local = eval_T tl wrapper success fail } in
	 eval_tau t wrapper success fail'

    let frontier path =
      match path with
      | []   -> [], []
      | t::q -> [t], q

    let rec eval_open_path mode p p1 p2 success_p2 =
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>open_path_rec[[mode %a, prefix %a, src %a, dst %a]]@ " pp_mode mode pp_path p pp_path p1 pp_path p2);
      match frontier p1, frontier p2 with
      | ([x], ps), ([y], pd) when x = y -> eval_open_path mode (p@[x]) ps pd success_p2
      | (x  , _ ), (y  , pd)            ->
	 match mode with
	 | Outer -> (Transformer.(Theta.theta X (p@x) Loose  >> success_p2 >> Theta.theta E (p@y) pd Loose))
	 | Inner -> (assert (x = []);
		     Transformer.(Theta.theta X (p@x) Strict >> success_p2 >> Theta.theta E (p@y) pd Strict))
	 | Enter -> (assert (x = [] && y <> []);
		     Transformer.(                              success_p2 >> Theta.theta E (p@y) pd Loose))

    let rec eval_C : type a b. (a, b, Transformer.t) tag_t -> path_t -> composition_t -> Transformer.t =
      fun tag prefix comp ->
	match tag with
	| E -> Transformer.(
	  Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>C_%a[[%a, %a]]@ " pp_tag tag pp_path prefix SF.pp_comp comp);
	  match comp with
	  | Or (_T, [])   -> null
	  | Or ([], [s0]) -> eval_open_path Enter prefix [] [s0] null
	  | Or (_T, _S)   -> let wrapper = eval_open_path Enter [] prefix in
			     let success p_d = null in
			     eval_T _T wrapper success { local = bot; global = bot }
	  | And (_S)    -> List.fold_right (fun p -> (>>) (Theta.theta E (prefix@[p]) [] Loose)) _S null
	)
	| D -> Transformer.(
	  match comp with
	  | Or (_T, [])    -> null
	  | Or (_T, p::_S) -> eval_cond (active (prefix@[p])) (Theta.theta D (prefix@[p])) (eval_C D prefix (Or (_T, _S)))
	  | And (_S)       -> List.fold_right (fun p -> (>>) (Theta.theta D (prefix@[p]))) _S null
	)
	| X -> Transformer.(
	  match comp with
	  | Or (_T, [])    -> null
	  | Or (_T, p::_S) -> eval_cond (active (prefix@[p])) (Theta.theta X (prefix@[p]) Loose) (eval_C X prefix (Or (_T, _S)))
	  | And (_S)       -> List.fold_right (fun p -> (>>) (Theta.theta X (prefix@[p]) Loose)) _S null
	)
	| J -> assert false

    let eval_S : type b. (path_t, b, Transformer.t) tag_t -> path_t -> state_def_t -> b =
      fun tag p p_def ->
	match tag with
	| E -> fun path frontier -> Transformer.(
	  Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>S_%a[[node %a, dest %a, frontier %a]]@ " pp_tag tag pp_path p pp_path path pp_frontier frontier);
	  ((frontier = Loose) >? (eval_act (module Theta) p_def.state_actions.entry_act >> eval_act (module Theta) (open_path p))) >>
	  match path with
	  | []         -> eval_C E p p_def.internal_composition
	  | s::path_tl -> Theta.theta E (p@[s]) path_tl Loose
	)
	| D -> Transformer.(
	  Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>S_%a[[node %a]]@ " pp_tag tag pp_path p);
	  let wrapper_i = eval_open_path Inner [] p in
	  let wrapper_o = eval_open_path Outer [] p in
	  let success p_d = null in
	  let fail_o =
	    let fail_i =
	      let same_fail_C = eval_C D p p_def.internal_composition in
	      { local = same_fail_C; global = same_fail_C }
	    in
	    let same_fail_i = eval_act (module Theta) p_def.state_actions.during_act >> eval_T p_def.inner_trans wrapper_i success fail_i in
	    { local = same_fail_i; global = same_fail_i }
	  in
	  eval_T p_def.outer_trans wrapper_o success fail_o
	)
	| X -> fun frontier -> Transformer.(
	  Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "@[<v 2>S_%a[[node %a, frontier %a]]@ " pp_tag tag pp_path p pp_frontier frontier);
	  eval_C X p p_def.internal_composition >>
	  ((frontier = Loose) >? (eval_act (module Theta) p_def.state_actions.exit_act >> eval_act (module Theta) (close_path p)))
	)
  end

  module type ProgType =
  sig
    val init : state_name_t
    val defs : prog_t src_components_t list
  end

  module type EvaluationType =
  sig
    module Theta : ThetaType with type t = Transformer.t
    val eval_prog : Transformer.t
    val eval_components : 'c call_t -> ('c * Transformer.t) list
  end

  module Evaluation (Thetaify : ThetaifyType) (Prog : ProgType) : EvaluationType =
  struct
    module Denotation = Denotation (Thetaify)
    module AbsDenotation = AbstractKenv (Denotation)
    include AbsDenotation

    module Kenv : KenvType =
    struct
      let kenv =
	List.fold_left (
	  fun accu d -> match d with
          | State (p, defp)    -> { accu with cont_node = (p, ((fun kenv -> eval_S kenv E p defp),
                                                               (fun kenv -> eval_S kenv D p defp),
                                                               (fun kenv -> eval_S kenv X p defp)))::accu.cont_node }
	  | Junction (j, defj) -> { accu with cont_junc = (j, (fun kenv -> eval_T kenv defj))::accu.cont_junc }
          | SFFunction _       -> accu
        ) {cont_node = []; cont_junc = []} Prog.defs
    end

    module AppDenotation = Denotation (Kenv)
    module Theta = AppDenotation.Theta

    let eval_components = Theta.components

    let eval_prog =
      Transformer.(eval_cond (active [Prog.init]) (Theta.theta D [Prog.init]) (Theta.theta E [Prog.init] [] Loose))
  end
 (*
  module ThetaFix (Prog : ProgType) (Theta : ThetaType) : ThetaType =
  struct
    include Denotation (Theta)

    let theta =
      let kenv =
	List.fold_left (
	  fun accu d -> match d with
	  | State (p, defp) -> { accu with cont_node = (p, (eval_S E p defp, eval_S D p defp, eval_S X p defp))::accu.cont_node  }
	  | Junction (j, defj) -> { accu with cont_junc = (j, (eval_T defj))::accu.cont_junc  }
	) {cont_node = []; cont_junc = []} Prog.defs
      in Obj.magic (fun tag -> theta_ify kenv tag)
  end
  module Rec (Prog : ProgType) =
  struct
    module rec Theta : ThetaType = ThetaFix (Prog) (Theta)
  end

  module Eval (Prog : ProgType) : EvaluationType =
  struct
    module RecTheta = Rec (Prog)
    module Theta = RecTheta.Theta

    let eval_prog =
	Transformer.(eval_cond (active [Prog.init]) (Theta.theta D [Prog.init]) (Theta.theta E [Prog.init] []))
  end

  module Eval (Prog : ProgType) =
  struct
    module ThetaFunctor (Evaluation : EvaluationType) : ThetaType =
    struct
      let theta tag = (theta_ify Evaluation.kenv) tag
    end
    module EvaluationFunctor (Theta : ThetaType) : EvaluationType =
    struct
      include Denotation (Theta)

      let kenv =
	List.fold_left (
	  fun accu d -> match d with
	  | State (p, defp) -> { accu with cont_node = (p, (fun kenv -> eval_S E p defp, eval_S D p defp, eval_S X p defp))::accu.cont_node  }
	  | Junction (j, defj) -> { accu with cont_junc = (j, (eval_T defj))::accu.cont_junc  }
	) {cont_node = []; cont_junc = []} Prog.defs

      let eval_prog =
	Transformer.(eval_cond (active [Prog.init]) (Theta.theta D [Prog.init]) (Theta.theta E [Prog.init] []))
    end
    module rec Theta : ThetaType = ThetaFunctor (Evaluation)
    and Evaluation : EvaluationType = EvaluationFunctor (Theta)
end
  *)
end
