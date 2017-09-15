 open Basetypes 
(* open ActiveEnv *)
open CPS_transformer 
(* open Datatype *)
(* open Simulink *)

(* Theta functions interfaces including memoization when using modular calls 
   
   parametrized by the transformer specifying the type of the
   continuation. Evaluation of actions is also continuation dependent.
*)

module KenvTheta (T : TransformerType) =
struct

  type kenv_t = {
    cont_node : (
      path_t * (
	(kenv_t -> path_t -> frontier_t -> T.t) *
	(kenv_t -> T.t) *
	(kenv_t -> frontier_t -> T.t)
      )
    ) list;
    cont_junc : (
      junction_name_t * (
	kenv_t -> T.t wrapper_t -> T.t success_t -> T.t fail_t -> T.t)
    ) list
  }

	
  let init_env src =
    List.fold_left (fun accu d ->
      match d with
      | Datatype.State (p, _) -> ActiveStates.Env.add p false accu
      | _ -> accu)
      ActiveStates.Env.empty
      src

  module type KenvType =
  sig
    val kenv : kenv_t
  end

  module type MemoThetaTablesType =
  sig
    val tables : 'c call_t -> ('c, T.t) Memo.t
  end

  module type MemoThetaType =
  sig
    include ThetaType with type t = T.t
    val components : 'c call_t -> ('c * t) list
  end

  module type ModularType =
  sig
    val modular : (path_t, 'b, bool) tag_t -> path_t -> 'b
  end

  module MemoThetaTables : functor () -> MemoThetaTablesType = functor () ->
  struct
    let table_theta_e = (Memo.create () : (path_t * path_t * frontier_t, T.t) Memo.t)
    let table_theta_d = (Memo.create () : (path_t, T.t) Memo.t)
    let table_theta_x = (Memo.create () : (path_t * frontier_t, T.t) Memo.t)

    let tables : type c. c call_t -> (c, T.t) Memo.t =
      fun call ->
      match call with
      | Ecall -> table_theta_e
      | Dcall -> table_theta_d
      | Xcall -> table_theta_x
  end

  module Memoize (Tables : MemoThetaTablesType) (Theta : ThetaType with type t = T.t) : MemoThetaType =
  struct
    type t = Theta.t

    let components call =
      Memo.fold (Tables.tables call) (fun k v res -> (k, v)::res) []

    let theta : type a b. (a, b, t) theta_t =
      fun tag ->
      match tag with
      | J -> Theta.theta J
      | E -> Memo.apply3 (Tables.tables Ecall) (Theta.theta E)
      | D -> Memo.apply  (Tables.tables Dcall) (Theta.theta D)
      | X -> Memo.apply2 (Tables.tables Xcall) (Theta.theta X)
  end
    
  module Modularize (Mod : ModularType) (Theta : MemoThetaType) : MemoThetaType =
  struct
    type t = Theta.t

    let mod_theta = (module Theta : ThetaType with type t = T.t)

    let components : type c. c call_t -> (c * t) list =
      fun call ->
      match call with
      | Ecall -> List.filter (fun ((p, p', f), _) -> Mod.modular E p p' f) (Theta.components Ecall)
      | Dcall -> List.filter (fun (p         , _) -> Mod.modular D p)      (Theta.components Dcall)
      | Xcall -> List.filter (fun ((p, f)    , _) -> Mod.modular X p f)    (Theta.components Xcall)

    let theta : type a b. (a, b, t) theta_t =
      fun tag ->
      match tag with
      | J -> Theta.theta tag
      | E -> (fun p p' f -> let theta_e = Theta.theta tag p p' f in
	if Mod.modular E p p' f
	then T.(eval_act mod_theta (call Ecall (p, p', f)))
	else theta_e)
      | D -> (fun p -> let theta_d = Theta.theta tag p in
	if Mod.modular D p
	then T.(eval_act mod_theta (call Dcall p))
	else theta_d)
      | X -> (fun p f -> let theta_x = Theta.theta tag p f in
	if Mod.modular X p f
	then T.(eval_act mod_theta (call Xcall (p, f)))
	else theta_x)
  end

  module type ThetaifyType = functor (Kenv : KenvType) -> MemoThetaType

  module BasicThetaify : ThetaifyType = functor (Kenv : KenvType) ->
  struct
    type t = T.t

    let theta_ify : type a b. kenv_t -> (a, b, T.t) theta_t =
      fun kenv tag ->
      match tag with
      | J -> (fun j ->
	try List.assoc j kenv.cont_junc kenv
	with Not_found ->
	  Format.eprintf "Lost junction %a@ " pp_junction_name j;
	  assert false)
      | E -> (fun p ->
	try
	  let (e, _, _) = List.assoc p kenv.cont_node in e kenv
	with Not_found -> 
	  Format.eprintf "Entering lost path [%a]@." pp_path p; assert false)
      | D -> (fun p ->
	try
	  let (_, d, _) = List.assoc p kenv.cont_node in d kenv
	with Not_found -> 
	  Format.eprintf "During lost path [%a]@." pp_path p; assert false)
      | X -> (fun p ->
	try
	  let (_, _, x) = List.assoc p kenv.cont_node in x kenv
	with Not_found -> 
	  Format.eprintf "Exiting lost path [%a]@." pp_path p; assert false)

    let theta tag = theta_ify Kenv.kenv tag

    let components call = []
  end

  module ModularThetaify : functor (Tables : MemoThetaTablesType) (Mod : ModularType) -> ThetaifyType =
    functor (Tables : MemoThetaTablesType) (Mod : ModularType) (Kenv : KenvType) ->
      Modularize (Mod) (Memoize (Tables) (BasicThetaify (Kenv)))

end
