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
  val mktransformer : Format.formatter -> t -> unit
  val mkprincipal : t -> LustreSpec.program
  val mkcomponent : 'c call_t -> 'c -> t -> LustreSpec.program
end

module type ComparableTransformerType =
sig
  include TransformerType

  val ( == ) : t -> t -> bool
end


  
  
