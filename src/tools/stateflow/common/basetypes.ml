
let sf_level = 2

(* Basic datatype for model elements: state and junction name, events ... *)
type state_name_t         = string
type junction_name_t      = string
type path_t               = state_name_t list
type event_base_t         = string
type event_t              = event_base_t option
type user_variable_name_t = string

(* Connected to lustrec types *)
type base_action_t    = { defs : Lustre_types.statement list;
			  ainputs: Lustre_types.var_decl list;
			  aoutputs: Lustre_types.var_decl list;
			  avariables: Lustre_types.var_decl list;
			  (* ident: string; *)
			}
type base_condition_t = { expr: Lustre_types.expr;
			  cinputs: Lustre_types.var_decl list;
			  coutputs: Lustre_types.var_decl list;
			  cvariables: Lustre_types.var_decl list }

(* P(r)etty printers *)
let pp_state_name     = Format.pp_print_string
let pp_junction_name  = Format.pp_print_string
let pp_path fmt p     = Utils.fprintf_list ~sep:"." pp_state_name fmt p
let pp_event fmt e    = match e with None -> Format.fprintf fmt "none" | Some s -> Format.fprintf fmt "%s" s
let pp_base_act fmt a = Printers.pp_node_stmts fmt a.defs
let pp_base_cond fmt c= Printers.pp_expr fmt c.expr

(* Action and Condition types and functions. *)

(* Actions are defined by string + the opening and closing of states *)

(* This version is slightly more complex than the one of EMSOFT'05 to enable the
   use of function calls in action.

   TODO: these rich call type could be externalized and a functor introduced. *)

type frontier_t =
  | Loose
  | Strict

let pp_frontier fmt frontier =
  match frontier with
  | Loose  -> Format.fprintf fmt "Loose"
  | Strict -> Format.fprintf fmt "Strict"

type _ call_t =
  | Ecall : (path_t * path_t * frontier_t) call_t
  | Dcall : path_t call_t
  | Xcall : (path_t * frontier_t) call_t

let pp_call :
type a. Format.formatter -> a call_t -> unit =
  fun fmt call ->
    match call with
    | Ecall -> Format.fprintf fmt "CallE"
    | Dcall -> Format.fprintf fmt "CallD"
    | Xcall -> Format.fprintf fmt "CallX"

module type ActionType =
sig
  type t
  val nil : t
  val aquote : base_action_t -> t
  val open_path : path_t -> t
  val close_path : path_t -> t
  val call : 'c call_t -> 'c -> t

  val pp_act : Format.formatter -> t -> unit
end


module Action =
struct
  type t =
    | Quote : base_action_t -> t
    | Close : path_t -> t
    | Open  : path_t -> t
    | Call  : 'c call_t * 'c -> t
    | Nil   : t


  let nil = Nil
  let aquote act = Quote act
  let open_path p = Open p
  let close_path p = Close p
  let call c a = Call (c, a)

  let pp_call : type c. Format.formatter -> c call_t -> c -> unit =
    fun fmt call ->
    match call with
    | Ecall -> (fun (p, p', f) -> Format.fprintf fmt "%a(%a, %a, %a)" pp_call call pp_path p pp_path p' pp_frontier f)
    | Dcall -> (fun p          -> Format.fprintf fmt "%a(%a)" pp_call call pp_path p)
    | Xcall -> (fun (p, f)     -> Format.fprintf fmt "%a(%a, %a)" pp_call call pp_path p pp_frontier f)

  let pp_act fmt act =
    match act with
    | Call (c, a)      -> pp_call fmt c a
    | Quote a          -> Format.fprintf fmt "%a" pp_base_act a
    | Close p          -> Format.fprintf fmt "Close(%a)" pp_path p
    | Open p           -> Format.fprintf fmt "Open(%a)" pp_path p
    | Nil              -> Format.fprintf fmt "Nil"
end

let _ = (module Action : ActionType)


(* Conditions are either (1) simple strings, (2) the active status of a state or
   (3) occurence of an event. They can be combined (conjunction, negation) *)
module type ConditionType =
sig
  type t
  val cquote : base_condition_t -> t
  val tru : t
  val active : path_t -> t
  val event : event_t -> t
  val ( && ) : t -> t -> t
  val neg : t -> t

  val pp_cond : Format.formatter -> t -> unit
end
  
module Condition =
struct
  type t =
    | Quote of base_condition_t
    | Active of path_t
    | Event of event_base_t
    | And of t * t
    | Neg of t
    | True

  let cquote cond = Quote cond
  let tru = True
  let neg cond = Neg cond
  let ( && ) cond1 cond2 = And (cond1, cond2)
  let active path = Active path
  let event evt =
    match evt with
    | None   -> True
    | Some e -> Event e

  let rec pp_cond fmt cond =
    match cond with
    | True               -> Format.fprintf fmt "true"
    | Active p           -> Format.fprintf fmt "Active(%a)" pp_path p
    | Event e            -> Format.fprintf fmt "Event(%s)" e
    | Neg cond           -> Format.fprintf fmt "(neg %a)" pp_cond cond
    | And (cond1, cond2) -> Format.fprintf fmt "%a /\\ %a" pp_cond cond1 pp_cond cond2
    | Quote c            -> Format.fprintf fmt "%a" pp_base_cond c

end

let _ = (module Condition : ConditionType)

module GlobalVarDef =
struct
  type t = {variable: Lustre_types.var_decl; init_val: Lustre_types.expr}
end
