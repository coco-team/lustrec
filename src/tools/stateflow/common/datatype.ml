open Basetypes
(* open ActiveEnv *)

(* Type definitions of a model *)

type destination_t =
  | DPath of path_t
  | DJunction of junction_name_t

type trans_t = {
  event: event_t;
  condition: Condition.t;
  condition_act: Action.t;
  transition_act: Action.t;
  dest: destination_t;
}

type transitions_t = trans_t list

type composition_t =
  | Or of transitions_t * state_name_t list
  | And of state_name_t list

type state_actions_t = {
  entry_act: Action.t;
  during_act: Action.t;
  exit_act: Action.t;
}

type state_def_t = {
  state_actions : state_actions_t;
  outer_trans : transitions_t;
  inner_trans : transitions_t;
  internal_composition : composition_t;
}

type 'prog_t src_components_t =
  | State of path_t * state_def_t
  | Junction of junction_name_t * transitions_t
  | SFFunction of 'prog_t

type prog_t = Program of state_name_t * prog_t src_components_t list * (Lustre_types.var_decl * Lustre_types.expr) list

type scope_t = Constant | Input | Local | Output | Parameter

type datatype_var_init_t = Bool of bool | Real of float | Int of int

type user_variable_t = user_variable_name_t * scope_t * datatype_var_init_t

type trace_t = event_t list

module type MODEL_T = sig
  val name : string
  val model : prog_t
  val traces: trace_t list
end

(* Module (S)tate(F)low provides basic constructors for action, condition,
   events, as well as printer functions *)
module SF =
struct

  (* Basic constructors *)

  let no_action    = Action.nil
  let no_condition = Condition.tru
  let no_event     = None
  let event s      = Some s
  let action s     = Action.aquote s
  let condition s  = Condition.cquote s
  let no_state_action    = {entry_act = no_action; during_act = no_action; exit_act = no_action; }
  let state_action a b c = {entry_act = a; during_act = b; exit_act = c; }

  let states (Program (_, defs, _)) =
    List.fold_left
      (fun res c ->
	match c with
	| State (p, _) -> ActiveStates.Vars.add p res
	| Junction _  -> res
        | SFFunction _ -> res
      )
      ActiveStates.Vars.empty defs

  let init_env model = ActiveStates.Env.from_set (states model) false

  let global_vars (Program (_, _, env)) = env

  (* Printers *)

  let pp_event fmt e =
    match e with
    | Some e -> Format.fprintf fmt "%s" e
    | None -> Format.fprintf fmt "noevent"

  let pp_dest fmt d = match d with
    | DPath p -> Format.fprintf fmt "Path %a" pp_path p
    | DJunction j -> Format.fprintf fmt "Junction %a" pp_junction_name j

  let pp_trans fmt t =
    Format.fprintf fmt
      "@[<hov 0>(@[<hov 0>%a,@ %a,@ %a,@ %a,@ %a@]@ )@]"
      pp_event t.event
      Condition.pp_cond t.condition
      Action.pp_act t.condition_act
      Action.pp_act t.transition_act
      pp_dest t.dest

  let pp_transitions fmt l =
    Format.fprintf fmt
      "@[<hov 0>[@[<hov 0>%a@]@ ]@]"
      (Utils.fprintf_list ~sep:";@ " pp_trans) l

  let pp_comp fmt c = match c with
    | Or (_T, _S) ->
       Format.fprintf fmt "Or(%a, {%a})"
	 pp_transitions _T
	 (Utils.fprintf_list ~sep:"; " pp_state_name) _S
    | And (_S) ->
       Format.fprintf fmt "And({%a})"
	 (Utils.fprintf_list ~sep:"; " pp_state_name) _S

  let pp_state_actions fmt sa =
    Format.fprintf fmt "@[<hov 0>(%a,@ %a,@ %a)@]"
      Action.pp_act sa.entry_act
      Action.pp_act sa.during_act
      Action.pp_act sa.exit_act

  let pp_state fmt s =
    Format.fprintf fmt "@[<v 0>(@[<v 0>%a,@ %a,@ %a,@ %a@]@ @])"
      pp_state_actions s.state_actions
      pp_transitions s.outer_trans
      pp_transitions s.inner_trans
      pp_comp s.internal_composition

  let pp_src pp_sffunction fmt src =
    Format.fprintf fmt "@[<v>%a@ @]"
      (Utils.fprintf_list ~sep:"@ @ "
         (fun fmt src -> match src with
	    | State (p, def)   -> Format.fprintf fmt "%a: %a"
                                  pp_path p pp_state def
            | Junction (s, tl) -> Format.fprintf fmt "%a: %a"
	                            pp_state_name s
	                            pp_transitions tl
            | SFFunction p     -> pp_sffunction fmt p
         ))
      src

  let rec pp_sffunction fmt (Program (name, component_list, _)) =
    Format.fprintf fmt "SFFunction name: %s@ %a@ "
      name
      (pp_src pp_sffunction) component_list

  let pp_vars fmt src =
    Format.fprintf fmt "@[<v>%a@ @]"
      (Utils.fprintf_list ~sep:"@ " Printers.pp_var)
    src

  let pp_prog fmt (Program (name, component_list, vars)) =
    Format.fprintf fmt "Main node name: %s@ %a@ %a@"
      name
      (pp_src pp_sffunction) component_list
      pp_vars (List.map fst vars)

  let pp_scope fmt src =
    Format.fprintf fmt (match src with
        | Constant  -> "Constant"
        | Input     -> "Input"
        | Local     -> "Local"
        | Output    -> "Output"
        | Parameter -> "Parameter")
end
