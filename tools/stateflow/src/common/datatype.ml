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

type src_components_t =
  | State of path_t * state_def_t
  | Junction of junction_name_t * transitions_t

type prog_t = state_name_t * src_components_t list
  
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

  let states (_, defs) =
    List.fold_left (fun res c -> match c with State (p, _) -> ActiveStates.Vars.add p res | Junction _ -> res) ActiveStates.Vars.empty defs

  let init_env model = ActiveStates.Env.from_set (states model) false
    
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
      
  let pp_src fmt src =
    Format.fprintf fmt "@[<v>%a@ @]"
      (Utils.fprintf_list ~sep:"@ @ " (fun fmt src -> match src with
	State (p, def) ->
	  Format.fprintf fmt "%a: %a"
	    pp_path p
	    pp_state def
      | Junction (s, tl) ->
	 Format.fprintf fmt "%a: %a"
	   pp_state_name s
	   pp_transitions tl
       ))
      src
end
   
