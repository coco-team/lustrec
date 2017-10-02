open Yojson
open Datatype
(* open Simulink *)
(* open Transformer *)
open Basetypes
open Basic
open Corelang
open CPS
open LustreSpec

module type ParseExt =
sig
  val parse_condition : json -> Condition.t
  val parse_action : json -> Action.t
  val parse_event : json -> Basetypes.event_t
end

module Parser (Ext : ParseExt) =
struct
  let path_split = String.split_on_char '/'
  let path_concat = String.concat (String.make 1 '/')

  open Util

  let to_list json =
    try
      json |> to_list
    with
      Type_error _ -> [ json ]

  let rec parse_prog json : prog_t =
     (*Format.printf "parse_prog@.";*)
    Program (
      json |> member "name"        |> to_string,
     (json |> member "states"      |> to_list |> List.map parse_state) @
     (json |> member "junctions"   |> to_list |> List.map parse_junction)
     @
     (json |> member "sffunctions" |> to_list |> List.map
        (fun res -> SFFunction (parse_prog res))),
      json |> member "data"        |> to_list |> List.map parse_variable
    )
  (* and parse_variables json = *)
  (*    (\*Format.printf "parse_variables@.";*\) *)
  (*   json |> member "data"       |> to_list |> List.map parse_variable *)
  and parse_state json =
    (*Format.printf "parse_state@.";*)
    State (
      json |> member "path" |> parse_path,
      json |> parse_state_def
    )
  and parse_path json =
      (*Format.printf "parse_path@.";*)
      json |> to_string |> path_split
  and parse_state_def json =
    (*Format.printf "parse_state_def@.";*)
    {
      state_actions        = json |> member "state_actions"        |> parse_state_actions;
      outer_trans          = json |> member "outer_trans"          |> to_list |> List.map parse_transition;
      inner_trans          = json |> member "inner_trans"          |> to_list |> List.map parse_transition;
      internal_composition = json |> member "internal_composition" |> parse_internal_composition
    }
  and parse_state_actions json =
    (*Format.printf "parse_state_actions@.";*)
    {
      entry_act  = json |> member "entry_act"  |> Ext.parse_action;
      during_act = json |> member "during_act" |> Ext.parse_action;
      exit_act   = json |> member "exit_act"   |> Ext.parse_action;
    }
  and parse_transition json =
    (*Format.printf "parse_transition@.";*)
    {
      event          = json |> member "event"          |> Ext.parse_event;
      condition      = json |> member "condition"      |> Ext.parse_condition;
      condition_act  = json |> member "condition_act"  |> Ext.parse_action;
      transition_act = json |> member "transition_act" |> Ext.parse_action;
      dest           = json |> member "dest"           |> parse_dest
    }
  and parse_dest json =
    (*Format.printf "parse_dest@.";*)
    (json |> member "type" |> to_string |>
	(function
	| "State"    -> (fun p -> DPath p)
	| "Junction" -> (fun j -> DJunction (path_concat j))
	| _ -> assert false))
      (json |> member "name" |> parse_path)
  and parse_internal_composition json =
    (*Format.printf "parse_internal_composition@.";*)
    (json |> member "type" |> to_string |>
	(function
	| "EXCLUSIVE_OR" -> (fun tinit substates ->                      Or  (tinit, substates))
	| "PARALLEL_AND" -> (fun tinit substates -> assert (tinit = []); And (substates))
	| _ -> assert false))
      (json |> member "tinit"     |> parse_tinit)
      (json |> member "substates" |> to_list |> List.map to_string)
  and parse_tinit json =
    (*Format.printf "parse_tinit@.";*)
    json |> to_list |> List.map parse_transition
  and parse_junction json =
    (*Format.printf "parse_junction@.";*)
    Junction (
      json |> member "path"        |> to_string,
      json |> member "outer_trans" |> to_list |> List.map parse_transition
    )
  and scope_of_string s =
    match s with
    | "Constant"  -> Constant
    | "Input"     -> Input
    | "Local"     -> Local
    | "Output"    -> Output
    | "Parameter" -> Parameter
    | _           -> failwith ("Invalid scope for variable: " ^ s)
  and datatype_of_json json =
    let datatype = json |> member "datatype" |> to_string in
    let init_value = json |> member "initial_value" |> to_string in
    match datatype with
    | "bool" -> Bool (bool_of_string init_value)
    | "int"  -> Int  (int_of_string init_value)
    | "real" -> Real (float_of_string init_value)
    | _      -> failwith ("Invalid datatype " ^ datatype
                          ^ " for variable " ^ (json |> member "name"
                                                |> to_string))
  and lustre_datatype_of_json json location =
    let datatype      = json |> member "datatype"      |> to_string in
    let initial_value = json |> member "initial_value" |> to_string in
    match datatype with
    | "bool" -> (Tydec_bool, mkexpr location
                   (Expr_const (Const_bool
                                  (bool_of_string initial_value))))
    | "int"  -> (Tydec_int, mkexpr location
                   (Expr_const (Const_int (int_of_string
                                             initial_value))))
    | "real" -> (Tydec_real, mkexpr location
                   (Expr_const (Const_real (Num.num_of_int 0,
                                           0,
                                           initial_value))))
    | _      -> failwith ("Invalid datatype " ^ datatype
                          ^ " for variable " ^ (json |> member "name"
                                                |> to_string))
  and parse_variable json =
    (*Format.printf "parse_variable@.";*)
    let location                  = Location.dummy_loc in
    let (datatype, initial_value) = lustre_datatype_of_json json location in
    mkvar_decl location ~orig:true
      ( json |> member "name" |> to_string,
        {ty_dec_desc = datatype;  ty_dec_loc = location},
        {ck_dec_desc = Ckdec_any; ck_dec_loc = location},
        false,
        Some initial_value
      )
end
