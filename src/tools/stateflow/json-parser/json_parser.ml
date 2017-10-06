open Yojson
open Datatype
open Basetypes
open Basic
open Corelang
open LustreSpec
open Str

module type ParseExt =
sig
  val parse_condition : json -> Condition.t
  val parse_action    : json -> Action.t
  val parse_event     : json -> Basetypes.event_t
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
    Logs.info  (fun m -> m "parse_prog %s" (json |> member "name" |> to_string));
    Program (
      json |> member "name"        |> to_string,
     (json |> member "states"      |> to_list |> List.map parse_state) @
     (json |> member "junctions"   |> to_list |> List.map parse_junction)
     @
     (json |> member "sffunctions" |> to_list |> List.map
        (fun res -> SFFunction (parse_prog res))),
      json |> member "data"        |> to_list |> List.map parse_variable
    )
  and parse_state json =
    Logs.debug (fun m -> m "parse_state");
    State (
      json |> member "path" |> parse_path,
      json |> parse_state_def
    )
  and parse_path json =
    Logs.debug (fun m -> m "parse_path %s" (json |> to_string));
    json |> to_string |> path_split
  and parse_state_def json =
    Logs.debug (fun m -> m "parse_state_def");
    {
      state_actions        = json |> member "state_actions"        |> parse_state_actions;
      outer_trans          = json |> member "outer_trans"          |> to_list |> List.map parse_transition;
      inner_trans          = json |> member "inner_trans"          |> to_list |> List.map parse_transition;
      internal_composition = json |> member "internal_composition" |> parse_internal_composition
    }
  and parse_state_actions json =
    Logs.debug (fun m -> m "parse_state_actions");
    {
      entry_act  = json |> member "entry_act"  |> Ext.parse_action;
      during_act = json |> member "during_act" |> Ext.parse_action;
      exit_act   = json |> member "exit_act"   |> Ext.parse_action;
    }
  and parse_transition json =
    Logs.debug (fun m -> m "parse_transition");
    {
      event          = json |> member "event"          |> Ext.parse_event;
      condition      = json |> member "condition"      |> Ext.parse_condition;
      condition_act  = json |> member "condition_act"  |> Ext.parse_action;
      transition_act = json |> member "transition_act" |> Ext.parse_action;
      dest           = json |> member "dest"           |> parse_dest
    }
  and parse_dest json =
    Logs.debug (fun m -> m "parse_dest");
    let dest_type = json |> member "type" |> to_string in
    (dest_type |>
	(function
	| "State"    -> (fun p -> DPath p)
	| "Junction" -> (fun j -> DJunction (path_concat j))
	| _ -> failwith ("Invalid destination type: " ^ dest_type)))
      (json |> member "name" |> parse_path)
  and parse_internal_composition json =
    Logs.debug (fun m -> m "parse_internal_composition");
    let state_type = json |> member "type" |> to_string in
    (state_type |>
	(function
	| "EXCLUSIVE_OR" -> (fun tinit substates ->                      Or  (tinit, substates))
	| "PARALLEL_AND" -> (fun tinit substates -> assert (tinit = []); And (substates))
	| _ -> failwith ("Invalid state type: " ^ state_type)))
      (json |> member "tinit"     |> parse_tinit)
      (json |> member "substates" |> to_list |> List.map to_string)
  and parse_tinit json =
    Logs.debug (fun m -> m "parse_tinit");
    json |> to_list |> List.map parse_transition
  and parse_junction json =
    Logs.debug (fun m -> m "parse_junction");
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
  and parse_real_value s =
    Logs.debug (fun m -> m "parse_real_value %s" s);
    let real_regexp_simp = regexp "-?\\([0-9][0-9]*\\)\\.\\([0-9]*\\)" in
    let real_regexp_e    = regexp "-?\\([0-9][0-9]*\\)\\.\\([0-9]*\\)(E|e)\\((\\+|\\-)[0-9][0-9]*\\)" in
    if string_match real_regexp_e s 0 then
      let l = matched_group 1 s in
      let r = matched_group 2 s in
      let e = matched_group 3 s in
      Const_real (Num.num_of_string (l ^ r),
                  String.length r + -1 * int_of_string e,
                  s)
    else
    if string_match real_regexp_simp s 0 then
      let l = matched_group 1 s in
      let r = matched_group 2 s in
      Const_real (Num.num_of_string (l ^ r), String.length r, s)
    else
      failwith ("Invalid real constant " ^ s)
  and lustre_datatype_of_json json location =
    let datatype      = json |> member "datatype"      |> to_string in
    let initial_value = json |> member "initial_value" |> to_string in
    match datatype with
    | "bool" -> (Tydec_bool, mkexpr location
                   (Expr_const (Const_tag
                                  ((fun s -> match s with
                                     | "true"  -> tag_true
                                     | "false" -> tag_false
                                     | _       ->
                                       failwith ("Invalid constant for
     boolean: " ^ s)) initial_value))))
    | "int"  -> (Tydec_int, mkexpr location
                   (Expr_const (Const_int (int_of_string
                                             initial_value))))
    | "real" -> (Tydec_real, mkexpr location
                   (Expr_const (parse_real_value initial_value)))
    | _      -> failwith ("Invalid datatype " ^ datatype
                          ^ " for variable " ^ (json |> member "name"
                                                |> to_string))
  and parse_variable json =
    Logs.debug (fun m -> m "parse_variable %s" (json |> member "name" |> to_string));
    let location                  = Location.dummy_loc in
    let (datatype, initial_value) = lustre_datatype_of_json json location in
    mkvar_decl location ~orig:true
      ( json |> member "name" |> to_string,
        {ty_dec_desc = datatype;  ty_dec_loc = location},
        {ck_dec_desc = Ckdec_any; ck_dec_loc = location},
        true,
        Some initial_value
      )
end
