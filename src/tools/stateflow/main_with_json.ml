open Env
open Basetypes
open Datatype
(* open Interpreter *)
open Parser_json
(* open Transformer *)
open Theta
open CPS_ccode_generator
open CPS_interpreter
open CPS_transformer

module ParseExt =
struct
  let parse_condition _ = Condition.tru
  let parse_action    _ = Action.nil
  let parse_event json  = Some Yojson.Basic.(json |> to_string)
end

module Parse = Parser (ParseExt)

module Prog =
struct
  let json                       = Yojson.Basic.from_file "GPCA_Alarm_Alarm_SFIR_pp.json"
  let Program (init, defs, vars) = Parse.parse_prog json
  let prog                       = Parse.parse_prog json
  let user_vars                  = Parse.parse_variables json
  (*let _ = Format.printf "Model definitions@.%a@.####" Simulink.pp_src defs; ()*)
end

module Transformer =
  (* CPS_ccode_generator.LustrePrinter (Program) *)
  CodeGenerator
  (* CPS_ccode_generator.Evaluator *)

module Interp = Interpreter (Transformer)

module KenvTheta = KenvTheta (Transformer)

module Modularity : KenvTheta.ModularType =
struct
  let modular : type b. (path_t, b, bool) tag_t -> path_t -> b =
    fun tag ->
    match tag with
    | E -> (fun p p' f -> true)
    | D -> (fun p      -> true)
    | X -> (fun p f    -> true)
end

module Tables = KenvTheta.MemoThetaTables ()

module Thetaify = KenvTheta.ModularThetaify (Tables) (Modularity)

module EvalProg = Interp.Evaluation (Thetaify) (Prog)

let main ()  =
  begin
    SF.pp_prog Format.std_formatter (Parse.parse_prog Prog.json);
    SF.pp_vars Format.std_formatter (Parse.parse_variables Prog.json);
  end

let _ = main ()
