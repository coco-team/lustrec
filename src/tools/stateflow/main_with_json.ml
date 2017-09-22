open Env
open Basetypes
open Datatype
(* open Interpreter *)
open Parser_json
open Transformer
open Theta

module ParseExt =
struct
  let parse_condition _ = Transformer.Condition.tru
  let parse_action    _ = Transformer.Action.nil
  let parse_event json  = Some Yojson.Basic.(json |> to_string)
end

module Parse = Parser (ParseExt)

module Prog =
struct
  let json                       = Yojson.Basic.from_file "GPCA_Alarm_Alarm_SFIR_pp.json"
  let Simulink.Prog (init, defs) = Parse.parse_prog json
  let prog                       = Parse.parse_prog json
  let user_vars                  = Parse.parse_variables json
  let vars                       = Simulink.states (Simulink.Prog (init, defs))
  (*let _ = Format.printf "Model definitions@.%a@.####" Simulink.pp_src defs; ()*)
end

module Transformer =
  (* Transformer.LustrePrinter (Prog) *)
  Transformer.CodeGenerator
  (* Transformer.Evaluator *)

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
  let principal = EvalProg.eval_prog in
  let components = EvalProg.eval_components in
  begin
    Format.printf "%a@." Transformer.pp_principal principal;
    List.iter
      (fun (c, tr) -> Format.printf "@.%a@." (fun fmt -> Transformer.pp_component fmt Ecall c) tr)
      (components Ecall);
    List.iter
      (fun (c, tr) -> Format.printf "@.%a@." (fun fmt -> Transformer.pp_component fmt Dcall c) tr)
      (components Dcall);
    List.iter
      (fun (c, tr) -> Format.printf "@.%a@." (fun fmt -> Transformer.pp_component fmt Xcall c) tr)
      (components Xcall);
    Simulink.pp_prog Format.std_formatter (Parse.parse_prog Prog.json);
    Simulink.pp_vars Format.std_formatter (Parse.parse_variables Prog.json);
  end

let _ = main ()
