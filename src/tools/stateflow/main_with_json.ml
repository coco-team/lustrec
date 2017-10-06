open Basetypes
open Cmdliner
open Datatype
open Parser_json
open Sys

module ParseExt =
struct
  let parse_condition _ = Condition.tru
  let parse_action    _ = Action.nil
  let parse_event json  = Some Yojson.Basic.(json |> to_string)
end

module Parse = Parser (ParseExt)

(* setup for logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(* function representing the program to execute *)
let json_parse _ file pp =
  let prog = Parse.parse_prog (Yojson.Basic.from_file file) in
  if pp then
    SF.pp_prog Format.std_formatter prog

(* term representing argument for file *)
let file =
  let doc = "The file to parse." in
  let env = Arg.env_var "JSON_FILE" ~doc in
  let doc = "The file to parse." in
  Arg.(required & pos 0 (some string) None & info [] ~env ~docv:"FILE" ~doc)

(* term representing argument for flag for pretty printing the program *)
let pp =
  let doc = "Pretty print the resulting program" in
  Arg.(value & flag & info ["pp"; "pretty-print"] ~docv:"PP" ~doc)

(* term for argument for logging *)
let setup_log_arg =
  let env = Arg.env_var "TOOL_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

(* term representing the program to execute *)
let json_parse_t = Term.(const json_parse $ setup_log_arg $ file $ pp)

(* term info for manpages etc. *)
let info =
  let doc = "parse a JSON file representing a Stateflow model" in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bug to Github issues tracking." ]
  in
  Term.info "json-parser-example" ~doc ~exits:Term.default_exits ~man

let main () =
  begin
    let json = Yojson.Basic.from_file Sys.argv.(1) in
    SF.pp_prog Format.std_formatter (Parse.parse_prog json);
  end

(* program *)
let _ = Term.exit @@ Term.eval (json_parse_t, info)
