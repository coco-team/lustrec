open Basetypes
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

let main ()  =
  begin
    let json = Yojson.Basic.from_file Sys.argv.(1) in
    SF.pp_prog Format.std_formatter (Parse.parse_prog json);
  end

let _ = main ()
