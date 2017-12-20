open Basetypes
open Cmdliner
open Datatype
open Json_parser
open Sys

module ParseExt =
struct
  open Yojson.Basic
    
      
  let remove_quotes s =
    let len = String.length s in
    if String.get s 0 = '"' && String.get s (len-1) = '"' then
      String.sub s 1 (len-2)
    else (
      Format.eprintf "No quotes in string %s@.@?" s;
      assert false
    )

  let get_vars json =
      let get_vdecls key json =
	let s = json |> Util.member key |> to_string in
	try
	  let s'= remove_quotes s in
	  if s' = "" then [] else  
	    let lexbuf = Lexing.from_string s' in
	    Parser_lustre.vdecl_list Lexer_lustre.token lexbuf
	with _ -> (Format.eprintf "Issues parsing decls for %s: %s@.@?" key s; assert false)
	  
      in
      let inputs = get_vdecls "inputs" json in
      let outputs = get_vdecls "outputs" json in
      let variables = get_vdecls "variables" json in
      inputs, outputs, variables
      
  (* Protecting the generation of condition/action in case of an empty string
     instead of a subtree *)
  let protect default parse_fun embed_fun json =
    try
      let vars = get_vars json in
	let actions = json |> Util.member "actions" |> to_string in
	if actions = "[]" || actions = "" then default (* should not happen *) else (
	  Format.eprintf "Parsing string: %s@." actions;
	  let lexbuf = Lexing.from_string (remove_quotes actions) in
	  try
	    let content = parse_fun Lexer_lustre.token lexbuf in
	    Parsing.clear_parser ();
	    embed_fun content vars
	  with Parsing.Parse_error ->
	    let loc = Location.dummy_loc in
	    raise (Parse.Error (loc, Parse.String_Syntax_error actions))
	)
    with Util.Type_error _ -> (
      Format.eprintf
	"Unable to explore json subtree: empty string %s@." (to_string json);
      default
    )
      
  let parse_condition =
    protect
      Condition.tru
      Parser_lustre.expr
      (fun e (in_,out_,locals_) ->
	(* let vars = Corelang.get_expr_vars e in *)
	Condition.cquote {
	expr =  e;
	cinputs = in_;
	coutputs = out_;
	cvariables = locals_;
	  
      })
    
  let parse_action =
    protect Action.nil Parser_lustre.stmt_list
      (fun (stmts, asserts, annots) (in_, out_, locals_) ->
	if asserts != [] || annots != [] then
	  assert false (* Stateflow equations should not use asserts nor define
			  annotations *)
	else
	  Action.aquote ({
	    defs = stmts;
	    ainputs = in_;
	    aoutputs = out_;
	    avariables = locals_;
	  })
      )
      
  let parse_event json  = Some Yojson.Basic.(json |> to_string)
end

module JParse = Parser (ParseExt)

(* setup for logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let modular = ref 0

(* function representing the program to execute *)
let json_parse _ file pp =
  try
    let prog = JParse.parse_prog (Yojson.Basic.from_file file) in
    if pp then
      SF.pp_prog Format.std_formatter prog;

    let module Model =
	struct
	  let model = prog
	  let name = "toto" (* TODO find a meaningful name *)
	  let traces = [] (* TODO: shall we remove the traces field? *)
	end
    in
    let modularmode =
      match !modular with
      | 2 -> true, true, true
      | 1 -> false, true, false
      | _ (* 0 *) -> false, false ,false
    in
    let state_vars = Datatype.SF.states Model.model in
    let global_vars = Datatype.SF.global_vars Model.model in
    
    let module T = CPS_lustre_generator.LustrePrinter (struct
      let state_vars = state_vars
      let global_vars = global_vars 
    end) in
    let module Sem = CPS.Semantics (T) (Model) in
    let prog = Sem.code_gen modularmode in
    Options.print_dec_types := true;
    Format.printf "%a@." Printers.pp_prog prog;

    let auto_file = "sf_gen_test_auto.lus" in (* Could be changed *)
    let auto_out = open_out auto_file in
    let auto_fmt = Format.formatter_of_out_channel auto_out in
    Format.fprintf auto_fmt "%a@." Printers.pp_prog prog;

    let prog = (LustreSpec.Open ("math",false))::prog
    let prog, deps = Compiler_stages.stage1 prog "" "" in

    Format.printf "%a@." Printers.pp_prog prog;
    let noauto_file = "sf_gen_test_noauto.lus" in (* Could be changed *)
    let noauto_out = open_out noauto_file in
    let noauto_fmt = Format.formatter_of_out_channel noauto_out in
    Format.fprintf noauto_fmt "%a@." Printers.pp_prog prog


      

  with Parse.Error (l, err) -> Format.eprintf "Parse error at loc %a : %a@.@?" Location.pp_loc l Parse.pp_error err

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
  Term.info "parse_json_file" ~doc ~exits:Term.default_exits ~man

(* program *)
let _ =
  Term.exit @@ Term.eval (json_parse_t, info)
