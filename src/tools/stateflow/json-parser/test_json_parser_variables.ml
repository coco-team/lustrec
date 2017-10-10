open Basetypes
open Corelang
open Datatype
open Json_parser
open LustreSpec
open OUnit2

module ParseExt =
struct
  let parse_condition _ = Condition.tru
  let parse_action    _ = Action.nil
  let parse_event json  = Some Yojson.Basic.(json |> to_string)
end

module Parse = Parser (ParseExt)

let location = Location.dummy_loc

let test_var_skeleton var id var_type value =
  begin
    assert_bool
      "orig for user variables should be true"
      var.var_orig;
    assert_bool
      "user variables are considered as constants"
      var.var_dec_const;
    assert_equal
      ~msg:("problem with variable " ^ var.var_id ^ " clock type")
      var.var_dec_clock.ck_dec_desc
      Ckdec_any;
    assert_equal
      ~msg:("problem with variable " ^ var.var_id ^ " ident")
      var.var_id
      id;
    assert_equal
      ~msg:("problem with variable " ^ var.var_id ^ " type")
      var.var_dec_type.ty_dec_desc
      var_type;
    match var.var_dec_value with
    | Some { expr_desc = d } ->
      assert_equal
        ~msg:("problem with variable " ^ var.var_id ^ " value")
        d
        value
    | _       -> raise (OUnitTest.OUnit_failure
                          "User variables should have an initial value")
  end

let test_simple_var_bool_false tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-bool-false.json") in
  match prog with
  | Program ("simple_var_bool_false", [ ], [ x ]) ->
    test_var_skeleton x "x" Tydec_bool (Expr_const (Const_tag tag_false))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-bool-false.json is not correct")

let test_simple_var_bool_true tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-bool-true.json") in
  match prog with
  | Program ("simple_var_bool_true", [ ], [ x ]) ->
    test_var_skeleton x "my_var" Tydec_bool (Expr_const (Const_tag tag_true))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-bool-true.json is not correct")

let var_suite =
  "suite for variables" >:::
  [ "simple test for variable (boolean, false)" >::
    test_simple_var_bool_false;
    "simple test for variable (boolean, true)"  >::
    test_simple_var_bool_true
  ]

let _ =
  run_test_tt_main var_suite
