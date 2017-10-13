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

let string_of_var_type var_type =
  match var_type with
  | Tydec_bool -> "bool"
  | Tydec_int  -> "int"
  | Tydec_real -> "real"
  | _          -> "other"

let string_of_var_value value =
  match value with
  | Expr_const (Const_tag label)      -> label
  | Expr_const (Const_int v)          -> string_of_int v
  | Expr_const (Const_real (n, l, s)) -> (Num.string_of_num n) ^
                                         " x 10^-" ^
                                         (string_of_int l) ^
                                         " (" ^ s ^ ")"
  | _                   -> "other value (not possible)"

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
      Ckdec_any
      var.var_dec_clock.ck_dec_desc;
    assert_equal
      ~msg:("problem with variable " ^ var.var_id ^ " ident")
      ~printer:(fun x -> x)
      id
      var.var_id;
    assert_equal
      ~msg:("problem with variable " ^ var.var_id ^ " type")
      ~printer:string_of_var_type
      var_type
      var.var_dec_type.ty_dec_desc;
    match var.var_dec_value with
    | Some { expr_desc = d } ->
      assert_equal
        ~msg:("problem with variable " ^ var.var_id ^ " value")
        ~printer:string_of_var_value
        value
        d
    | _       -> raise (OUnitTest.OUnit_failure
                          "User variables should have an initial value")
  end

let test_simple_var_bool_false tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-bool-false.json") in
  match prog with
  | Program ("simple_var_bool_false", [ ], [ x ]) ->
    test_var_skeleton x "my_bool_var_false"
      Tydec_bool (Expr_const (Const_tag tag_false))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-bool-false.json is not correct")

let test_simple_var_bool_true tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-bool-true.json") in
  match prog with
  | Program ("simple_var_bool_true", [ ], [ x ]) ->
    test_var_skeleton x "my_bool_var_true"
      Tydec_bool (Expr_const (Const_tag tag_true))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-bool-true.json is not correct")

let test_simple_var_int_zero tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-int-zero.json") in
  match prog with
  | Program ("simple_var_int_zero", [ ], [ x ]) ->
    test_var_skeleton x "my_int_var_zero"
      Tydec_int (Expr_const (Const_int 0))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-int-zero.json is not correct")

let test_simple_var_int_pos tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-int-pos.json") in
  match prog with
  | Program ("simple_var_int_pos", [ ], [ x ]) ->
    test_var_skeleton x "my_int_var_pos"
      Tydec_int (Expr_const (Const_int 2))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-int-pos.json is not correct")

let test_simple_var_int_neg tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-int-neg.json") in
  match prog with
  | Program ("simple_var_int_neg", [ ], [ x ]) ->
    test_var_skeleton x "my_int_var_neg"
      Tydec_int (Expr_const (Const_int (-5)))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-int-neg.json is not correct")

let test_simple_var_real_zero tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-real-zero.json") in
  match prog with
  | Program ("simple_var_real_zero", [ ], [ x ]) ->
    test_var_skeleton x "my_real_var_zero"
      Tydec_real (Expr_const (Const_real (Num.num_of_int 0, 1, "0.0")))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-real-zero.json is not correct")

let test_simple_var_real_pos tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-real-pos.json") in
  match prog with
  | Program ("simple_var_real_pos", [ ], [ x ]) ->
    test_var_skeleton x "my_real_var_pos"
      Tydec_real (Expr_const (Const_real (Num.num_of_int 2115, 2, "21.15")))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-real-pos.json is not correct")

let test_simple_var_real_neg tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-real-neg.json") in
  match prog with
  | Program ("simple_var_real_neg", [ ], [ x ]) ->
    test_var_skeleton x "my_real_var_neg"
      Tydec_real (Expr_const (Const_real (Num.num_of_int (-224), 2, "-2.24")))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-real-neg.json is not correct")

let test_simple_var_real_e tests_ctxt =
  let prog = Parse.parse_prog
      (Yojson.Basic.from_file "../data-test/simple-var-real-e.json") in
  match prog with
  | Program ("simple_var_real_e", [ ], [ x ]) ->
    test_var_skeleton x "my_real_var_e"
      Tydec_real (Expr_const (Const_real (Num.num_of_int (-2115), 4, "-21.15e-02")))
  | _ -> raise (OUnitTest.OUnit_failure
                  "Program obtained from simple-var-real-e.json is not correct")

let test_simple_var_real_wo_dec tests_ctxt =
  assert_raises (Parse.JSON_parse_error("Invalid real constant 2500"))
    (fun _ -> Parse.parse_prog (Yojson.Basic.from_file
                                  "../data-test/simple-var-real-wo-dec.json"))

let var_suite =
  "suite for variables" >:::
  [ "simple test for variable (boolean, false)" >::
    test_simple_var_bool_false;
    "simple test for variable (boolean, true)"  >::
    test_simple_var_bool_true;
    "simple test for variable (int, 0)"  >::
    test_simple_var_int_zero;
    "simple test for variable (int, 2)"  >::
    test_simple_var_int_pos;
    "simple test for variable (int, -5)"  >::
    test_simple_var_int_neg;
    "simple test for variable (real, 0.0)"  >::
    test_simple_var_real_zero;
    "simple test for variable (real, 21.15)"  >::
    test_simple_var_real_pos;
    "simple test for variable (real, -2.24)"  >::
    test_simple_var_real_neg;
    "simple test for variable (real, -21.15e-02)"  >::
    test_simple_var_real_e;
    "simple test for variable (real, 2500)"  >::
    test_simple_var_real_wo_dec;
  ]

let _ =
  run_test_tt_main var_suite
