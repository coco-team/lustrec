open Datatype
open Basetypes
open SF

let name = "simple"

let condition x = condition {
  expr = Corelang.mkexpr Location.dummy_loc (Lustre_types.Expr_const (Corelang.const_of_bool true));
  cinputs = [];
  coutputs = [];
  cvariables = [];
}
  
  let action _ = no_action

let model : prog_t =
    let state_main = "main" in
    let state_a = "a" in
    let state_a1 = "a1" in
    let state_b = "b" in

    let actions_main = state_action (action "emain") (action "dmain") (action "xmain") in
    let actions_a = state_action (action "eA") (action "dA") (action "xA") in
    let actions_a1 = state_action (action "eA1") (action "dA1") (action "xA1") in
    let actions_b = state_action (action "eB") (action "dB") (action "xB") in

    let tA = {
      event = no_event;
      condition = condition "cond_tA";
      condition_act = action "condact_tA";
      transition_act = action "transact_tA";
      dest = DPath [state_main;state_a];
    }
    in
    let tB = {
      event = no_event;
      condition = condition "cond_tB";
      condition_act = action "condact_tB";
      transition_act = action "transact_tB";
      dest = DPath [state_main;state_b];
    }
    in
    let tA1 = {
      event = no_event;
      condition = condition "cond_tA1";
      condition_act = action "condact_tA1";
      transition_act = action "transact_tA1";
      dest = DPath [state_main;state_a;state_a1];
    }
    in


    let def_a = {
      state_actions = actions_a;
      outer_trans = [tB];
      inner_trans = [];
      internal_composition = Or ([tA1], [state_a1])
    }
    in
    let def_a1 = {
      state_actions = actions_a1;
      outer_trans = [tB];
      inner_trans = [];
      internal_composition = Or ([], [])
    }
    in
    let def_b = {
      state_actions = actions_b;
      outer_trans = [tA1];
      inner_trans = [];
      internal_composition = Or ([], [])
    }
    in
    let def_main = {
      state_actions = actions_main;
      outer_trans = [];
      inner_trans = [];
      internal_composition = Or ([tA], [state_a; state_b])
    }
    in
    let src = [State([state_main;state_a], def_a);
	       State([state_main;state_a;state_a1], def_a1);
	       State([state_main;state_b], def_b);
	       State([state_main], def_main);
	      ]
    in
    Program (state_main, src, [])

let traces : trace_t list = [[None; None]]
