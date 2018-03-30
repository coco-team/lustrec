open Datatype
open Basetypes
(* open Transformer2 *)
open SF

let verbose = false
let actionv x = no_action (*TODO if verbose then action x else no_action*)
let action x = no_action (* TODO *)
let condition x = condition {
  expr = Corelang.mkexpr Location.dummy_loc (Lustre_types.Expr_const (Corelang.const_of_bool true));
  cinputs = [];
  coutputs = [];
  cvariables = [];
}
let name = "stopwatch"

let model =
  let smain    = "main" in
  let sstop    = "stop" in
  let sreset   = "reset" in
  let slapstop = "lap_stop" in
  let srun     = "run" in
  let srunning = "running" in
  let slap     = "lap" in

  let tinitstop = {
    event = no_event;
    condition = no_condition;
    condition_act  = actionv "ac_cond_init_stop";
    transition_act = actionv "ac_trans_init_stop";
    dest = DPath [smain;sstop];
  }
  in
  let tinitreset = {
    event = no_event;
    condition = no_condition;
    condition_act  = actionv "ac_cond_init_reset";
    transition_act = actionv "ac_cond_init_stop";
    dest = DPath [smain;sstop;sreset];
  }
  in
  let treset = {
    event = event "LAP";
    condition = no_condition;
    condition_act  = action "reset counter";
    transition_act = actionv "ac_trans_reset_junction";
    dest = DJunction "jreset" (* [smain;sstop;sreset]; ou bien mettre une junction.  Verifier
				 si il y a des effets de bords non
				 desirés de fermeture/ouverture de
				 noeud *)
  }
  in
  let treset_start  = {
    event = event "START";
    condition = no_condition;
    condition_act  = actionv "ac_cond_reset->running";
    transition_act = actionv "ac_trans_reset->running";
    dest = DPath [smain;srun;srunning];
  }
  in

  let tlapstop_lap  = {
    event = event "LAP";
    condition = no_condition;
    condition_act  = actionv "ac_cond_lap_stop->reset";
    transition_act = actionv "ac_trans_lap_stop->reset";
    dest = DPath [smain;sstop;sreset];
  }
  in

  let tlapstop_start  = {
    event = event "START";
    condition = no_condition;
    condition_act  = actionv "ac_cond_lap_stop->lap";
    transition_act = actionv "ac_trans_lap_stop->lap";
    dest = DPath [smain;srun;slap];
  }
  in
  let ttic   = {
    event = event "TIC";
    condition = no_condition;
    condition_act  = action "cent+=1";
    transition_act = actionv "ac_trans_->J1";
    dest = DJunction "j1";
  }
  in
  let trunning_start  = {
    event = event "START";
    condition = no_condition;
    condition_act  = actionv "ac_cond_running->reset";
    transition_act = actionv "ac_trans_running->reset";
    dest = DPath [smain;sstop;sreset];
  }
  in
  let tlap_start  = {
    event = event "START";
    condition = no_condition;
    condition_act  = actionv "ac_cond_lap->lap_stop";
    transition_act = actionv "ac_trans_lap->lap_stop";
    dest = DPath [smain;sstop;slapstop];
  }
  in
  let tlap_lap  = {
    event = event "LAP";
    condition = no_condition;
    condition_act  = actionv "ac_cond_lap->running";
    transition_act = actionv "ac_trans_lap->running";
    dest = DPath [smain;srun;srunning];
  }
  in
  let trunning_lap  = {
    event = event "LAP";
    condition = no_condition;
    condition_act  = actionv "ac_cond_running->lap";
    transition_act = actionv "ac_trans_running->lap";
    dest = DPath [smain;srun;slap];
  }
  in
  let tj1j2 = {
    event = no_event;
    condition = condition "cent==100";
    condition_act  = action "cont=0;sec+=1";
    transition_act = actionv "ac_trans_J1->J2";
    dest = DJunction "j2";
  }
  in
  let tj1j3 =    {
    event = no_event;
    condition = condition "cent!=100";
    condition_act  = actionv "ac_cond_J1->J3";
    transition_act = actionv "ac_trans_J1->J3";
    dest = DJunction "j3";
  }
  in
  let tj2j3gauche   = {
    event = no_event;
    condition = condition "sec!=60";
    condition_act  = actionv "ac_cond_J2->J3_left";
    transition_act = actionv "ac_trans_J2->J3_left";
    dest = DJunction "j3";
  }
  in
  let tj2j3droite    = {
    event = no_event;
    condition = condition "sec==60";
    condition_act  = action "sec=0; min+=1";
    transition_act = actionv "ac_trans_J2->J3_right";
    dest = (*DPath [smain;srun];*) DJunction "j3";
  }
  in
  let def_main = {
    state_actions = {
      entry_act  = actionv "ac_main_entry";
      during_act = actionv "ac_main_during";
      exit_act   = actionv "ac_main_exit";
    };
    outer_trans = [];
    inner_trans = [];
    internal_composition = Or ([tinitstop], [sstop; srun])
  }
  in

  let def_stop = {
    state_actions = {
      entry_act  = actionv "ac_stop_entry";
      during_act = actionv "ac_stop_during";
      exit_act   = actionv "ac_stop_exit";
    };
    outer_trans = [];
    inner_trans = [];
    internal_composition = Or ([tinitreset], [sreset; slapstop])
  }
  in

  let def_reset = {
    state_actions = {
      entry_act  = actionv "ac_reset_entry";
      during_act = actionv "ac_reset_during";
      exit_act   = actionv "ac_reset_exit";
    };
    outer_trans = [treset_start];
    inner_trans = [treset];
    internal_composition = Or ([treset_start], [])
  }
  in

  let def_lapstop = {
    state_actions = {
      entry_act  = actionv "ac_lapstop_entry";
      during_act = actionv "ac_lapstop_during";
      exit_act   = actionv "ac_lapstop_exit";
    };
    outer_trans = [tlapstop_lap; tlapstop_start];
    inner_trans = [];
    internal_composition = Or ([], [])
  }
  in

  let def_run = {
    state_actions = {
      entry_act  = actionv "ac_run_entry";
      during_act = actionv "ac_run_during";
      exit_act   = actionv "ac_run_exit";
    };
    outer_trans = [];
    inner_trans = [ttic];
    internal_composition = Or ([], [srunning; slap])
  }
  in

  let def_running = {
    state_actions = {
      entry_act  = actionv "ac_running_entry";
      during_act = action "disp=(cent,sec,min)";
      exit_act   = actionv "ac_running_exit";
    };
    outer_trans = [trunning_start; trunning_lap];
    inner_trans = [];
    internal_composition = Or ([], [])
  }
  in

  let def_lap = {
    state_actions = {
      entry_act  = actionv "ac_lap_entry";
      during_act = actionv "ac_lap_during";
      exit_act   = actionv "ac_lap_exit";
    };
    outer_trans = [tlap_start; tlap_lap];
    inner_trans = [];
    internal_composition = Or ([], [])
  }
  in

  let src = [
    State([smain;srun;srunning], def_running);
    State([smain;srun;slap], def_lap);
    State([smain;srun], def_run);
    State([smain;sstop;sreset], def_reset);
    State([smain;sstop;slapstop], def_lapstop);
    State([smain;sstop], def_stop);
    State([smain], def_main);
    Junction("jreset", []);
    Junction("j1", [tj1j2;tj1j3]);
    Junction("j2", [tj2j3droite; tj2j3gauche]);
    Junction("j3", []);
  ]
  in
  let globals =
    let int_typ = Corelang.mktyp Location.dummy_loc Lustre_types.Tydec_int in
    List.map (fun k ->
      Corelang.mkvar_decl
	Location.dummy_loc
	(k, (* name *)
	 int_typ, (* type *)
	 Corelang.dummy_clock_dec, (* clock *)
	 false, (* not a constant *)
	 None, (* no default value *)
	 None (* no parent known *)
	),
      (* Default value is zero *)
      Corelang.mkexpr Location.dummy_loc (Lustre_types.Expr_const (Lustre_types.Const_int 0))
      
    )
      ["cent";
       "sec";
       "min";
       "cont"
      ]
  in
  Program (smain, src, globals)

let traces : trace_t list =
  [
    [None; Some "TIC"; Some "START"; Some "TIC"; Some "TIC"];
    [None; Some "START"; Some "START"; Some "START"];
    [None; Some "START"; Some "TIC"; Some "START"; Some "TIC"]
  ]
