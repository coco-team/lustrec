open Basetypes
open ActiveStates
open CPS_transformer

let ff = Format.fprintf 
       
module LustrePrinter (
  Vars : sig
    val state_vars : ActiveStates.Vars.t
    val global_vars : LustreSpec.var_decl list
  end) : TransformerType =
struct
  include TransformerStub

  type name_t = string
  type t_base = { statements : LustreSpec.statement list; assert_false: bool }
  type t = name_t -> name_t -> (ActiveStates.Vars.t * t_base)

	
  let new_loc, reset_loc =
    let cpt = ref 0 in
    ((fun () -> incr cpt; Format.sprintf "loc_%i" !cpt),
     (fun () -> cpt := 0))

  let new_aut, reset_aut =
    let cpt = ref 0 in
    ((fun () -> incr cpt; Format.sprintf "aut_%i" !cpt),
     (fun () -> cpt := 0))
      
  let pp_path prefix fmt path =
    Format.fprintf fmt "%s%t"
      prefix
      (fun fmt -> Utils.fprintf_list ~sep:"_" Format.pp_print_string fmt path)

  let pp_typed_path sin fmt path =
    Format.fprintf fmt "%a : bool" (pp_path sin) path

  let pp_vars sin fmt vars =
    Format.fprintf fmt "%t" (fun fmt -> Utils.fprintf_list ~sep:", " (pp_path sin) fmt (ActiveStates.Vars.elements vars))
  let pp_vars_decl sin fmt vars =
    Format.fprintf fmt "%t" (fun fmt -> Utils.fprintf_list ~sep:"; " (pp_typed_path sin) fmt (ActiveStates.Vars.elements vars))
       
  let var_to_ident prefix p =
    pp_path prefix Format.str_formatter p;
    Format.flush_str_formatter ()

  let vars_to_ident_list ?(prefix="") vars =
    List.map (
      fun p -> var_to_ident prefix p
    ) (ActiveStates.Vars.elements vars)

  let mkvar name typ = 
    let loc = Location.dummy_loc in
    Corelang.mkvar_decl
      loc
      (name, typ, Corelang.mkclock loc LustreSpec.Ckdec_any, false, None)

  let var_to_vdecl ?(prefix="") var typ = mkvar (var_to_ident prefix var) typ 
  let state_vars_to_vdecl_list ?(prefix="") vars =
    let bool_type = Corelang.mktyp Location.dummy_loc LustreSpec.Tydec_bool in
    List.map 
      (fun v -> var_to_vdecl ~prefix:prefix v bool_type)
      (ActiveStates.Vars.elements vars)

  let mk_locals locs =
    ActiveStates.Vars.fold (fun v accu ->
      (state_vars_to_vdecl_list ~prefix:(List.hd v) Vars.state_vars)@accu
    ) locs []
     (* TODO: declare global vars *)

  let mkeq = Corelang.mkeq Location.dummy_loc
  let mkexpr = Corelang.mkexpr Location.dummy_loc
  let mkpredef_call = Corelang.mkpredef_call Location.dummy_loc
  let expr_of_bool b = mkexpr (LustreSpec.Expr_const (Corelang.const_of_bool b))
  let mkstmt_eq lhs_vars ?(prefix_lhs="") rhs =
    { statements = [
      LustreSpec.Eq (
	mkeq (
	  vars_to_ident_list ~prefix:prefix_lhs lhs_vars, (* lhs *)
	  rhs (* rhs *)
	)
      )
      ];
      assert_false = false
    }
  let base_to_assert b =
    if b.assert_false then
      [{LustreSpec.assert_expr = expr_of_bool false; assert_loc = Location.dummy_loc}]
    else
      []

    
  let var_to_expr ?(prefix="") p =
    let id = var_to_ident prefix p in
    Corelang.expr_of_ident id Location.dummy_loc

  let vars_to_exprl ?(prefix="") vars =
    List.map
      (fun p -> var_to_expr ~prefix:prefix p)
      (ActiveStates.Vars.elements vars)


  (* Events *)
  let event_type_decl =
    Corelang.mktop
      (
	LustreSpec.TypeDef {
	  LustreSpec.tydef_id = "event_type";
	  tydef_desc = LustreSpec.Tydec_int
	}
      )
    
  let event_type = {
    LustreSpec.ty_dec_desc = LustreSpec.Tydec_const "event_type";
    LustreSpec.ty_dec_loc = Location.dummy_loc;
  }
    
  let event_var = mkvar "event" event_type


  let const_map : (event_base_t, int) Hashtbl.t = Hashtbl.create 13
  let get_event_const e =
    try Hashtbl.find const_map e
    with Not_found -> (
      let nb = Hashtbl.length const_map in
      Hashtbl.add const_map e nb;
      nb	
    )


      
  let null sin sout =
    let expr_list = vars_to_exprl ~prefix:sin Vars.state_vars in
    ActiveStates.Vars.empty,
    (
      (* Nothing happen here: out_vars = in_vars *)
      let rhs = mkexpr (LustreSpec.Expr_tuple expr_list) in 
      mkstmt_eq ~prefix_lhs:sout Vars.state_vars rhs
    )
      
  let bot sin sout =
    let (vars, tr) = null sin sout in
    (
      ActiveStates.Vars.empty,
      { tr with assert_false = true }
    )
      
  let ( >> ) tr1 tr2 sin sout =
    let l = new_loc () in
    let (vars1, tr1) = tr1 sin l in
    let (vars2, tr2) = tr2 l sout in
    (ActiveStates.Vars.add [l] (ActiveStates.Vars.union vars1 vars2),
     {
       statements = tr1.statements @ tr2.statements;
       assert_false = tr1.assert_false || tr2.assert_false
     }
    )
      
  let pp_name :
  type c. c call_t  -> c -> unit =
    fun call -> 
      match call with
      | Ecall -> (fun (p, p', f) ->
	Format.fprintf Format.str_formatter "theta%a%a%a_%a"
	  pp_call call
	  (pp_path "_from_") p
	  (pp_path "_to_") p'
	  pp_frontier f)
      | Dcall -> (fun p          ->
	Format.fprintf Format.str_formatter "theta%a%a"
	  pp_call call
	  (pp_path "_from_") p)
      | Xcall -> (fun (p, f)     ->
	Format.fprintf Format.str_formatter "theta%a%a_%a"
	  pp_call call
	  (pp_path "_from_") p
	  pp_frontier f)
	     

  let mkcall' :
  type c. name_t -> name_t -> c call_t -> c -> t_base =
    fun sin sout call arg ->
      pp_name call arg;
      let funname = Format.flush_str_formatter () in
      let args = (Corelang.expr_of_vdecl event_var)::(vars_to_exprl ~prefix:sin Vars.state_vars) in
      let rhs = mkpredef_call funname args in
      mkstmt_eq ~prefix_lhs:sout Vars.state_vars rhs
	
  let mkact' action sin sout =
    match action with
    | Action.Call (c, a) -> mkcall' sin sout c a
    | Action.Quote a     ->
	let funname = "action_" ^ a.ident in
	let args = vars_to_exprl ~prefix:sin Vars.state_vars in
	let rhs = mkpredef_call funname args in
	mkstmt_eq ~prefix_lhs:sout Vars.state_vars rhs
    | Action.Open p      ->
       let vars' = ActiveStates.Vars.remove p Vars.state_vars in
       (* eq1: sout_p = true *)
       let eq1 = mkeq ([var_to_ident sout p] , expr_of_bool true) in
       (* eq2: sout_xx = sin_xx *)
       let expr_list = vars_to_exprl ~prefix:sin vars' in
       let rhs = mkexpr (LustreSpec.Expr_tuple expr_list) in 
       let eq2 = mkeq (vars_to_ident_list ~prefix:sout vars', rhs) in
	 {
	   statements = [
	     LustreSpec.Eq (eq1);
	     LustreSpec.Eq (eq2);
	   ];
	   assert_false = false
	 }
	 
    | Action.Close p     ->
       let vars' = ActiveStates.Vars.remove p Vars.state_vars in
       (* eq1: sout_p = false *)
       let eq1 = mkeq ([var_to_ident sout p] , expr_of_bool false) in
       (* eq2: sout_xx = sin_xx *)
       let expr_list = vars_to_exprl ~prefix:sin vars' in
       let rhs = mkexpr (LustreSpec.Expr_tuple expr_list) in 
       let eq2 = mkeq (vars_to_ident_list ~prefix:sout vars', rhs) in
	 {
	   statements = [
	     LustreSpec.Eq (eq1);
	     LustreSpec.Eq (eq2);
	   ];
	   assert_false = false
	 }

    | Action.Nil         ->
       let expr_list = vars_to_exprl ~prefix:sin Vars.state_vars in
       let rhs = mkexpr (LustreSpec.Expr_tuple expr_list) in 
       mkstmt_eq ~prefix_lhs:sout Vars.state_vars rhs
	 
  let eval_act kenv (action : act_t) =
    (*Format.printf "----- action = %a@." Action.pp_act action;*)
    (fun sin sout -> (ActiveStates.Vars.empty,
		      mkact' action sin sout   ))
       
  let rec mkcond' sin condition =
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    match condition with
    | Condition.True               -> expr_of_bool true
    | Condition.Active p           -> var_to_expr ~prefix:sin p
    | Condition.Event e            ->
       mkpredef_call "=" [
	 Corelang.expr_of_vdecl event_var;
	 mkexpr (LustreSpec.Expr_const (LustreSpec.Const_int (get_event_const e)))
       ]
    | Condition.Neg cond           -> mkpredef_call "not" [mkcond' sin cond]
    | Condition.And (cond1, cond2) -> mkpredef_call "&&" [mkcond' sin cond1;
							  mkcond' sin cond2]
    | Condition.Quote c            -> c (* TODO: shall we prefix with sin ? *)

  let rec eval_cond condition (ok:t) ko sin sout =
    let open LustreSpec in
    let loc = Location.dummy_loc in
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    let (vars1, tr1) = ok sin sout in
    let (vars2, tr2) = ko sin sout in
    let (vars0, tr0) = bot sin sout in
    let aut = new_aut () in
    (ActiveStates.Vars.empty,
     {
       statements = [
	 let handler_default_mode =          	      (* Default mode : CenterPoint *)
	   let handler_default_mode_unless = [
	     (loc, mkcond' sin condition, true (* restart *), "Cond_" ^ aut);
	     (loc, mkcond' sin (Condition.Neg condition), true (* restart *), "NotCond_" ^ aut);
	   ]
	   in
	   Automata.mkhandler
	     loc                                      (* location *)
	     ("CenterPoint_" ^ aut)                   (* state name *)
	     handler_default_mode_unless              (* unless *)
	     []                                       (* until *)
	     []                                       (* locals *)
	     (tr0.statements, base_to_assert tr0, []) (* stmts, asserts, annots *)
	 in
	 let handler_cond_mode =                       	 (* Cond mode *)
       	   let handler_cond_mode_until = [
	     (loc, expr_of_bool true, true (* restart *), "CenterPoint_" ^ aut);
       	   ]
	   in
	   Automata.mkhandler
	     loc                                      (* location *)
	     ("Cond_" ^ aut)                          (* state name *)
	     []                                       (* unless *)
	     handler_cond_mode_until                  (* until *)
	     (mk_locals vars1)                        (* locals *)
	     (tr1.statements, base_to_assert tr1, []) (* stmts, asserts, annots *)
	 in
	 let handler_notcond_mode =                       	 (* NotCond mode *)
       	   let handler_notcond_mode_until = [
	     (loc, expr_of_bool true, true (* restart *), "CenterPoint_" ^ aut);
       	   ]
	   in
	   Automata.mkhandler
	     loc                                      (* location *)
	     ("NotCond_" ^ aut)                       (* state name *)
	     []                                       (* unless *)
	     handler_notcond_mode_until               (* until *)
	     (mk_locals vars2)                        (* locals *)
	     (tr2.statements, base_to_assert tr2, []) (* stmts, asserts, annots *)
	 in
	 let handlers = [ handler_default_mode; handler_cond_mode; handler_notcond_mode ] in
	 Aut (Automata.mkautomata loc aut handlers)
       ];
       assert_false = false
     }
    )
      
  let mktransformer tr =
    let (vars, tr) = tr "sin_" "sout_"
    in tr 
    
  let mkcomponent :
  type c. c call_t -> c -> t -> LustreSpec.program =
    fun call args ->
      fun tr ->
	reset_loc ();
	let (vars', tr') = tr "sin_" "sout_" in
	pp_name call args;
	let funname = Format.flush_str_formatter () in
	let node =
	  Corelang.mktop (	
	    LustreSpec.Node {LustreSpec.node_id = funname;
			   node_type = Types.new_var ();
			   node_clock = Clocks.new_var true;
			   node_inputs = event_var :: state_vars_to_vdecl_list ~prefix:"sin_" Vars.state_vars;
			   node_outputs = state_vars_to_vdecl_list ~prefix:"sout_" Vars.state_vars;
			   node_locals = mk_locals vars'; (* TODO: add global vars *)
			   node_gencalls = [];
			   node_checks = [];
			   node_stmts = tr'.statements;
			   node_asserts = base_to_assert tr';
			   node_dec_stateless = false;
			   node_stateless = None;
			   node_spec = None;
			   node_annot = []}
      )  
	in
	[node]
	  
  let mk_main_loop () =
    (* let loc = Location.dummy_loc in *)
    
    let call_stmt =
      (* (%t) -> pre (thetaCallD_from_principal (event, %a)) *)
      let init = mkexpr
	(LustreSpec.Expr_tuple (List.map (fun _ -> expr_of_bool false) (ActiveStates.Vars.elements Vars.state_vars)))
      in
      let args = (Corelang.expr_of_vdecl event_var)::
	(vars_to_exprl ~prefix:"sout_" Vars.state_vars)
      in
      let call_expr = mkpredef_call "thetaCallD_from_principal" args in
      let pre_call_expr = mkexpr (LustreSpec.Expr_pre (call_expr)) in
      let rhs = mkexpr (LustreSpec.Expr_arrow (init, pre_call_expr)) in
      mkstmt_eq Vars.state_vars ~prefix_lhs:"sout_" rhs
    in
    let node_principal =
      Corelang.mktop (	
	LustreSpec.Node {LustreSpec.node_id = "principal_loop";
			 node_type = Types.new_var ();
			 node_clock = Clocks.new_var true;
			 node_inputs = List.map Corelang.copy_var_decl [event_var];
			 node_outputs = state_vars_to_vdecl_list ~prefix:"sout_" Vars.state_vars;
			 node_locals = []; (* TODO: add global vars *)
			 node_gencalls = [];
			 node_checks = [];
			 node_asserts = base_to_assert call_stmt; 
			 node_stmts = call_stmt.statements;
			 node_dec_stateless = false;
			 node_stateless = None;
			 node_spec = None;
			 node_annot = []}
      )  
    in
      node_principal
    

  let mkprincipal tr =
    event_type_decl :: mkcomponent Dcall ["principal"] tr @ [mk_main_loop ()]

end
