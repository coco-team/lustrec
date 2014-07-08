
(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2013, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 * Copyright (C) 2012-2013, INPT, Toulouse, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

(* This module is used for the lustre to C compiler *)

open LustreSpec
open Corelang
open Clocks
open Causality

exception NormalizationError

module OrdVarDecl:Map.OrderedType with type t=var_decl =
  struct type t = var_decl;; let compare = compare end

module ISet = Set.Make(OrdVarDecl)

type value_t = 
  | Cst of constant
  | LocalVar of var_decl
  | StateVar of var_decl
  | Fun of ident * value_t list 
  | Array of value_t list
  | Access of value_t * value_t
  | Power of value_t * value_t

type instr_t =
  | MLocalAssign of var_decl * value_t
  | MStateAssign of var_decl * value_t
  | MReset of ident
  | MStep of var_decl list * ident * value_t list
  | MBranch of value_t * (label * instr_t list) list
 
let rec pp_val fmt v =
  match v with
    | Cst c         -> Printers.pp_const fmt c 
    | LocalVar v    -> Format.pp_print_string fmt v.var_id
    | StateVar v    -> Format.pp_print_string fmt v.var_id
    | Array vl      -> Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:", " pp_val)  vl
    | Access (t, i) -> Format.fprintf fmt "%a[%a]" pp_val t pp_val i
    | Power (v, n)  -> Format.fprintf fmt "(%a^%a)" pp_val v pp_val n
    | Fun (n, vl)   -> Format.fprintf fmt "%s (%a)" n (Utils.fprintf_list ~sep:", " pp_val)  vl

let rec pp_instr fmt i =
  match i with 
    | MLocalAssign (i,v) -> Format.fprintf fmt "%s<-l- %a" i.var_id pp_val v
    | MStateAssign (i,v) -> Format.fprintf fmt "%s<-s- %a" i.var_id pp_val v
    | MReset i           -> Format.fprintf fmt "reset %s" i
    | MStep (il, i, vl)  ->
      Format.fprintf fmt "%a = %s (%a)"
	(Utils.fprintf_list ~sep:", " (fun fmt v -> Format.pp_print_string fmt v.var_id)) il
	i      
	(Utils.fprintf_list ~sep:", " pp_val) vl
    | MBranch (g,hl)     ->
      Format.fprintf fmt "@[<v 2>case(%a) {@,%a@,}@]"
	pp_val g
	(Utils.fprintf_list ~sep:"@," pp_branch) hl

and pp_branch fmt (t, h) =
  Format.fprintf fmt "@[<v 2>%s:@,%a@]" t (Utils.fprintf_list ~sep:"@," pp_instr) h

type step_t = {
  step_checks: (Location.t * value_t) list;
  step_inputs: var_decl list;
  step_outputs: var_decl list;
  step_locals: var_decl list;
  step_instrs: instr_t list;
  step_asserts: value_t list;
}

type static_call = top_decl * (Dimension.dim_expr list)

type machine_t = {
  mname: node_desc;
  mmemory: var_decl list;
  mcalls: (ident * static_call) list; (* map from stateful/stateless instance to node, no internals *)
  minstances: (ident * static_call) list; (* sub-map of mcalls, from stateful instance to node *)
  minit: instr_t list;
  mstatic: var_decl list; (* static inputs only *)
  mstep: step_t;
  mspec: node_annot option;
  mannot: expr_annot list;
}

let pp_step fmt s =
  Format.fprintf fmt "@[<v>inputs : %a@ outputs: %a@ locals : %a@ checks : %a@ instrs : @[%a@]@ asserts : @[%a@]@]@ "
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_inputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_outputs
    (Utils.fprintf_list ~sep:", " Printers.pp_var) s.step_locals
    (Utils.fprintf_list ~sep:", " (fun fmt (_, c) -> pp_val fmt c)) s.step_checks
    (Utils.fprintf_list ~sep:"@ " pp_instr) s.step_instrs
    (Utils.fprintf_list ~sep:", " pp_val) s.step_asserts


let pp_static_call fmt (node, args) =
 Format.fprintf fmt "%s<%a>"
   (node_name node)
   (Utils.fprintf_list ~sep:", " Dimension.pp_dimension) args

let pp_machine fmt m =
  Format.fprintf fmt 
    "@[<v 2>machine %s@ mem      : %a@ instances: %a@ init     : %a@ step     :@   @[<v 2>%a@]@ @  spec : @[%t@]@  annot : @[%a@]@]@ "
    m.mname.node_id
    (Utils.fprintf_list ~sep:", " Printers.pp_var) m.mmemory
    (Utils.fprintf_list ~sep:", " (fun fmt (o1, o2) -> Format.fprintf fmt "(%s, %a)" o1 pp_static_call o2)) m.minstances
    (Utils.fprintf_list ~sep:"@ " pp_instr) m.minit
    pp_step m.mstep
    (fun fmt -> match m.mspec with | None -> () | Some spec -> Printers.pp_spec fmt spec)
    (Utils.fprintf_list ~sep:"@ " Printers.pp_expr_annot) m.mannot

(* Returns the declared stateless status and the computed one. *)
let get_stateless_status m =
 (m.mname.node_dec_stateless, Utils.desome m.mname.node_stateless)

let is_input m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_inputs

let is_output m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mstep.step_outputs

let is_memory m id =
  List.exists (fun o -> o.var_id = id.var_id) m.mmemory

let conditional c t e =
  MBranch(c, [ (tag_true, t); (tag_false, e) ])

let dummy_var_decl name typ =
  {
    var_id = name;
    var_dec_type = dummy_type_dec;
    var_dec_clock = dummy_clock_dec;
    var_dec_const = false;
    var_type =  typ;
    var_clock = Clocks.new_ck (Clocks.Cvar Clocks.CSet_all) true;
    var_loc = Location.dummy_loc
  }

let arrow_id = "_arrow"

let arrow_typ = Types.new_ty Types.Tunivar

let arrow_desc =
  {
    node_id = arrow_id;
    node_type = Type_predef.type_bin_poly_op;
    node_clock = Clock_predef.ck_bin_univ;
    node_inputs= [dummy_var_decl "_in1" arrow_typ; dummy_var_decl "_in2" arrow_typ];
    node_outputs= [dummy_var_decl "_out" arrow_typ];
    node_locals= [];
    node_gencalls = [];
    node_checks = [];
    node_asserts = [];
    node_eqs= [];
    node_dec_stateless = false;
    node_stateless = Some false;
    node_spec = None;
    node_annot = [];  }

let arrow_top_decl =
  {
    top_decl_desc = Node arrow_desc;
    top_decl_loc = Location.dummy_loc
  }

let arrow_machine =
  let state = "_first" in
  let var_state = dummy_var_decl state (Types.new_ty Types.Tbool) in
  let var_input1 = List.nth arrow_desc.node_inputs 0 in
  let var_input2 = List.nth arrow_desc.node_inputs 1 in
  let var_output = List.nth arrow_desc.node_outputs 0 in
  {
    mname = arrow_desc;
    mmemory = [var_state];
    mcalls = [];
    minstances = [];
    minit = [MStateAssign(var_state, Cst (const_of_bool true))];
    mstatic = [];
    mstep = {
      step_inputs = arrow_desc.node_inputs;
      step_outputs = arrow_desc.node_outputs;
      step_locals = [];
      step_checks = [];
      step_instrs = [conditional (StateVar var_state)
			         [MStateAssign(var_state, Cst (const_of_bool false));
                                  MLocalAssign(var_output, LocalVar var_input1)]
                                 [MLocalAssign(var_output, LocalVar var_input2)] ];
      step_asserts = [];
    };
    mspec = None;
    mannot = [];
  }

let new_instance =
  let cpt = ref (-1) in
  fun caller callee tag ->
    begin
      let o =
	if Stateless.check_node callee then
	  node_name callee
	else
	  Printf.sprintf "ni_%d" (incr cpt; !cpt) in
      let o =
	if !Options.ansi && is_generic_node callee
	then Printf.sprintf "%s_inst_%d" o (Utils.position (fun e -> e.expr_tag = tag) caller.node_gencalls)
	else o in
      o
    end

(* translate_<foo> : node -> context -> <foo> -> machine code/expression *)
(* the context contains  m : state aka memory variables  *)
(*                      si : initialization instructions *)
(*                       j : node aka machine instances  *)
(*                       d : local variables             *)
(*                       s : step instructions           *)
let translate_ident node (m, si, j, d, s) id =
  try (* id is a node var *)
    let var_id = get_node_var id node in
    if ISet.exists (fun v -> v.var_id = id) m
    then StateVar var_id
    else LocalVar var_id
  with Not_found -> (* id is a constant *)
    LocalVar (Corelang.var_decl_of_const (Hashtbl.find Corelang.consts_table id))

let rec control_on_clock node ((m, si, j, d, s) as args) ck inst =
 match (Clocks.repr ck).cdesc with
 | Con    (ck1, cr, l) ->
   let id  = Clocks.const_of_carrier cr in
   control_on_clock node args ck1 (MBranch (translate_ident node args id,
					    [l, [inst]] ))
 | _                   -> inst

let rec join_branches hl1 hl2 =
 match hl1, hl2 with
 | []          , _            -> hl2
 | _           , []           -> hl1
 | (t1, h1)::q1, (t2, h2)::q2 ->
   if t1 < t2 then (t1, h1) :: join_branches q1 hl2 else
   if t1 > t2 then (t2, h2) :: join_branches hl1 q2
   else (t1, List.fold_right join_guards h1 h2) :: join_branches q1 q2

and join_guards inst1 insts2 =
 match inst1, insts2 with
 | _                   , []                               ->
   [inst1]
 | MBranch (x1, hl1), MBranch (x2, hl2) :: q when x1 = x2 ->
   MBranch (x1, join_branches (sort_handlers hl1) (sort_handlers hl2))
   :: q
 | _ -> inst1 :: insts2

let join_guards_list insts =
 List.fold_right join_guards insts []

(* specialize predefined (polymorphic) operators
   wrt their instances, so that the C semantics 
   is preserved *)
let specialize_to_c expr =
 match expr.expr_desc with
 | Expr_appl (id, e, r) ->
   if List.exists (fun e -> Types.is_bool_type e.expr_type) (expr_list_of_expr e)
   then let id =
	  match id with
	  | "="  -> "equi"
	  | "!=" -> "xor"
	  | _    -> id in
	{ expr with expr_desc = Expr_appl (id, e, r) }
   else expr
 | _ -> expr

let specialize_op expr =
  match !Options.output with
  | "C" -> specialize_to_c expr
  | _   -> expr

let rec translate_expr node ((m, si, j, d, s) as args) expr =
 let expr = specialize_op expr in
 match expr.expr_desc with
 | Expr_const v                     -> Cst v
 | Expr_ident x                     -> translate_ident node args x
 | Expr_array el                    -> Array (List.map (translate_expr node args) el)
 | Expr_access (t, i)               -> Access (translate_expr node args t, translate_expr node args (expr_of_dimension i))
 | Expr_power  (e, n)               -> Power  (translate_expr node args e, translate_expr node args (expr_of_dimension n))
 | Expr_tuple _
 | Expr_arrow _ 
 | Expr_fby _
 | Expr_pre _                       -> (Printers.pp_expr Format.err_formatter expr; Format.pp_print_flush Format.err_formatter (); raise NormalizationError)
 | Expr_when    (e1, _, _)          -> translate_expr node args e1
 | Expr_merge   (x, _)              -> raise NormalizationError
 | Expr_appl (id, e, _) when Basic_library.is_internal_fun id ->
   let nd = node_from_name id in
   Fun (node_name nd, List.map (translate_expr node args) (expr_list_of_expr e))
 | Expr_ite (g,t,e) -> (
   (* special treatment depending on the active backend. For horn backend, ite
      are preserved in expression. While they are removed for C or Java
      backends. *)
   match !Options.output with | "horn" -> 
     Fun ("ite", [translate_expr node args g; translate_expr node args t; translate_expr node args e])
   | "C" | "java" | _ -> 
     (Printers.pp_expr Format.err_formatter expr; Format.pp_print_flush Format.err_formatter (); raise NormalizationError)
 )
 | _                   -> raise NormalizationError

let translate_guard node args expr =
  match expr.expr_desc with
  | Expr_ident x  -> translate_ident node args x
  | _ -> (Format.eprintf "internal error: translate_guard %s %a@." node.node_id Printers.pp_expr expr;assert false)

let rec translate_act node ((m, si, j, d, s) as args) (y, expr) =
  match expr.expr_desc with
  | Expr_ite   (c, t, e) -> let g = translate_guard node args c in
			    conditional g [translate_act node args (y, t)]
                              [translate_act node args (y, e)]
  | Expr_merge (x, hl)   -> MBranch (translate_ident node args x, List.map (fun (t,  h) -> t, [translate_act node args (y, h)]) hl)
  | _                    -> MLocalAssign (y, translate_expr node args expr)

let reset_instance node args i r c =
  match r with
  | None        -> []
  | Some (x, l) -> [control_on_clock node args c (MBranch (translate_ident node args x, [l, [MReset i]]))]

let translate_eq node ((m, si, j, d, s) as args) eq =
  (*Format.eprintf "translate_eq %a with clock %a@." Printers.pp_node_eq eq Clocks.print_ck eq.eq_rhs.expr_clock;*)
  match eq.eq_lhs, eq.eq_rhs.expr_desc with
  | [x], Expr_arrow (e1, e2)                     ->
    let var_x = get_node_var x node in
    let o = new_instance node arrow_top_decl eq.eq_rhs.expr_tag in
    let c1 = translate_expr node args e1 in
    let c2 = translate_expr node args e2 in
    (m,
     MReset o :: si,
     Utils.IMap.add o (arrow_top_decl, []) j,
     d,
     (control_on_clock node args eq.eq_rhs.expr_clock (MStep ([var_x], o, [c1;c2]))) :: s)
  | [x], Expr_pre e1 when ISet.mem (get_node_var x node) d     ->
    let var_x = get_node_var x node in
    (ISet.add var_x m,
     si,
     j,
     d,
     control_on_clock node args eq.eq_rhs.expr_clock (MStateAssign (var_x, translate_expr node args e1)) :: s)
  | [x], Expr_fby (e1, e2) when ISet.mem (get_node_var x node) d ->
    let var_x = get_node_var x node in
    (ISet.add var_x m,
     MStateAssign (var_x, translate_expr node args e1) :: si,
     j,
     d,
     control_on_clock node args eq.eq_rhs.expr_clock (MStateAssign (var_x, translate_expr node args e2)) :: s)

  | p  , Expr_appl (f, arg, r) when not (Basic_library.is_internal_fun f) ->
    let var_p = List.map (fun v -> get_node_var v node) p in
    let el = expr_list_of_expr arg in
    let vl = List.map (translate_expr node args) el in
    let node_f = node_from_name f in
    let call_f =
      node_f,
      NodeDep.filter_static_inputs (node_inputs node_f) el in 
    let o = new_instance node node_f eq.eq_rhs.expr_tag in
    let call_ck = Clocks.new_var true in
    Clock_calculus.unify_imported_clock (Some call_ck) eq.eq_rhs.expr_clock;
    (m,
     (if Stateless.check_node node_f then si else MReset o :: si),
     Utils.IMap.add o call_f j,
     d,
     reset_instance node args o r eq.eq_rhs.expr_clock @
       (control_on_clock node args call_ck (MStep (var_p, o, vl))) :: s)

   (* special treatment depending on the active backend. For horn backend, x = ite (g,t,e)
      are preserved. While they are replaced as if g then x = t else x = e in  C or Java
      backends. *)
  | [x], Expr_ite   (c, t, e) 
    when (match !Options.output with | "horn" -> true | "C" | "java" | _ -> false)
      -> 
    let var_x = get_node_var x node in
    (m, 
     si, 
     j, 
     d, 
     (control_on_clock node args eq.eq_rhs.expr_clock 
	(MLocalAssign (var_x, translate_expr node args eq.eq_rhs))::s)
    )
      
  | [x], _                                       -> (
    let var_x = get_node_var x node in
    (m, si, j, d, 
     control_on_clock 
       node
       args
       eq.eq_rhs.expr_clock
       (translate_act node args (var_x, eq.eq_rhs)) :: s
    )
  )
  | _                                            ->
    begin
      Format.eprintf "unsupported equation: %a@?" Printers.pp_node_eq eq;
      assert false
    end

let find_eq xl eqs =
  let rec aux accu eqs =
      match eqs with
	| [] ->
	  begin
	    Format.eprintf "Looking for variables %a in the following equations@.%a@."
	      (Utils.fprintf_list ~sep:" , " (fun fmt v -> Format.fprintf fmt "%s" v)) xl
	      Printers.pp_node_eqs eqs;
	    assert false
	  end
	| hd::tl -> 
	  if List.exists (fun x -> List.mem x hd.eq_lhs) xl then hd, accu@tl else aux (hd::accu) tl
    in
    aux [] eqs

(* Sort the set of equations of node [nd] according 
   to the computed schedule [sch]
*)
let sort_equations_from_schedule nd sch =
  let split_eqs = Splitting.tuple_split_eq_list nd.node_eqs in
  let eqs_rev, remainder =
    List.fold_left 
      (fun (accu, node_eqs_remainder) vl -> 
       if List.exists (fun eq -> List.exists (fun v -> List.mem v eq.eq_lhs) vl) accu
       then
	 (accu, node_eqs_remainder)
       else
	 let eq_v, remainder = find_eq vl node_eqs_remainder in
	 eq_v::accu, remainder
      ) 
      ([], split_eqs) 
      sch 
  in
  if List.length remainder > 0 then (
    Format.eprintf "Equations not used are@.%a@.Full equation set is:@.%a@.@?"
		   Printers.pp_node_eqs remainder
      		   Printers.pp_node_eqs nd.node_eqs;
    assert false);
  List.rev eqs_rev

let translate_eqs node args eqs =
  List.fold_right (fun eq args -> translate_eq node args eq) eqs args;;

let translate_decl nd sch =
  (*Log.report ~level:1 (fun fmt -> Printers.pp_node fmt nd);*)

(*
  let eqs_rev, remainder = 
    List.fold_left 
      (fun (accu, node_eqs_remainder) v -> 
	  if List.exists (fun eq -> List.mem v eq.eq_lhs) accu
	  then
	    (accu, node_eqs_remainder)
	  else
	    (*if   List.exists (fun vdecl -> vdecl.var_id = v) nd.node_locals
	      || List.exists (fun vdecl -> vdecl.var_id = v) nd.node_outputs
	    then*)
	      let eq_v, remainder = find_eq v node_eqs_remainder in
	      eq_v::accu, remainder
	    (* else it is a constant value, checked during typing phase
	    else	 
	      accu, node_eqs_remainder *)
      ) 
      ([], split_eqs) 
      sch 
  in
 *)
  let sorted_eqs = sort_equations_from_schedule nd sch in

  let init_args = ISet.empty, [], Utils.IMap.empty, List.fold_right (fun l -> ISet.add l) nd.node_locals ISet.empty, [] in
  (* memories, init instructions, node calls, local variables (including memories), step instrs *)
  let m, init, j, locals, s = translate_eqs nd init_args sorted_eqs in
  let mmap = Utils.IMap.fold (fun i n res -> (i, n)::res) j [] in
  {
    mname = nd;
    mmemory = ISet.elements m;
    mcalls = mmap;
    minstances = List.filter (fun (_, (n,_)) -> not (Stateless.check_node n)) mmap;
    minit = init;
    mstatic = List.filter (fun v -> v.var_dec_const) nd.node_inputs;
    mstep = {
      step_inputs = nd.node_inputs;
      step_outputs = nd.node_outputs;
      step_locals = ISet.elements (ISet.diff locals m);
      step_checks = List.map (fun d -> d.Dimension.dim_loc, translate_expr nd init_args (expr_of_dimension d)) nd.node_checks;
      step_instrs = (
	(* special treatment depending on the active backend. For horn backend,
	   common branches are not merged while they are in C or Java
	   backends. *)
	match !Options.output with
	| "horn" -> s
	| "C" | "java" | _ -> join_guards_list s
      );
      step_asserts = 
	let exprl = List.map (fun assert_ -> assert_.assert_expr ) nd.node_asserts in
	List.map (translate_expr nd init_args) exprl
	;
    };
    mspec = nd.node_spec;
    mannot = nd.node_annot;
  }

(** takes the global delcarations and the scheduling associated to each node *)
let translate_prog decls node_schs =
  let nodes = get_nodes decls in 
  List.map 
    (fun node -> 
      let sch = (Utils.IMap.find node.node_id node_schs).Scheduling.schedule in
      translate_decl node sch 
    ) nodes

let get_machine_opt name machines =  
  List.fold_left 
    (fun res m -> 
      match res with 
      | Some _ -> res 
      | None -> if m.mname.node_id = name then Some m else None)
    None machines
    
(* variable substitution for optimizing purposes *)

(* checks whether an [instr] is skip and can be removed from program *)
let rec instr_is_skip instr =
  match instr with
  | MLocalAssign (i, LocalVar v) when i = v -> true
  | MStateAssign (i, StateVar v) when i = v -> true
  | MBranch (g, hl) -> List.for_all (fun (_, il) -> instrs_are_skip il) hl
  | _               -> false
and instrs_are_skip instrs =
  List.for_all instr_is_skip instrs

let instr_cons instr cont =
 if instr_is_skip instr then cont else instr::cont

let rec instr_remove_skip instr cont =
  match instr with
  | MLocalAssign (i, LocalVar v) when i = v -> cont
  | MStateAssign (i, StateVar v) when i = v -> cont
  | MBranch (g, hl) -> MBranch (g, List.map (fun (h, il) -> (h, instrs_remove_skip il [])) hl) :: cont
  | _               -> instr::cont

and instrs_remove_skip instrs cont =
  List.fold_right instr_remove_skip instrs cont

let rec value_replace_var fvar value =
  match value with
  | Cst c -> value
  | LocalVar v -> LocalVar (fvar v)
  | StateVar v -> value
  | Fun (id, args) -> Fun (id, List.map (value_replace_var fvar) args) 
  | Array vl -> Array (List.map (value_replace_var fvar) vl)
  | Access (t, i) -> Access(value_replace_var fvar t, i)
  | Power (v, n) -> Power(value_replace_var fvar v, n)

let rec instr_replace_var fvar instr cont =
  match instr with
  | MLocalAssign (i, v) -> instr_cons (MLocalAssign (fvar i, value_replace_var fvar v)) cont
  | MStateAssign (i, v) -> instr_cons (MStateAssign (i, value_replace_var fvar v)) cont
  | MReset i            -> instr_cons instr cont
  | MStep (il, i, vl)   -> instr_cons (MStep (List.map fvar il, i, List.map (value_replace_var fvar) vl)) cont
  | MBranch (g, hl)     -> instr_cons (MBranch (value_replace_var fvar g, List.map (fun (h, il) -> (h, instrs_replace_var fvar il [])) hl)) cont

and instrs_replace_var fvar instrs cont =
  List.fold_right (instr_replace_var fvar) instrs cont

let step_replace_var fvar step =
  { step with
    step_checks = List.map (fun (l, v) -> (l, value_replace_var fvar v)) step.step_checks;
    step_locals = Utils.remove_duplicates (List.map fvar step.step_locals);
    step_instrs = instrs_replace_var fvar step.step_instrs [];
}

let rec machine_replace_var fvar m =
  { m with
    mstep = step_replace_var fvar m.mstep
  }

let machine_reuse_var m reuse =
  let fvar v =
    try
      Hashtbl.find reuse v.var_id
    with Not_found -> v in
  machine_replace_var fvar m

let prog_reuse_var prog node_schs =
  List.map 
    (fun m -> 
      machine_reuse_var m (Utils.IMap.find m.mname.node_id node_schs).Scheduling.reuse_table
    ) prog

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
