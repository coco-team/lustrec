(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
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

(** Clocks definitions and a few utility functions on clocks. *)
open Utils
open Format

(* Clock type sets, for subtyping. *)
type clock_set =
    CSet_all
  | CSet_pck of int*rat

(* Clock carriers basically correspond to the "c" in "x when c" *)
type carrier_desc =
  | Carry_const of ident
  | Carry_name
  | Carry_var
  | Carry_link of carrier_expr

(* Carriers are scoped, to detect clock extrusion. In other words, we
   check the scope of a clock carrier before generalizing it. *)
and carrier_expr =
    {mutable carrier_desc: carrier_desc;
     mutable carrier_scoped: bool;
     carrier_id: int}

type clock_expr =
    {mutable cdesc: clock_desc;
     mutable cscoped: bool;
     cid: int}

(* pck stands for periodic clock. Easier not to separate pck from other clocks *)
and clock_desc =
  | Carrow of clock_expr * clock_expr
  | Ctuple of clock_expr list
  | Con of clock_expr * carrier_expr * ident
  | Pck_up of clock_expr * int
  | Pck_down of clock_expr * int
  | Pck_phase of clock_expr * rat
  | Pck_const of int * rat
  | Cvar of clock_set (* Monomorphic clock variable *)
  | Cunivar of clock_set (* Polymorphic clock variable *)
  | Clink of clock_expr (* During unification, make links instead of substitutions *)
  | Ccarrying of carrier_expr * clock_expr

type error =
  | Clock_clash of clock_expr * clock_expr
  | Not_pck
  | Clock_set_mismatch of clock_expr * clock_set
  | Cannot_be_polymorphic of clock_expr
  | Invalid_imported_clock of clock_expr
  | Invalid_const of clock_expr
  | Factor_zero
  | Carrier_mismatch of carrier_expr * carrier_expr
  | Carrier_extrusion of clock_expr * carrier_expr
  | Clock_extrusion of clock_expr * clock_expr

exception Unify of clock_expr * clock_expr
exception Subsume of clock_expr * clock_set
exception Mismatch of carrier_expr * carrier_expr
exception Scope_carrier of carrier_expr
exception Scope_clock of clock_expr
exception Error of Location.t * error

let new_id = ref (-1)

let new_ck desc scoped =
  incr new_id; {cdesc=desc; cid = !new_id; cscoped = scoped}

let new_var scoped =
  new_ck (Cvar CSet_all) scoped

let new_univar () =
  new_ck (Cunivar CSet_all) false

let new_carrier_id = ref (-1)

let new_carrier desc scoped =
  incr new_carrier_id; {carrier_desc = desc;
                        carrier_id = !new_carrier_id;
                        carrier_scoped = scoped}

let new_carrier_name () =
  new_carrier Carry_name true

let rec repr =
  function
      {cdesc=Clink ck'} ->
        repr ck'
    | ck -> ck

let rec carrier_repr =
  function {carrier_desc = Carry_link cr'} -> carrier_repr cr'
    | cr -> cr

(** Splits [ck] into the [lhs,rhs] of an arrow clock. Expects an arrow clock
    (ensured by language syntax) *)
let split_arrow ck =
  match (repr ck).cdesc with
  | Carrow (cin,cout) -> cin,cout
    (* Functions are not first order, I don't think the var case
       needs to be considered here *)
  | _ -> failwith "Internal error: not an arrow clock"

let get_carrier_name ck =
 match (repr ck).cdesc with
 | Ccarrying (cr, _) -> Some cr
 | _                 -> None

let uncarrier ck =
 match ck.cdesc with
 | Ccarrying (_, ck') -> ck'
 | _                  -> ck

(* Removes all links in a clock. Only used for clocks
   simplification though. *)
let rec deep_repr ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      new_ck (Carrow (deep_repr ck1, deep_repr ck2)) ck.cscoped
  | Ctuple cl ->
      new_ck (Ctuple (List.map deep_repr cl)) ck.cscoped
  | Con (ck', c, l) ->
      new_ck (Con (deep_repr ck', c, l)) ck.cscoped
  | Pck_up (ck',k) ->
      new_ck (Pck_up (deep_repr ck',k)) ck.cscoped
  | Pck_down (ck',k) ->
      new_ck (Pck_down (deep_repr ck',k)) ck.cscoped
  | Pck_phase (ck',q) ->
      new_ck (Pck_phase (deep_repr ck',q)) ck.cscoped
  | Pck_const (_,_) | Cvar _ | Cunivar _ -> ck
  | Clink ck' -> deep_repr ck'
  | Ccarrying (cr,ck') -> new_ck (Ccarrying (cr, deep_repr ck')) ck.cscoped

(** Splits ck into the [lhs,rhs] of an arrow clock. Expects an arrow clock
    (ensured by language syntax) *)
let split_arrow ck =
  match (repr ck).cdesc with
  | Carrow (cin,cout) -> cin,cout
  | _ -> failwith "Internal error: not an arrow clock"

(** Returns the clock corresponding to a clock list. *)
let clock_of_clock_list ckl =
  if (List.length ckl) > 1 then
    new_ck (Ctuple ckl) true
  else
    List.hd ckl

let clock_list_of_clock ck =
 match (repr ck).cdesc with
 | Ctuple cl -> cl
 | _         -> [ck]

let clock_of_impnode_clock ck =
  let ck = repr ck in
  match ck.cdesc with
  | Carrow _ | Clink _ | Cvar _ | Cunivar _ ->
      failwith "internal error clock_of_impnode_clock"
  | Ctuple cklist -> List.hd cklist
  | Con (_,_,_) | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_)
  | Pck_const (_,_) | Ccarrying (_,_) -> ck

(** [intersect set1 set2] returns the intersection of clock subsets
    [set1] and [set2]. *)
let intersect set1 set2 =
  match set1,set2 with
  | CSet_all,_ -> set2
  | _,CSet_all -> set1
  | CSet_pck (k,q), CSet_pck (k',q') ->
      let k'',q'' = lcm k k',max_rat q q' in
      CSet_pck (k'',q'')

(** [can_be_pck ck] returns true if [ck] "may be" a pclock (the uncertainty is
    due to clock variables) *)
let rec can_be_pck ck =
  match (repr ck).cdesc with
  | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_) | Pck_const (_,_)
  | Cvar _ | Cunivar _ ->
      true
  | Ccarrying (_,ck') -> can_be_pck ck
  | _ -> false

(** [is_concrete_pck ck] returns true if [ck] is a concrete [pck] (pck
    transformations applied to a pck constant) *)
let rec is_concrete_pck ck =
  match ck.cdesc with
  | Carrow (_,_) | Ctuple _ | Con (_,_,_)
  | Cvar _ | Cunivar _ -> false
  | Pck_up (ck',_) | Pck_down (ck',_) -> is_concrete_pck ck'
  | Pck_phase (ck',_) -> is_concrete_pck ck'
  | Pck_const (_,_) -> true
  | Clink ck' -> is_concrete_pck ck'
  | Ccarrying (_,ck') -> false

(** [is_polymorphic ck] returns true if [ck] is polymorphic. *)
let rec is_polymorphic ck =
  match ck.cdesc with
  | Cvar _ | Pck_const (_,_) -> false
  | Carrow (ck1,ck2) -> (is_polymorphic ck1) || (is_polymorphic ck2)
  | Ctuple ckl -> List.exists (fun c -> is_polymorphic c) ckl
  | Con (ck',_,_) -> is_polymorphic ck'
  | Pck_up (ck',_) | Pck_down (ck',_) -> is_polymorphic ck'
  | Pck_phase (ck',_) -> is_polymorphic ck'
  | Cunivar _ -> true
  | Clink ck' -> is_polymorphic ck'
  | Ccarrying (_,ck') -> is_polymorphic ck'

(** [constrained_vars_of_clock ck] returns the clock variables subject
    to sub-typing constraints appearing in clock [ck]. Removes duplicates *)
(* Used mainly for debug, non-linear complexity. *)
let rec constrained_vars_of_clock ck =
  let rec aux vars ck =
    match ck.cdesc with
    | Pck_const (_,_) ->
        vars
    | Cvar cset ->
        begin
          match cset with
          | CSet_all -> vars
          | _ ->
              list_union [ck] vars
        end
    | Carrow (ck1,ck2) ->
        let l = aux vars ck1 in
        aux l ck2
    | Ctuple ckl ->
        List.fold_left
          (fun acc ck' -> aux acc ck') 
          vars ckl
    | Con (ck',_,_) -> aux vars ck'
    | Pck_up (ck',_) | Pck_down (ck',_) -> aux vars ck'
    | Pck_phase (ck',_) -> aux vars ck'
    | Cunivar cset ->
        begin
          match cset with
          | CSet_all -> vars
          | _ -> list_union [ck] vars
        end
    | Clink ck' -> aux vars ck'
    | Ccarrying (_,ck') -> aux vars ck'
  in
  aux [] ck

let print_ckset fmt s =
  match s with
  | CSet_all -> ()
  | CSet_pck (k,q) ->
      let (a,b) = simplify_rat q in
      if k = 1 && a = 0 then
        fprintf fmt "<:P"
      else
        fprintf fmt "<:P_(%i,%a)" k print_rat (a,b)

let rec print_carrier fmt cr =
 (* (if cr.carrier_scoped then fprintf fmt "[%t]" else fprintf fmt "%t") (fun fmt -> *)
  match cr.carrier_desc with
  | Carry_const id -> fprintf fmt "%s" id
  | Carry_name ->
      fprintf fmt "_%s" (name_of_carrier cr.carrier_id)
  | Carry_var ->
    fprintf fmt "'%s" (name_of_carrier cr.carrier_id)
  | Carry_link cr' ->
    print_carrier fmt cr'

(* Simple pretty-printing, performs no simplifications. Linear
   complexity. For debug mainly. *)
let rec print_ck_long fmt ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      fprintf fmt "%a->%a" print_ck_long ck1 print_ck_long ck2
  | Ctuple cklist ->
    fprintf fmt "(%a)"
      (fprintf_list ~sep:" * " print_ck_long) cklist
  | Con (ck,c,l) ->
    fprintf fmt "%a on %s(%a)" print_ck_long ck l print_carrier c
  | Pck_up (ck,k) ->
    fprintf fmt "%a*^%i" print_ck_long ck k
  | Pck_down (ck,k) ->
    fprintf fmt "%a/^%i" print_ck_long ck k
  | Pck_phase (ck,q) ->
    fprintf fmt "%a~>%a" print_ck_long ck print_rat (simplify_rat q)
  | Pck_const (n,p) ->
    fprintf fmt "(%i,%a)" n print_rat (simplify_rat p)
  | Cvar cset ->
    fprintf fmt "'_%i%a" ck.cid print_ckset cset
  | Cunivar cset ->
    fprintf fmt "'%i%a" ck.cid print_ckset cset
  | Clink ck' ->
    fprintf fmt "link %a" print_ck_long ck'
  | Ccarrying (cr,ck') ->
    fprintf fmt "(%a:%a)" print_carrier cr print_ck_long ck'

(** [period ck] returns the period of [ck]. Expects a constant pclock
    expression belonging to the correct clock set. *)
let rec period ck =
  let rec aux ck =
    match ck.cdesc with
    | Carrow (_,_) | Ctuple _ | Con (_,_,_)
    | Cvar _ | Cunivar _ ->
        failwith "internal error: can only compute period of const pck"
    | Pck_up (ck',k) ->
        (aux ck')/.(float_of_int k)
    | Pck_down (ck',k) ->
        (float_of_int k)*.(aux ck')
    | Pck_phase (ck',_) ->
        aux ck'
    | Pck_const (n,_) ->
        float_of_int n
    | Clink ck' -> aux ck'
    | Ccarrying (_,ck') -> aux ck'
  in
  int_of_float (aux ck)

(** [phase ck] returns the phase of [ck]. It is not expressed as a
    fraction of the period, but instead as an amount of time. Expects a
    constant expression belonging to the correct P_k *)
let phase ck =
  let rec aux ck =
  match ck.cdesc with
  | Carrow (_,_) | Ctuple _ | Con (_,_,_)
  | Cvar _ | Cunivar _ ->
      failwith "internal error: can only compute phase of const pck"
  | Pck_up (ck',_) ->
      aux ck'
  | Pck_down (ck',k) ->
      aux ck'
  | Pck_phase (ck',(a,b)) ->
      let n = period ck' in
      let (a',b') = aux ck' in
      sum_rat (a',b') (n*a,b)
  | Pck_const (n,(a,b)) ->
      (n*a,b)
  | Clink ck' -> aux ck'
  | Ccarrying (_,ck') -> aux ck'
  in
  let (a,b) = aux ck in
  simplify_rat (a,b)

let eq_carrier cr1 cr2 =
  match (carrier_repr cr1).carrier_desc, (carrier_repr cr2).carrier_desc with
 | Carry_const id1, Carry_const id2 -> id1 = id2
 | _                                -> cr1.carrier_id = cr2.carrier_id

(* Returns the clock root of a clock *)
let rec root ck =
  match (repr ck).cdesc with
  | Con (ck',_,_) | Clink ck' | Ccarrying (_,ck') ->
      root ck'
  | Pck_up _ | Pck_down _ | Pck_phase _ | Pck_const _ | Cvar _ | Cunivar _ -> ck
  | Carrow _ | Ctuple _ -> failwith "Internal error pclock_parent"

(* Returns the branch of clock [ck] in its clock tree *)
let rec branch ck =
  let rec branch ck acc =
    match (repr ck).cdesc with
    | Ccarrying (_, ck) -> branch ck acc
    | Con (ck, cr, l)   -> branch ck ((cr, l) :: acc)
    | Ctuple _
    | Carrow _          -> assert false
    | _                 -> acc
  in branch ck [];;

(* Tests whether clock branches [br1] nd [br2] are statically disjoint *)
let rec disjoint_branches br1 br2 =
 match br1, br2 with
 | []          , _
 | _           , []           -> false
 | (cr1,l1)::q1, (cr2,l2)::q2 -> eq_carrier cr1 cr2 && ((l1 <> l2) || disjoint_branches q1 q2);;

(* Disjunction relation between variables based upon their static clocks. *)
let disjoint ck1 ck2 =
 root ck1 = root ck2 && disjoint_branches (branch ck1) (branch ck2);;

(** [normalize pck] returns the normal form of clock [pck]. *)
let normalize pck =
  let changed = ref true in
  let rec aux pck =
    match pck.cdesc with
    | Pck_up ({cdesc=Pck_up (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_up (aux pck',k*k')) pck.cscoped
    | Pck_up ({cdesc=Pck_down (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_down (new_ck (Pck_up (aux pck',k)) pck.cscoped,k')) pck.cscoped
    | Pck_up ({cdesc=Pck_phase (pck',(a,b))},k) ->
        changed:=true;
        new_ck (Pck_phase (new_ck (Pck_up (aux pck',k)) pck.cscoped,(a*k,b))) pck.cscoped
    | Pck_down ({cdesc=Pck_down (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_down (aux pck',k*k')) pck.cscoped
    | Pck_down ({cdesc=Pck_phase (pck',(a,b))},k) ->
        changed:=true;
        new_ck (Pck_phase (new_ck (Pck_down (aux pck',k)) pck.cscoped,(a,b*k))) pck.cscoped
    | Pck_phase ({cdesc=Pck_phase (pck',(a',b'))},(a,b)) ->
        changed:=true;
        let (a'',b'') = sum_rat (a,b) (a',b') in
        new_ck (Pck_phase (aux pck',(a'',b''))) pck.cscoped
    | Pck_up (pck',k') ->
        new_ck (Pck_up (aux pck',k')) pck.cscoped
    | Pck_down (pck',k') ->
        new_ck (Pck_down (aux pck',k')) pck.cscoped
    | Pck_phase (pck',k') ->
        new_ck (Pck_phase (aux pck',k')) pck.cscoped
    | Ccarrying (cr,ck') ->
        new_ck (Ccarrying (cr, aux ck')) pck.cscoped
    | _ -> pck
  in
  let nf=ref pck in
  while !changed do
    changed:=false;
    nf:=aux !nf
  done;
  !nf

(** [canonize pck] reduces transformations of [pck] and removes
    identity transformations. Expects a normalized periodic clock ! *)
let canonize pck =
  let rec remove_id_trans pck =
    match pck.cdesc with
    | Pck_up (pck',1) | Pck_down (pck',1) | Pck_phase (pck',(0,_)) ->
        remove_id_trans pck'
    | _ -> pck
  in
  let pck =
    match pck.cdesc with
    | Pck_phase ({cdesc=Pck_down ({cdesc=Pck_up (v,k)},k')},k'') ->
        let gcd = gcd k k' in
        new_ck (Pck_phase
                  (new_ck (Pck_down
                             (new_ck (Pck_up (v,k/gcd)) pck.cscoped,k'/gcd))
                     pck.cscoped,k''))
          pck.cscoped
    | Pck_down ({cdesc=Pck_up (v,k)},k') ->
        let gcd = gcd k k' in
        new_ck (Pck_down (new_ck (Pck_up (v,k/gcd)) pck.cscoped,k'/gcd)) pck.cscoped
    | _ -> pck
  in
  remove_id_trans pck

(** [simplify pck] applies pclocks simplifications to [pck] *)
let simplify pck =
  if (is_concrete_pck pck) then
    let n = period pck in
    let (a,b) = phase pck in
    let phase' = simplify_rat (a,b*n) in 
    new_ck (Pck_const (n,phase')) pck.cscoped
  else
    let pck' = deep_repr pck in
    let nf_pck = normalize pck' in
    canonize nf_pck
        
let print_cvar fmt cvar =
  match cvar.cdesc with
  | Cvar cset ->
 (*
      if cvar.cscoped
      then
	fprintf fmt "[_%s%a]"
	  (name_of_type cvar.cid)
	  print_ckset cset
      else
 *)
	fprintf fmt "_%s%a"
	  (name_of_type cvar.cid)
	  print_ckset cset
  | Cunivar cset ->
 (*
      if cvar.cscoped
      then
	fprintf fmt "['%s%a]"
	  (name_of_type cvar.cid)
	  print_ckset cset
      else
 *)
	fprintf fmt "'%s%a"
	  (name_of_type cvar.cid)
	  print_ckset cset
  | _ -> failwith "Internal error print_cvar"

(* Nice pretty-printing. Simplifies expressions before printing them. Non-linear
   complexity. *)
let print_ck fmt ck =
  let rec aux fmt ck =
    let ck = simplify ck in
    match ck.cdesc with
    | Carrow (ck1,ck2) ->
      fprintf fmt "%a->%a" aux ck1 aux ck2
    | Ctuple cklist ->
      fprintf fmt "(%a)" 
	(fprintf_list ~sep:" * " aux) cklist
    | Con (ck,c,l) ->
      fprintf fmt "%a on %s(%a)" aux ck l print_carrier c
    | Pck_up (ck,k) ->
      fprintf fmt "%a*.%i" aux ck k
    | Pck_down (ck,k) ->
      fprintf fmt "%a/.%i" aux ck k
    | Pck_phase (ck,q) ->
      fprintf fmt "%a->.%a" aux ck print_rat (simplify_rat q)
    | Pck_const (n,p) ->
      fprintf fmt "(%i,%a)" n print_rat (simplify_rat p)
    | Cvar cset ->
(*
      if ck.cscoped
      then
        fprintf fmt "[_%s]" (name_of_type ck.cid)
      else
*)
	fprintf fmt "_%s" (name_of_type ck.cid)
    | Cunivar cset ->
(*
      if ck.cscoped
      then
        fprintf fmt "['%s]" (name_of_type ck.cid)
      else
*)
        fprintf fmt "'%s" (name_of_type ck.cid)
    | Clink ck' ->
        aux fmt ck'
    | Ccarrying (cr,ck') ->
      fprintf fmt "(%a:%a)" print_carrier cr aux ck'
  in
  let cvars = constrained_vars_of_clock ck in
  aux fmt ck;
  if cvars <> [] then
    fprintf fmt " (where %a)"
      (fprintf_list ~sep:", " print_cvar) cvars

(* prints only the Con components of a clock, useful for printing nodes *)
let rec print_ck_suffix fmt ck =
  let ck = simplify ck in
  match ck.cdesc with
  | Carrow _
  | Ctuple _
  | Cvar _
  | Cunivar _   -> ()
  | Con (ck,c,l) ->
    fprintf fmt "%a when %s(%a)" print_ck_suffix ck l print_carrier c
  | Clink ck' ->
    print_ck_suffix fmt ck'
  | Ccarrying (cr,ck') ->
    fprintf fmt "%a" print_ck_suffix ck'
  | _ -> assert false

let pp_error fmt = function
  | Clock_clash (ck1,ck2) ->
      reset_names ();
      fprintf fmt "Expected clock %a, got clock %a@."
      print_ck ck1
      print_ck ck2
  | Not_pck ->
    fprintf fmt "The clock of this expression must be periodic@."
  | Carrier_mismatch (cr1, cr2) ->
     fprintf fmt "Name clash. Expected clock %a, got clock %a@."
       print_carrier cr1
       print_carrier cr2
  | Clock_set_mismatch (ck,cset) ->
      reset_names ();
    fprintf fmt "Clock %a is not included in clock set %a@."
      print_ck ck
      print_ckset cset
  | Cannot_be_polymorphic ck ->
      reset_names ();
    fprintf fmt "The main node cannot have a polymorphic clock: %a@."
      print_ck ck
  | Invalid_imported_clock ck ->
      reset_names ();
    fprintf fmt "Not a valid imported node clock: %a@."
      print_ck ck
  | Invalid_const ck ->
      reset_names ();
    fprintf fmt "Clock %a is not a valid periodic clock@."
      print_ck ck;
  | Factor_zero ->
    fprintf fmt "Cannot apply clock transformation with factor 0@."
  | Carrier_extrusion (ck,cr) ->
    fprintf fmt "This node has clock@.%a@.It is invalid as %a escapes its scope@."
      print_ck ck
      print_carrier cr
  | Clock_extrusion (ck_node,ck) ->
    fprintf fmt "This node has clock@.%a@.It is invalid as %a escapes its scope@."
      print_ck ck_node
      print_ck ck

let const_of_carrier cr =
 match (carrier_repr cr).carrier_desc with
 | Carry_const id -> id
 | Carry_name
 | Carry_var
 | Carry_link _ -> (Format.eprintf "internal error: const_of_carrier %a@." print_carrier cr; assert false) (* TODO check this Xavier *)
 
let uneval const cr =
  (*Format.printf "Clocks.uneval %s %a@." const print_carrier cr;*)
  let cr = carrier_repr cr in
  match cr.carrier_desc with
  | Carry_var -> cr.carrier_desc <- Carry_const const
  | Carry_name -> cr.carrier_desc <- Carry_const const
  | _         -> assert false

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
