(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT - LIFL             *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *) 
(*  This file was originally from the Prelude compiler              *)
(*                                                                  *) 
(********************************************************************)

(** Clocks definitions and a few utility functions on clocks. *)
open Utils
open Format

(* (\* Clock type sets, for subtyping. *\) *)
(* type clock_set = *)
(*     CSet_all *)
(*   | CSet_pck of int*rat *)

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
  (* | Pck_up of clock_expr * int *)
  (* | Pck_down of clock_expr * int *)
  (* | Pck_phase of clock_expr * rat *)
  (* | Pck_const of int * rat *)
  | Cvar (* of clock_set *) (* Monomorphic clock variable *)
  | Cunivar (* of clock_set *) (* Polymorphic clock variable *)
  | Clink of clock_expr (* During unification, make links instead of substitutions *)
  | Ccarrying of carrier_expr * clock_expr

type error =
  | Clock_clash of clock_expr * clock_expr
  (* | Not_pck *)
  (* | Clock_set_mismatch of clock_expr * clock_set *)
  | Cannot_be_polymorphic of clock_expr
  | Invalid_imported_clock of clock_expr
  | Invalid_const of clock_expr
  | Factor_zero
  | Carrier_mismatch of carrier_expr * carrier_expr
  | Carrier_extrusion of clock_expr * carrier_expr
  | Clock_extrusion of clock_expr * clock_expr

exception Unify of clock_expr * clock_expr
exception Mismatch of carrier_expr * carrier_expr
exception Scope_carrier of carrier_expr
exception Scope_clock of clock_expr
exception Error of Location.t * error

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
      fprintf fmt "%a -> %a" print_ck_long ck1 print_ck_long ck2
  | Ctuple cklist ->
    fprintf fmt "(%a)"
      (fprintf_list ~sep:" * " print_ck_long) cklist
  | Con (ck,c,l) ->
    fprintf fmt "%a on %s(%a)" print_ck_long ck l print_carrier c
  | Cvar -> fprintf fmt "'_%i" ck.cid 
  | Cunivar -> fprintf fmt "'%i" ck.cid 
  | Clink ck' ->
    fprintf fmt "link %a" print_ck_long ck'
  | Ccarrying (cr,ck') ->
    fprintf fmt "(%a:%a)" print_carrier cr print_ck_long ck'

let new_id = ref (-1)

let rec bottom =
  { cdesc = Clink bottom; cid = -666; cscoped = false }

let new_ck desc scoped =
  incr new_id; {cdesc=desc; cid = !new_id; cscoped = scoped}

let new_var scoped = new_ck Cvar scoped

let new_univar () = new_ck Cunivar false

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

let rename_carrier_static rename cr =
  match (carrier_repr cr).carrier_desc with
  | Carry_const id -> { cr with carrier_desc = Carry_const (rename id) }
  | _              -> (Format.eprintf "internal error: Clocks.rename_carrier_static %a@." print_carrier cr; assert false)

let rec rename_static rename ck =
 match (repr ck).cdesc with
 | Ccarrying (cr, ck') -> { ck with cdesc = Ccarrying (rename_carrier_static rename cr, rename_static rename ck') }
 | Con (ck', cr, l)    -> { ck with cdesc = Con (rename_static rename ck', rename_carrier_static rename cr, l) }
 | _                   -> ck

let uncarrier ck =
 match ck.cdesc with
 | Ccarrying (_, ck') -> ck'
 | _                  -> ck

(* Removes all links in a clock. Only used for clocks
   simplification though. *)
let rec simplify ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      new_ck (Carrow (simplify ck1, simplify ck2)) ck.cscoped
  | Ctuple cl ->
      new_ck (Ctuple (List.map simplify cl)) ck.cscoped
  | Con (ck', c, l) ->
      new_ck (Con (simplify ck', c, l)) ck.cscoped
  | Cvar | Cunivar -> ck
  | Clink ck' -> simplify ck'
  | Ccarrying (cr,ck') -> new_ck (Ccarrying (cr, simplify ck')) ck.cscoped

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

let clock_on ck cr l =
 clock_of_clock_list (List.map (fun ck -> new_ck (Con (ck,cr,l)) true) (clock_list_of_clock ck))

let clock_current ck =
 clock_of_clock_list (List.map (fun ck -> match (repr ck).cdesc with Con(ck',_,_) -> ck' | _ -> assert false) (clock_list_of_clock ck))

let clock_of_impnode_clock ck =
  let ck = repr ck in
  match ck.cdesc with
  | Carrow _ | Clink _ | Cvar | Cunivar ->
      failwith "internal error clock_of_impnode_clock"
  | Ctuple cklist -> List.hd cklist
  | Con (_,_,_) 
  | Ccarrying (_,_) -> ck


(** [is_polymorphic ck] returns true if [ck] is polymorphic. *)
let rec is_polymorphic ck =
  match ck.cdesc with
  | Cvar -> false
  | Carrow (ck1,ck2) -> (is_polymorphic ck1) || (is_polymorphic ck2)
  | Ctuple ckl -> List.exists (fun c -> is_polymorphic c) ckl
  | Con (ck',_,_) -> is_polymorphic ck'
  | Cunivar  -> true
  | Clink ck' -> is_polymorphic ck'
  | Ccarrying (_,ck') -> is_polymorphic ck'

(** [constrained_vars_of_clock ck] returns the clock variables subject
    to sub-typing constraints appearing in clock [ck]. Removes duplicates *)
(* Used mainly for debug, non-linear complexity. *)
let rec constrained_vars_of_clock ck =
  let rec aux vars ck =
    match ck.cdesc with
    | Cvar -> vars
    | Carrow (ck1,ck2) ->
        let l = aux vars ck1 in
        aux l ck2
    | Ctuple ckl ->
        List.fold_left
          (fun acc ck' -> aux acc ck') 
          vars ckl
    | Con (ck',_,_) -> aux vars ck'
    | Cunivar -> vars
    | Clink ck' -> aux vars ck'
    | Ccarrying (_,ck') -> aux vars ck'
  in
  aux [] ck

let eq_carrier cr1 cr2 =
  match (carrier_repr cr1).carrier_desc, (carrier_repr cr2).carrier_desc with
 | Carry_const id1, Carry_const id2 -> id1 = id2
 | _                                -> cr1.carrier_id = cr2.carrier_id

let eq_clock ck1 ck2 =
 (repr ck1).cid = (repr ck2).cid

(* Returns the clock root of a clock *)
let rec root ck =
  let ck = repr ck in
  match ck.cdesc with
  | Ctuple (ck'::_)
  | Con (ck',_,_) | Clink ck' | Ccarrying (_,ck') -> root ck'
  | Cvar | Cunivar -> ck
  | Carrow _ | Ctuple _ -> failwith "Internal error root"

(* Returns the branch of clock [ck] in its clock tree *)
let rec branch ck =
  let rec branch ck acc =
    match (repr ck).cdesc with
    | Ccarrying (_, ck) -> branch ck acc
    | Con (ck, cr, l)   -> branch ck ((cr, l) :: acc)
    | Ctuple (ck::_)    -> branch ck acc
    | Ctuple _
    | Carrow _          -> assert false
    | _                 -> acc
  in branch ck [];;

let clock_of_root_branch r br =
 List.fold_left (fun ck (cr,l) -> new_ck (Con (ck, cr, l)) true) r br

(* Computes the (longest) common prefix of two branches *)
let rec common_prefix br1 br2 =
 match br1, br2 with
 | []          , _
 | _           , []           -> []
 | (cr1,l1)::q1, (cr2,l2)::q2 ->
   if eq_carrier cr1 cr2 && (l1 = l2)
   then (cr1, l1) :: common_prefix q1 q2
   else []

(* Tests whether clock branches [br1] nd [br2] are statically disjoint *)
let rec disjoint_branches br1 br2 =
 match br1, br2 with
 | []          , _
 | _           , []           -> false
 | (cr1,l1)::q1, (cr2,l2)::q2 -> eq_carrier cr1 cr2 && ((l1 <> l2) || disjoint_branches q1 q2);;

(* Disjunction relation between variables based upon their static clocks. *)
let disjoint ck1 ck2 =
  eq_clock (root ck1) (root ck2) && disjoint_branches (branch ck1) (branch ck2)

let print_cvar fmt cvar =
  match cvar.cdesc with
  | Cvar ->
 (*
      if cvar.cscoped
      then
	fprintf fmt "[_%s]"
	  (name_of_type cvar.cid)
      else
 *)
	fprintf fmt "_%s"
	  (name_of_type cvar.cid)
  | Cunivar ->
 (*
      if cvar.cscoped
      then
	fprintf fmt "['%s]"
	  (name_of_type cvar.cid)
      else
 *)
	fprintf fmt "'%s"
	  (name_of_type cvar.cid)
  | _ -> failwith "Internal error print_cvar"

(* Nice pretty-printing. Simplifies expressions before printing them. Non-linear
   complexity. *)
let print_ck fmt ck =
  let rec aux fmt ck =
    match ck.cdesc with
    | Carrow (ck1,ck2) ->
      fprintf fmt "%a -> %a" aux ck1 aux ck2
    | Ctuple cklist ->
      fprintf fmt "(%a)" 
	(fprintf_list ~sep:" * " aux) cklist
    | Con (ck,c,l) ->
      fprintf fmt "%a on %s(%a)" aux ck l print_carrier c
    | Cvar ->
(*
      if ck.cscoped
      then
        fprintf fmt "[_%s]" (name_of_type ck.cid)
      else
*)
	fprintf fmt "_%s" (name_of_type ck.cid)
    | Cunivar ->
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
  match ck.cdesc with
  | Carrow _
  | Ctuple _
  | Cvar 
  | Cunivar    -> ()
  | Con (ck,c,l) ->
    fprintf fmt "%a when %s(%a)" print_ck_suffix ck l print_carrier c
  | Clink ck' ->
    print_ck_suffix fmt ck'
  | Ccarrying (cr,ck') ->
    fprintf fmt "%a" print_ck_suffix ck'


let pp_error fmt = function
  | Clock_clash (ck1,ck2) ->
      reset_names ();
      fprintf fmt "Expected clock %a, got clock %a@."
      print_ck ck1
      print_ck ck2
  | Carrier_mismatch (cr1, cr2) ->
     fprintf fmt "Name clash. Expected clock %a, got clock %a@."
       print_carrier cr1
       print_carrier cr2
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


(* Used in rename functions in Corelang. We have to propagate the renaming to
   ids of variables clocking the signals *)

(* Carrier are not renamed. They corresponds to enumerated type constants *)
(*
let rec rename_carrier f c =
  { c with carrier_desc = rename_carrier_desc fvar c.carrier_desc }
and rename_carrier_desc f 
let re = rename_carrier f
  match cd with
  | Carry_const id -> Carry_const (f id)
  | Carry_link ce -> Carry_link (re ce)
  | _ -> cd
*)

     
let rec rename_clock_expr fvar c =
  { c with cdesc = rename_clock_desc fvar c.cdesc }
and rename_clock_desc fvar cd =
  let re = rename_clock_expr fvar in
  match cd with
  | Carrow (c1, c2) -> Carrow (re c1, re c2)
  | Ctuple cl -> Ctuple (List.map re cl)
  | Con (c1, car, id) -> Con (re c1, car, fvar id)
  | Cvar 
  | Cunivar -> cd
  | Clink c -> Clink (re c)
  | Ccarrying (car, c) -> Ccarrying (car, re c)
    
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
