(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

open Graph

type rat = int*int
type ident = string
type tag = int
type longident = (string * tag) list

exception TransposeError of int*int

(** General utility functions. *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

module IdentModule =
struct (* Node module *)
  type t = ident
  let compare = compare
  let hash n = Hashtbl.hash n
  let equal n1 n2 = n1 = n2
end

module IMap = Map.Make(IdentModule)
    
module ISet = Set.Make(IdentModule)

exception DeSome
let desome x = match x with Some x -> x | None -> raise DeSome

let option_map f o =
  match o with
  | None   -> None
  | Some e -> Some (f e)

let add_cons x l =
 if List.mem x l then l else x::l

let rec remove_duplicates l =
 match l with
 | [] -> []
 | t::q -> add_cons t (remove_duplicates q)

let position pred l =
  let rec pos p l =
    match l with
    | [] -> assert false
    | t::q -> if pred t then p else pos (p+1) q
  in pos 0 l

let rec duplicate x n =
 if n < 0 then [] else x :: duplicate x (n - 1)

let enumerate n =
  let rec aux i =
    if i >= n then [] else i :: aux (i+1)
  in aux 0

let rec repeat n f x =
 if n <= 0 then x else repeat (n-1) f (f x)

let transpose_list ll =
  let rec transpose ll =
    match ll with
    | []   -> []
    | [l]  -> List.map (fun el -> [el]) l
    | l::q -> List.map2 (fun el eq -> el::eq) l (transpose q)
  in match ll with
  | []   -> []
  | l::q -> let length_l = List.length l in
	    List.iter (fun l' -> let length_l' = List.length l'
				 in if length_l <> length_l' then raise (TransposeError (length_l, length_l'))) q;
	    transpose ll

let rec filter_upto p n l =
 if n = 0 then [] else
 match l with
 | [] -> []
 | t::q -> if p t then t :: filter_upto p (n-1) q else filter_upto p n q

(* Warning: bad complexity *)
let list_of_imap imap =
  IMap.fold (fun i v (il,vl) -> (i::il,v::vl)) imap ([],[])

(** [gcd a b] returns the greatest common divisor of [a] and [b]. *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(** [lcm a b] returns the least common multiple of [a] and [b]. *)
let lcm a b =
  if a = 0 && b = 0 then
    0
  else a*b/(gcd a b)

(** [sum_rat (a,b) (a',b')] returns the sum of rationals [(a,b)] and
    [(a',b')] *)
let sum_rat (a,b) (a',b') =
  if a = 0 && b = 0 then
    (a',b')
  else if a'=0 && b'=0 then
    (a,b)
  else
    let lcm_bb' = lcm b b' in
    (a*lcm_bb'/b+a'*lcm_bb'/b',lcm_bb')

let simplify_rat (a,b) =
  let gcd = gcd a b in
  if (gcd =0) then
    (a,b)
  else (a/gcd,b/gcd)

let max_rat (a,b) (a',b') =
  let ratio_ab = (float_of_int a)/.(float_of_int b) in
  let ratio_ab' = (float_of_int a')/.(float_of_int b') in
  if ratio_ab > ratio_ab' then
    (a,b)
  else
    (a',b')

(** [list_union l1 l2] returns the union of list [l1] and [l2]. The
    result contains no duplicates. *)
let list_union l1 l2 =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x::tl ->
        if List.mem x acc then
          aux tl acc
        else
          aux tl (x::acc)
  in
  let l1' = aux l1 [] in
  aux l2 l1'

(** [hashtbl_add h1 h2] adds all the bindings in [h2] to [h1]. If the
    intersection is not empty, it replaces the former binding *)
let hashtbl_add h1 h2 =
  Hashtbl.iter (fun key value -> Hashtbl.replace h1 key value) h2

let hashtbl_iterlast h f1 f2 =
  let l = Hashtbl.length h in
  ignore(
  Hashtbl.fold
    (fun k v cpt ->
      if cpt = l then
        begin f2 k v; cpt+1 end
      else
        begin f1 k v; cpt+1 end)
    h 1)

(** Match types variables to 'a, 'b, ..., for pretty-printing. Type
    variables are identified by integers. *)
let tnames = ref ([]: (int * string) list)
let tname_counter = ref 0
(* Same for carriers *)
let crnames = ref ([]: (int * string) list)
let crname_counter = ref 0
(* Same for dimension *)
let dnames = ref ([]: (int * string) list)
let dname_counter = ref 0
(* Same for delays *)
let inames = ref ([]: (int * string) list)
let iname_counter = ref 0

let reset_names () =
  tnames := []; tname_counter := 0; crnames := []; crname_counter := 0; dnames := []; dname_counter := 0; inames := []; iname_counter := 0

(* From OCaml compiler *)
let new_tname () =
  let tname =
    if !tname_counter < 26
    then String.make 1 (Char.chr(97 + !tname_counter))
    else String.make 1 (Char.chr(97 + !tname_counter mod 26)) ^
      string_of_int(!tname_counter / 26) in
  incr tname_counter;
  tname

let new_crname () =
  incr crname_counter;
  Format.sprintf "c%i" (!crname_counter-1)

let name_of_type id =
  try List.assoc id !tnames with Not_found ->
    let name = new_tname () in
    tnames := (id, name) :: !tnames;
    name

let name_of_carrier id =
  let pp_id =
    try List.assoc id !crnames with Not_found ->
      let name = new_crname () in
      crnames := (id,name) :: !crnames;
      name
  in
  pp_id

let new_dname () =
  incr dname_counter;
  Format.sprintf "d%i" (!dname_counter-1)

let name_of_dimension id =
  try List.assoc id !dnames with Not_found ->
    let name = new_dname () in
    dnames := (id, name) :: !dnames;
    name

let new_iname () =
  incr iname_counter;
  Format.sprintf "t%i" (!iname_counter-1)

let name_of_delay id =
  try List.assoc id !inames with Not_found ->
    let name = new_iname () in
    inames := (id, name) :: !inames;
    name

open Format

let print_rat fmt (a,b) =
  if b=1 then
    Format.fprintf fmt "%i" a
  else
    if b < 0 then
      Format.fprintf fmt "%i/%i" (-a) (-b)
    else
      Format.fprintf fmt "%i/%i" a b
	

(* Generic pretty printing *)

let pp_final_char_if_non_empty c l =
  (fun fmt -> match l with [] -> () | _ -> Format.fprintf fmt "%(%)" c)

let pp_newline_if_non_empty l =
  (fun fmt -> match l with [] -> () | _ -> Format.fprintf fmt "@,")

let rec fprintf_list ~sep:sep f fmt = function
  | []   -> ()
  | [e]  -> f fmt e
  | x::r -> Format.fprintf fmt "%a%(%)%a" f x sep (fprintf_list ~sep f) r

let pp_list l pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let rec pp_l l =
    match l with
    | [] -> ()
    | [hd] -> 
        pp_fun hd
    | hd::tl ->
        pp_fun hd;
        if (sep_str="\n") then
          print_newline ()
        else
          print_string sep_str;
        pp_l tl
  in
  pp_l l;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_array a pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let n = Array.length a in
  if n > 0 then
    begin
      Array.iter (fun x -> pp_fun x; print_string sep_str) (Array.sub a 0 (n-1));
      pp_fun a.(n-1)
    end;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_iset fmt t =
  begin
    Format.fprintf fmt "{@ ";
    ISet.iter (fun s -> Format.fprintf fmt "%s@ " s) t;
    Format.fprintf fmt "}@."
  end

let pp_imap pp_val fmt m =
  begin
    Format.fprintf fmt "@[{@ ";
    IMap.iter (fun key v -> Format.fprintf fmt "%s -> %a@ " key pp_val v) m;
    Format.fprintf fmt "}@ @]"
  end
    
let pp_hashtbl t pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let pp_fun1 k v =
    pp_fun k v;
    if (sep_str="\n") then
      print_newline ()
    else
      print_string sep_str
  in
  hashtbl_iterlast t pp_fun1 pp_fun;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_longident lid =
  let pp_fun (nid, tag) =
    print_string nid;
    print_string "(";
    print_int tag;
    print_string ")"
  in
  pp_list lid pp_fun "" "." "."  

let pp_date fmt tm =
  Format.fprintf fmt "%i/%i/%i, %02i:%02i:%02i"
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_mon
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(* Used for uid in variables *)

let var_id_cpt = ref 0
let get_new_id () = incr var_id_cpt;!var_id_cpt


(* for lexing purposes *)

(* Update line number for location info *)
let incr_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }


let last_tag = ref (-1)
let new_tag () =
  incr last_tag; !last_tag


module List =
struct
  include List 
  let iteri2 f l1 l2 =
    if List.length l1 <> List.length l2 then
      raise (Invalid_argument "iteri2: lists have different lengths")
    else
      let rec run idx l1 l2 =
	match l1, l2 with
	| [], [] -> ()
	| hd1::tl1, hd2::tl2 -> (
	  f idx hd1 hd2;
	  run (idx+1) tl1 tl2
	)
	| _ -> assert false
      in
      run 0 l1 l2
end

  
(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
