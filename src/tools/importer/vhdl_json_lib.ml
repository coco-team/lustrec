open Yojson.Safe
open Yojson.Safe.Util

let rec assoc_map_except_str l f str =
  match l with
  | (s,x)::y -> 
    if (String.equal s str) then 
      assoc_map_except_str y f str
    else
      (s,f str x)::assoc_map_except_str y f str
  | [] -> []

let rec map_2_args f l arg1 =
  match l with
  | hd::tl -> (f arg1 hd)::(map_2_args f tl arg1)
  | [] -> []

(*
Remove `Assoc nodes with tag 'str' in json j
*)
let rec prune_str str json =
  match json with
    | `Assoc ((t,hd)::tl) -> 
      if (String.equal str t) then
        `Assoc (assoc_map_except_str tl prune_str str)
      else
        `Assoc ((t, prune_str str hd)::(assoc_map_except_str tl prune_str str))
    | `List (hd::tl) -> `List ((prune_str str hd)::(map_2_args prune_str tl str))
    | `String (s) -> if (String.equal str s) then `String ("") else `String (s)
    | x -> x

(*******************)

let rec name_pair_list_to_string l =
  match l with
  | (t, `String(x))::tl -> 
    if (String.equal t "name") then 
      (x::name_pair_list_to_string tl) 
    else 
      (name_pair_list_to_string tl)
  | _ -> []

let rec assoc_filter_string l str =
  match l with
  | `Assoc (x) -> name_pair_list_to_string x
  | _ -> []

(********************)

let rec pairlist_remove str l f =
  match l with
  | (t,j)::tl ->
    if (String.equal t str) then
      (f j)::(pairlist_remove str tl f)
    else
      `Assoc ((t, f j)::[])::(pairlist_remove str tl f)
  | [] -> []

(******************)
let rec assoc_elem_fst pair_list = 
  match pair_list with 
  | (t,j)::tl -> t::(assoc_elem_fst tl)
  | [] -> []

let rec assoc_elem_snd pair_list = 
  match pair_list with 
  | (t,j)::tl -> j::(assoc_elem_snd tl) 
  | [] -> []

let rec assoc_elem_filter pair_list str = 
  match pair_list with 
  | (t,j)::tl -> if (String.equal t str) then 
                    (t,j)::(assoc_elem_filter tl str) 
                  else assoc_elem_filter tl str
  | [] -> []

let rec assoc_elem_filternot pair_list str = 
  match pair_list with 
  | (t,j)::tl -> if (not (String.equal t str)) then 
                    (t,j)::(assoc_elem_filternot tl str) 
                  else assoc_elem_filternot tl str
  | [] -> []

let rec assoc_elem_filter_snd pair_list str = 
  match pair_list with 
  | (t,j)::tl -> if (String.equal t str) then 
                    j::(assoc_elem_filter_snd tl str) 
                  else assoc_elem_filter_snd tl str
  | [] -> []

let rec assoc_elem_filternot_snd pair_list str = 
  match pair_list with 
  | (t,j)::tl -> if (not (String.equal t str)) then 
                    j::(assoc_elem_filter_snd tl str) 
                  else assoc_elem_filter_snd tl str
  | [] -> []

let rec pairlist_snd_as_list pair_list str = 
  match pair_list with 
  | (t,j)::tl -> if (String.equal t str) then 
                    (t,`List (j::[]))::(pairlist_snd_as_list tl str)
                  else (t,j)::(pairlist_snd_as_list tl str)
  | [] -> []

let all_members str json =
  match json with
  | `Assoc (l) -> assoc_elem_filter_snd l str
  | _ -> []

let retain_other_members str json =
  match json with
  | `Assoc (l) -> `Assoc (assoc_elem_filter l str)
  | _ -> `Null

(*
DESIGN_UNIT as lists
*)
let vhdl_json_designunits_content_as_list json =
  let designunits_contents = json |> member "DESIGN_FILE" |> all_members "DESIGN_UNIT" in
  `List designunits_contents

let vhdl_json_designfile_content_excluding json str =
  json |> member "DESIGN_FILE" |> retain_other_members "DESIGN_UNIT" 

let vhdl_json_list_designunits json =
  let designunits_list = vhdl_json_designunits_content_as_list json in
  `Assoc (("DESIGN_FILE", (`Assoc (("DESIGN_UNIT", designunits_list)::[])))::[])

let rec pairlist_contains_str str l =
  match l with
  | (t,j)::tl -> if (String.equal t str) then true else pairlist_contains_str str tl
  | [] -> false

(*
ITEM element content as list
*)
let assoc_elem_as_list str json =
  match json with
  | `Assoc (l) -> `Assoc (pairlist_snd_as_list l str)
  | x -> x

let rec map_list map_f l f =
  match l with
  | hd::tl -> (map_f (f hd) f)::(map_list map_f tl f)
  | [] -> []

let rec map_pairlist map_f l f =
  match l with
  | (t,j)::tl -> (t, map_f (f j) f)::(map_pairlist map_f tl f)
  | [] -> []

let rec map_snd f l =
  match l with
  | (t,j)::tl -> (t,f j)::(map_snd f tl)
  | [] -> []

let rec map_all json f =
  match json with
  | `Assoc ((t,j)::tl) -> 
    `Assoc ((t,(map_all (f j) f))::(map_pairlist map_all tl f))
  | `List (hd::tl) -> 
    `List ((map_all (f hd) f)::(map_list map_all tl f))
  | x -> x

let numeric_literal_simpl json =
  match json with
  | `Assoc (("NUMERIC_LITERAL", `Assoc (("TOKEN", `Assoc (("text", `String(x))::[]))::[]))::[]) -> `String (x)
  | x -> x

let flatten_numeric_literal json =
  map_all json (numeric_literal_simpl)

let to_list_str str json =
  map_all json (assoc_elem_as_list str)

let rec to_list_content_str str json =
  match json with
  | `Assoc (l) -> if (pairlist_contains_str str l) then
      `Assoc (
         (str, to_list_content_str str (`List (assoc_elem_filter_snd l str)))
         ::(assoc_elem_filternot (map_snd (to_list_content_str str) l) str)
      )
    else 
      `Assoc (map_snd (to_list_content_str str) l)
  | `List (hd::tl) -> `List ((to_list_content_str str hd)::(List.map (to_list_content_str str) tl))
  | x -> x

let rec prune_null_assoc json =
  match json with
  | `Assoc ((t, `Assoc([]))::tl) -> prune_null_assoc (`Assoc tl)
  | `Assoc ((t, `Null)::tl) -> prune_null_assoc (`Assoc tl)
  | `Assoc ((t, j)::tl) -> `Assoc ((t, (prune_null_assoc j))::(map_snd prune_null_assoc tl))
  | `List (`Null::[]) -> `Null
  | `List (l) -> `List (List.map prune_null_assoc l)
  | x -> x

(*
Value printers
*)
let rec print_depth json depth indent =
  if (depth > 0) then
    match json with
    | `Assoc ((t,j)::tl) -> 
      (indent^t)::(List.append (print_depth j (depth-1) (indent^"  "))
                               (print_depth (`Assoc (tl)) depth indent))
    | `List (hd::tl) ->
      List.append (print_depth hd depth indent)
                  (print_depth (`List (tl)) depth indent)
    | `String (s) -> (indent^s)::[]
    | _ -> []
  else
    []

let rec flatten_ivd json =
  match json with
  | `Assoc ((t, `List (l))::[]) -> if (String.equal t "INTERFACE_VARIABLE_DECLARATION") then
      `List (List.map flatten_ivd l) else `Assoc ((t, flatten_ivd (`List(l)))::[])
  | `Assoc (l) -> `Assoc (map_snd flatten_ivd l)
  | `List (hd::tl) -> `List((flatten_ivd hd)::(List.map flatten_ivd tl))
  | x -> x

(*
let do_stuff json =
  match json with
  | `Assoc ((t,j)::tl) -> 
  | `List (hd::tl) ->
  | `String (s) ->
  | _ -> x
*)
