open Basetypes 

(* Module to manipulate set of active states.

   It relies on sets of path to represent active ones.
*)

(*******************************)
module Vars =
struct
  include Set.Make (struct type t = path_t let compare = compare end)

  let pp_set fmt rho =
    Format.fprintf fmt "@[<v 0>%a@ @]"
      (Utils.fprintf_list ~sep:"@ "
	 (fun fmt p -> Format.fprintf fmt "%a" pp_path p))
      (elements rho)
end

module Env =
struct
  include Map.Make (struct type t = path_t let compare = compare end)

  let from_set s default =
    Vars.fold (fun e m -> add e default m ) s empty
      
  let find a b =
    try 
      find a b
    with Not_found -> (
      Format.printf "Looking for %a@." pp_path a ;
      raise Not_found
    )
  let keys a =
    fold (fun key _ -> Vars.add key) a Vars.empty

  let pp_env fmt rho =
    Format.fprintf fmt "@[<v 0>%a@ @]"
      (Utils.fprintf_list ~sep:"@ "
	 (fun fmt (p,b) -> Format.fprintf fmt "%a -> %b" pp_path p b))
      (bindings rho)
end


