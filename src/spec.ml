open Lustre_types
(*      TODO:
	- verifier que les spec sont quantifiers free ou sinon mettre un warning
      - rajouter les expressions requires => ensures dans le node
    - sauver le nom des variables locales qui encodent ces specs.
*)

let enforce_spec_node nd = 
(* TODO: add asserts for quantifier free normalized eexpr  *)
  nd
  
let enforce_spec_prog prog =
  List.map (
    fun top -> match top.top_decl_desc with
    | Node nd -> {top with top_decl_desc = Node (enforce_spec_node nd) }
    | _ -> top
  ) prog

