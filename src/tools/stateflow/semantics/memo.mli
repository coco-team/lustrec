
(* Le type abstrait des tables utilisées dans la memoization des fonctions. *)
(* Permet de mémoriser des appels de fonctions efficacement sous la forme   *)
(* de couples (argument de type 'a, résultat de type 'b).                   *)
(* Chaque table devra être associée à une fonction unique.                  *)
type ('a, 'b) t;;


(* Création d'une table vide.                                               *)
(* Paramètres :                                                             *)
(* - aucun                                                                  *)
(* Résultat :                                                               *)
(* - une table vide destinée à mémoriser uniquement les appels              *)
(*   d'une fonction quelconque.                                             *)
val create : unit -> ('a, 'b) t;;

val reset : ('a, 'b) t -> unit;;

(* Utilisation d'une version "memoizée" d'une fonction à un paramètre.      *)
(* Paramètres :                                                             *)
(* - une table t de mémoization : ('a, 'b) t                                *)
(* - une fonction f à mémoizer : 'a -> 'b                                   *)
(* Précondition :                                                           *)
(* - le paramètre f doit être l'identificateur d'une fonction définie       *)
(*   auparavant. Les fonctions "dynamiques" (fun x -> ...) sont interdites. *)
(* Résultat :                                                               *)
(* - une fonction au contrat identique à celui de la fonction f,            *)
(*   mais dont la complexité en temps est quasi-linéaire, au prix d'une     *)
(*   occupation mémoire importante néanmoins.                               *)
(* Erreur :                                                                 *)
(* - exception Failure levée en cas d'utilisation d'une même table          *)
(*   avec plusieurs fonctions différentes.                                  *)
val apply : ('a, 'b) t -> ('a -> 'b) -> ('a -> 'b);;

(* Utilisation d'une version "memoizée" d'une fonction à deux paramètres.   *)
(* Paramètres :                                                             *)
(* - une table t de mémoization : ('a * 'b, 'c) t                           *)
(* - une fonction f à mémoizer : 'a -> 'b -> 'c                             *)
(* Résultat :                                                               *)
(* - une fonction au contrat identique à celui de la fonction f,            *)
(*   mais dont la complexité en temps est quasi-linéaire, au prix d'une     *)
(*   occupation mémoire importante néanmoins.                               *)
(* Erreur :                                                                 *)
(* - exception Failure levée en cas d'utilisation d'une même table          *)
(*   avec plusieurs fonctions différentes.                                  *)
val apply2 : ('a * 'b, 'c) t -> ('a -> 'b -> 'c) -> ('a -> 'b -> 'c);;

val apply3 : ('a * 'b * 'c, 'd) t -> ('a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> 'd);;

val fold : ('a, 'b) t -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c;;
