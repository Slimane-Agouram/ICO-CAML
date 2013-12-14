type 'a t

val create : 'a -> 'a t  (* Creer une table *)

val emit : 'a t -> 'a -> unit (* Ajouter un element a` la fin de la table *)

val iter : 'a t -> ('a -> unit) -> unit (* it�rer f sur tous les elts *)

(*
  Les deux ope'rations suivantes vident la table et renvoient
  une structure qui donne les e'le'ments de la table dans
  l'ordre d'emission.
*)
val trim : 'a t -> 'a array        (* Renvoie un tableau *)
val trim_to_list : 'a t -> 'a list (* Remvoie une liste *)


(*
  Serie de fonctions un peu moins abstraites
*)
exception Error

(* Comme emit, mais renvoie l'indice de la case allou� en plus *)
val emit_bis : 'a t -> 'a -> int

(* Acc�s dans la table, exception Error si la case n'existe pas *)
val get : 'a t -> int -> 'a

(* Cette fonction renvoie la liste des elements, sans vider la table *)
val to_list : 'a t -> 'a list 

(* Nombre d'�l�ments dans la table *)
val size : 'a t -> int

(* Copier une table *)
val copy : 'a t -> 'a t
