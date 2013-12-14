(*
  Petite réalisation  des ensembles.

  Les ensembles sont encodés comme des listes ordonnées, selon
  l'ordre générique de Caml.

  Toutes les opérations sont linéraires en fonction du cardinal
  des ensembles passés en argument.
*)

type 'a set

val eqset : 'a set -> 'a set -> bool
val empty : 'a set
val is_empty : 'a set -> bool

val choose : 'a set -> 'a option

val singleton : 'a -> 'a set
    (* créer un singleton *)
val of_list : 'a list -> 'a set
    (* "of_list l" creates a set whose elements are l *)
val to_list : 'a set -> 'a list
    (* "to_list s" return the list of elements of set s *)
val mem : 'a -> 'a set -> bool
    (* "mem x s" tests whether x is an element of set s *)
val union : 'a set -> 'a set -> 'a set
val union_list : 'a set list -> 'a set
    (* "union_list l" returns the union of all sets in l *)
val diff : 'a set -> 'a set -> 'a set
    (* "diff p q" returns the set of elements that are in p but not in q *)
val inter : 'a set -> 'a set -> 'a set
    (* "inter p q" returns the intersection of sets p and q *)

val add : 'a -> 'a set -> 'a set
    (* "add x s" adds the element x to the set s *)
val remove : 'a -> 'a set -> 'a set
    (* "remove x s" remove the element x from the set s *)

val iter : 'a set -> ('a -> unit) -> unit
    (* "iter s f" applied f once to all elements of s (in arbitrary order) *)
