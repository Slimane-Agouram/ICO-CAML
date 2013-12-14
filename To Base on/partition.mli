(* 
   Ce module fournit des opérations efficaces sur un ensemble partitionné en
   un nombre fixe de partitions, mais dont les éléments peuvent être ajoutés
   ou déplacés  d'une partition à une autre. L'ajout, le déplacement ou la
   consultation d'un élément d'une partition sont en temps amorti constant. 
*)

type 'a t 
type 'a elem
val make : int -> 'a t array
      (* make n créer un vecteur de partitions *)
val clear : 'a t array -> unit
       (* clear s efface la partition s e *)
    
val put : 'a t -> 'a -> 'a elem
      (* create s e créer un élément e dans la partition s *)
val info : 'a elem -> 'a
       (* « info e » retourne les informations sur l'élément e *)
val eq : 'a elem -> 'a elem -> bool
    
val belong : 'a elem -> 'a t -> bool
val move : 'a elem -> 'a t -> unit
val pick : 'a t -> 'a elem option
    ;;

(* opérations en temps linéaire *)
val list : 'a t -> 'a elem list 
(* "pick_lowest cost" retourne un élément minimal pour la fonction "cost" *)
val pick_lowest : ('a elem -> 'b) -> 'a t -> 'a elem option

(* opération en temps O(n log n) *)
val sort : ('a elem -> 'a elem -> bool) -> 'a t -> unit
