(* 
   Ce module fournit des op�rations efficaces sur un ensemble partitionn� en
   un nombre fixe de partitions, mais dont les �l�ments peuvent �tre ajout�s
   ou d�plac�s  d'une partition � une autre. L'ajout, le d�placement ou la
   consultation d'un �l�ment d'une partition sont en temps amorti constant. 
*)

type 'a t 
type 'a elem
val make : int -> 'a t array
      (* make n cr�er un vecteur de partitions *)
val clear : 'a t array -> unit
       (* clear s efface la partition s e *)
    
val put : 'a t -> 'a -> 'a elem
      (* create s e cr�er un �l�ment e dans la partition s *)
val info : 'a elem -> 'a
       (* � info e � retourne les informations sur l'�l�ment e *)
val eq : 'a elem -> 'a elem -> bool
    
val belong : 'a elem -> 'a t -> bool
val move : 'a elem -> 'a t -> unit
val pick : 'a t -> 'a elem option
    ;;

(* op�rations en temps lin�aire *)
val list : 'a t -> 'a elem list 
(* "pick_lowest cost" retourne un �l�ment minimal pour la fonction "cost" *)
val pick_lowest : ('a elem -> 'b) -> 'a t -> 'a elem option

(* op�ration en temps O(n log n) *)
val sort : ('a elem -> 'a elem -> bool) -> 'a t -> unit
