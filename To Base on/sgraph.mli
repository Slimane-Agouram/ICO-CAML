(* Graphes non-orient�s dont les noeuds contiennent des 'a *)

type ('a, 'b) t
type 'a node

(* Erreur, avec un message *)
exception Error of string

(* Cr�er un nouveau graphe *)
val create : 'a -> ('a, 'b) t

(* cr�er un nouveau sommet ajout� graphe par effet de bord *)
val new_node : ('a, 'b) t -> 'a -> 'a node

(* new_edge g n1 n2 l, ajoute un arc de n1 vers n2 �tiquett� par l *)
val new_edge : ('a, 'b) t -> 'a node -> 'a node -> 'b ->  unit

(* it�rer une fonctions sur tous les sommets du graphe *)
val iter : ('a, 'b) t -> ('a node -> unit) -> unit

(******************)
(* Lire le graphe *)
(******************)
(* liste de tous les noeuds, (ordre de creation) *)
val nodes : ('a, 'b) t -> 'a node list  
(* contenu d'un sommet *)
val info : ('a, 'b) t -> 'a node -> 'a           
(* voisins d'un sommet *)
val adj : ('a, 'b) t -> 'a node -> 'b -> 'a node list 
