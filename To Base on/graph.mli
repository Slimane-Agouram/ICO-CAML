(********************)
(* Graphes orientés *)
(********************)

(* en cas de malheur *)
exception Error of string

(* Types de sommets et des graphes *)
type 'a node
and 'a t

val create : 'a -> 'a t
(* Créer un nouveau graphe, initialement vide, les sommets contiendront l'information 'a *)

val new_node : 'a t -> 'a -> 'a node
(* « new_node g i » ajoute un noeud d'information i au graphe g par effet de bord *)

val new_edge : 'a t -> 'a node -> 'a node -> unit
(* « new_edge g n1 n2 » ajoute un arc de n1 vers n2, si il n'existe pas déjà *)

val nodes : 'a t -> 'a node list
(* Tous les noeuds, dans l'ordre de leur création *)

val info : 'a t -> 'a node -> 'a
(* Le contenu d'un noeud *)

val succ : 'a t -> 'a node -> 'a node list
(* Les successeurs d'un noeud *)

val iter : 'a t -> ('a node -> unit) -> unit
(* « iter g f » itère la fonction f sur les noeuds (ordre de création) *)

val debug : out_channel -> (out_channel -> 'a node -> unit) -> 'a t -> unit
(* Affichage pour le debug *)
