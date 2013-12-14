open Gen
open Ass
open Smallset

(*******************************************************)
(* Code, avec informations de liveness suffisantes     *)
(*******************************************************)
type flowinstr =
 {finstr:Ass.instr ; fdef: Gen.temp set ; fout : Gen.temp set}

val flow : instr list -> flowinstr list



(*************************)
(* Graphe d'interférence *)
(*************************)

(* Sortes d'arcs du graphe d'interférence *)
type ilab = Inter | Move_related

(* Contenu des noeud du graphe d'interférence *)

type interference = {
    temp : temp;                  (* un temporaire *)
(* Les champs suivants sont utiles pour l'allocation de registres *)
    mutable color : temp option ;
    mutable occurs : int ;
    mutable degree : int ;
    mutable elem : ((interference Sgraph.node) Partition.elem) option ;
  }

type igraph = (interference, ilab) Sgraph.t

val interference :  flowinstr list  -> igraph

(* Divers imprimeurs des graphes *)
val print_flowcode : out_channel -> flowinstr list -> unit
val print_interference : out_channel ->  igraph -> unit
val print_moves :  out_channel ->  igraph -> unit
