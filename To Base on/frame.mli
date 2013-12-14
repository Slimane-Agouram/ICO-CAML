(***************************************************)
(* Le module Frame définit les conventions d'appel *)
(***************************************************)

open Pp
open Gen

(* Le type Frame.frame décrit les fonctions et procédures *)
type frame

val new_frame : var_list -> type_expr option -> frame
   (* 
      "new_frame l r "^ crée un nouveau frame pour une définition
      prenant les arguments l et retournant le résultat r.
      L'étiquette est choisi au harsard.

      Cela alloue des temporaires pour placer les arguments, et
      éventuellement le résultat. 

      Les composantes du frame produit, telle que la liste des temporaires,
      peuvent être retrouvées par les fonctions ci-dessous. 
    *)

val bidon : frame (* Frame bidon, ça peut servir *)

val named_frame : string -> var_list -> type_expr option -> frame
   (* comme new_frame mais l'étiquette choisie est un suffixe du
      premier argument *)  
val frame_name : frame -> label
   (* retourne l'étiquette choisi pour le frame *)
val frame_args : frame -> temp list
   (* retourne la liste des temporaires choisis pour recevoice les arguments *)
val frame_result : frame -> temp option
   (* retourne le temporaire choisi pour retourner le résultat *)
val frame_return : frame -> label
   (* retourne l'étiquette choisie pour l'e'pilogue (marque la fin de la 
      fonction)  *) 
val frame_size : frame -> int
    (* retourne la taille du frame (nombre de locaux) *)
val frame_size_label : frame -> string
    (* retourne le nom de la constante qui définit la taille du frame *)

val new_primitive :
    string (* nom *) -> int (* nombre args *) -> bool (* fonction? *) -> frame

(**********************************)
(* Allocation en pile (par mots)  *)
(**********************************)    
val alloc_local : frame -> int
   (* au fond du frame, renvoie la position allouée par rapport à sp *)
val make_space_for_args : frame -> int -> unit
   (* au sommet du frame *)

(**********************************)
(* Allocation statique            *)
(**********************************)
val global_space : label
  (* étiquette de la zone (sert deux fois!) *)
val global_register : temp
  (* registre qui point sur la zone *)


val write_int : frame
val writeln_int : frame
val read_int : frame
    (* frame des primitives de même nom *)
val alloc  : frame
    (* nom de la primitive d'allocation *)
val wordsize : int
    (* sans commentaire *)
