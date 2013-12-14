(*********************)
(*  Les temporaires  *)
(*********************)

type temp

(* Renvoie un nouveau temporaire banalisé *)
val new_temp : unit -> temp

(* Renvoie un nouvel ephémère *)
val ephemere : unit -> temp

(* Distingue les deux sortes de temporaires *)
val is_ephemere : temp -> bool
val is_temp  : temp -> bool

(* les temporaires spéciaux qui coïcideront avec des registres *)
val registers : temp array


(********************)
(*  Les étiquettes  *)
(********************)

type label

(* Renvoie une nouvelle étiquette *)
val new_label: unit -> label

(* Retourne une étiquette avec le nom passé en argument.
   Échoue si une étiquette de ce nom existe déjà *)
val named_label : string -> label

(* Idem, mais ajoute un suffixe si le nome existe au lieu d'echouer *)
val prefixed_label : string -> label

(*********************************************************)
(* Utile pour imprimer les étiquettes et les temporaires *)
(*********************************************************)

(* Nom du temporaire *)
val temp_string : temp -> string

(* Nom du label *)
val label_string : label -> string


(*****************)
(* Truc étrange  *)
(*****************)

val temp_int : temp -> int



