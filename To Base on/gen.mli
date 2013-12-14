(*********************)
(*  Les temporaires  *)
(*********************)

type temp

(* Renvoie un nouveau temporaire banalis� *)
val new_temp : unit -> temp

(* Renvoie un nouvel eph�m�re *)
val ephemere : unit -> temp

(* Distingue les deux sortes de temporaires *)
val is_ephemere : temp -> bool
val is_temp  : temp -> bool

(* les temporaires sp�ciaux qui co�cideront avec des registres *)
val registers : temp array


(********************)
(*  Les �tiquettes  *)
(********************)

type label

(* Renvoie une nouvelle �tiquette *)
val new_label: unit -> label

(* Retourne une �tiquette avec le nom pass� en argument.
   �choue si une �tiquette de ce nom existe d�j� *)
val named_label : string -> label

(* Idem, mais ajoute un suffixe si le nome existe au lieu d'echouer *)
val prefixed_label : string -> label

(*********************************************************)
(* Utile pour imprimer les �tiquettes et les temporaires *)
(*********************************************************)

(* Nom du temporaire *)
val temp_string : temp -> string

(* Nom du label *)
val label_string : label -> string


(*****************)
(* Truc �trange  *)
(*****************)

val temp_int : temp -> int



