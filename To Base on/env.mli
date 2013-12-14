type ('a, 'b) environment 
(* type des environnement qui associe aux variables des valeurs de type 'a et
   aux fonctions des valeurs de type 'b *)

exception Free of string
(* retourne l'identificateur recherché lorsqu'il n'est pas trouvé *)

val create_global : (string * 'a) list -> (string * 'b) list ->
  ('a,'b) environment
(* 
   "create_global v d" crée un environement avec les liaisons globales
   v  et les définitions d. Sur une telle table, find_var x retournera la
   valeur de la liaison x dans v et find_definition x la valeur de la
   définition x dans d. 
*)

val add_local_vars : ('a,'b) environment -> (string * 'a) list ->
  ('a,'b) environment
(* ajoute des liaisons locales aux liaisons globales *)

val change_local_vars : ('a,'b) environment -> (string * 'a) list ->
  ('a,'b) environment
(* remplace les liaisons locales *)

val find_var : ('a,'b) environment -> string -> 'a
val find_definition : ('a,'b) environment -> string -> 'b
(* 
   "find_var env x" recherche la valeur de x dans les liaisons locales ou
   globales de env.

   "find_definition env x" recherche la valeur de x dans les definitions
   de env. 
 *)
