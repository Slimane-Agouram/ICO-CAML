(* Ce module décrie les différents types instructions assembleurs *)

type temp = Gen.temp
type label = Gen.label

type instr =
  | Oper of string * temp list * temp list * label list option
  | Move of string * temp * temp
  | Label of string * label 

(*
   La convention est la suivante, en notant 
   - instr: la chaîne de caractère codant l'instruction
   - src: le ou les registres lus et
   - dest: le ou les registres écrits
   - sauts: la liste des étiquette à laquelle l'instruction peut sauter 
   
           Oper (instr, src, dest, sauts)
           Move (instr, src, dest)
           Label (instr, label)
*)   


val namer : temp -> string
    (* pour immprimer un temporaire *)

exception Format of instr
    (* Exception levée lorsque qu'une instruction est mal formée. *)

val format : instr -> unit
    (* imprimer une instruction sur une ligne, sans le retour à la ligne,
       avec le namer précédent pour les temporaires, lève l'exception format
       lorsque l'instruction est mal formée *)
val nformat : (temp -> string) -> instr -> unit
    (* idem, mais avec le namer passé en argument *)
    
val print : out_channel -> (temp -> string) -> instr -> unit
    (* comme nformat, mais prend le chanal de sortie en argument *)
