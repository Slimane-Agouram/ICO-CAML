(* Ce module d�crie les diff�rents types instructions assembleurs *)

type temp = Gen.temp
type label = Gen.label

type instr =
  | Oper of string * temp list * temp list * label list option
  | Move of string * temp * temp
  | Label of string * label 

(*
   La convention est la suivante, en notant 
   - instr: la cha�ne de caract�re codant l'instruction
   - src: le ou les registres lus et
   - dest: le ou les registres �crits
   - sauts: la liste des �tiquette � laquelle l'instruction peut sauter 
   
           Oper (instr, src, dest, sauts)
           Move (instr, src, dest)
           Label (instr, label)
*)   


val namer : temp -> string
    (* pour immprimer un temporaire *)

exception Format of instr
    (* Exception lev�e lorsque qu'une instruction est mal form�e. *)

val format : instr -> unit
    (* imprimer une instruction sur une ligne, sans le retour � la ligne,
       avec le namer pr�c�dent pour les temporaires, l�ve l'exception format
       lorsque l'instruction est mal form�e *)
val nformat : (temp -> string) -> instr -> unit
    (* idem, mais avec le namer pass� en argument *)
    
val print : out_channel -> (temp -> string) -> instr -> unit
    (* comme nformat, mais prend le chanal de sortie en argument *)
