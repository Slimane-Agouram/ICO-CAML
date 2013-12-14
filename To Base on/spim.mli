(*****************************)
(* Point d'entrée principal  *)
(*****************************)

type procedure = {
  frame : Frame.frame ;   (* frame de la procédure *)
  code : Ass.instr list ; (* Code proprement dit *)
  (* Instruction a enlever si la fonction n'alloue pas en pile (oublier) *)
  remove_ifzero : string list ; 
  } 


type program = {
      prelude : Ass.instr;         (* A emettre avant tout le reste *)
      main : procedure;            (* Code de main *)
      procedures : procedure list; (* Code des proce'dures *)
    } 

val program : Code.code  Trans.program -> program

(********************************************)
(* Quelques iinformations sur les registres *)
(********************************************)

val transfert_registers : Gen.temp list (* Pour le -spill *)
val registers : Gen.temp list           (* Tous les registres d'usage général *)
val special_registers : Gen.temp list
(* Affichage des registres, selon le format de l'assembleur *)
val namer : Gen.temp -> string 


(**********************************************************************)
(* Pour emettre quelques instruction de l'exterieur (durant le spill) *)
(**********************************************************************)

(* load_sp t o t' recupe`re dans t le contenu de l'offset o en pile *)
val load_sp : Gen.temp -> int -> Gen.temp -> Ass.instr
(* save_sp t o t' sauve le contenu de t a` l'offset o en pile *)
val save_sp : Gen.temp -> int -> Gen.temp -> Ass.instr
(*
   Dans les deux cas, t' est le nom du temporaire concerne' et sert a mettre
   un commentaire, tandis que ``t'' est le registre machine effectivement
   utlise'.
*)

(* Fabriquer un « move » *)
val move : Gen.temp -> Gen.temp -> Ass.instr

