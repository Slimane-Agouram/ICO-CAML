(* spill_fun temps proc
   renvoie une proce'dure modifie'e, ou` les tempraires de temps
   sont alloue's en me'moire
*)
   
val spill_fun : Ass.temp Smallset.set -> Spim.procedure -> Spim.procedure

(* Allouer les temporaires d'un programme en me'moire,
   le code rendu est exe'cutable *)
val spill_all : Spim.program -> Ass.instr list
    
