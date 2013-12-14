open Frame

type exp = 
    Const of int                  (* Entiers et Booléens *)
  | Name of Gen.label             (* Globaux --chaînes et variables *)
  | Temp of Gen.temp              (* Lecture d'un temporaire *)
  | Mem of exp                    (* Lecture mémoire *)
  | Bin of binop * exp * exp      (* Opération binaire *)
  | Call of frame * exp list      (* Appel de fonction ou appel système *)
        
and stm = 
  | Label of Gen.label            (* Étiquette *)
  | Move_temp of Gen.temp * exp   (* Écriture dans un temporaire *)
  | Move_mem of exp * exp         (* Écriture en mémoire *)
  | Seq of stm list               (* Expression évaluer pour son effet *)
  | Exp of exp                    (* Expression évaluer pour son effet *)
  | Jump of Gen.label             (* Saut non conditionnel *)
  | Cjump of                      (* Saut conditionnel *)
      relop * exp * exp *
        Gen.label * Gen.label

and relop = Req | Rne | Rle | Rge | Rlt | Rgt
and binop =
| Plus | Minus | Times | Div | Lt | Le | Gt | Ge | Eq | Ne
| Uplus (* addition non-signée *)


type code = stm list
