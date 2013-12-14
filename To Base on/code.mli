open Frame

type exp = 
    Const of int                  (* Entiers et Bool�ens *)
  | Name of Gen.label             (* Globaux --cha�nes et variables *)
  | Temp of Gen.temp              (* Lecture d'un temporaire *)
  | Mem of exp                    (* Lecture m�moire *)
  | Bin of binop * exp * exp      (* Op�ration binaire *)
  | Call of frame * exp list      (* Appel de fonction ou appel syst�me *)
        
and stm = 
  | Label of Gen.label            (* �tiquette *)
  | Move_temp of Gen.temp * exp   (* �criture dans un temporaire *)
  | Move_mem of exp * exp         (* �criture en m�moire *)
  | Seq of stm list               (* Expression �valuer pour son effet *)
  | Exp of exp                    (* Expression �valuer pour son effet *)
  | Jump of Gen.label             (* Saut non conditionnel *)
  | Cjump of                      (* Saut conditionnel *)
      relop * exp * exp *
        Gen.label * Gen.label

and relop = Req | Rne | Rle | Rge | Rlt | Rgt
and binop =
| Plus | Minus | Times | Div | Lt | Le | Gt | Ge | Eq | Ne
| Uplus (* addition non-sign�e *)


type code = stm list
