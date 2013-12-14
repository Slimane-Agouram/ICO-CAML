open Gen
open Code
open Ass
open Spim
open Smallset

(***************)
(* Utilitaires *)
(***************)
let filter_set p s = of_list (List.filter p (to_list s))

let fold_set f r s = List.fold_left f r (to_list s)

(*
   type rendu apr`es ve'rification des temporaires
     No r -> ne pas spiller r
     Yes (t, o, r) ->
           r est en me'moire, a` l'offset de pile o,
           le registre t est utilisable pour le transfert
*)
type spilled =
  No of temp | Yes of temp * int * temp


(* Recupe'rer le(s) registre(s) *)
let real_reg = function
  | No r -> r
  | Yes (t,_,_) -> t

let real_regs xs = List.map real_reg xs

(* Acceder a` un argument *)
let get_arg x k = match x with
| No _ -> k
| Yes (t,o,r) -> load_sp r o t::k
let get_args args k = List.fold_right get_arg args k

(* Ranger un re'sultat *)
let put_dest x k = match x with
| No _ -> k
| Yes (t,o,r) -> save_sp r o t::k

let put_dests dests k = List.fold_right put_dest dests k

let mapi f l =
  let rec map_rec i = function
    | [] -> []
    | x::l -> f i x::map_rec (i+1) l in
  map_rec 0 l

(*
   Re'crit le code d'une fonction en spillant ``to_spill''
   transfert est une fonction qui rend un registre utlisable durant
   le transfert vers/de la me'moire
*)


let real_spill_fun transfert to_spill {frame=f ; code=code} =

  (* Une table temporaire -> offset en pile *)

    let reg2off = Hashtbl.create 17 in
    Smallset.iter to_spill
      (fun t ->
        let off = Frame.alloc_local f in
        Hashtbl.add reg2off t off) ;

  (* Ve'rifier si un (des) registres est (sont) a` spiller *)
    let spill_reg i r =
      try
        let off =  Hashtbl.find reg2off r in        
        Yes (transfert i, off,r)
      with
      | Not_found -> No r in

    let spill_regs regs = mapi (spill_reg) regs in

(*
  Un spill_regs plus malin qui evite de chercher deux fois les
  me^mes temporaires spille's (bof) *)

    let spill_regs_nodups =
      let rec spill_rec i seen = function
        | [] -> []
        | r::rest ->
            let here =
              try
                let r_assoc = List.assoc r seen in
                No (r_assoc)
              with
              | Not_found -> spill_reg i r in
            let rest =
              spill_rec (i+1)
                (match here with
                | Yes (t,_,_) -> (r,t)::seen
                | _ -> seen)
                rest in
            here::rest in
      spill_rec 0 [] in

  (* Effectivement re'e'crire le code *)      
    let spill_move src dest =
      let tsrc  = spill_reg 0 src
      and tdest = spill_reg 0 dest in
          begin match tsrc,tdest with
          | Yes (t,os,rs),Yes (_,od,rd) ->
              load_sp rs os t::
              save_sp rd od t::
              []
          | No r,Yes (_,od,rd) ->
              save_sp rd od r::
              []
          | Yes (_,os,rs),No r ->
              load_sp rs os r::
              []
          | No rs, No rd ->
              if rs = rd then []
              else
                move rs rd::
                []
          end in

    let rec spill_code = function
      | Oper (s, src, dest, sauts)::code ->
          let tsrc = spill_regs_nodups src
          and tdest = spill_regs dest in
          get_args tsrc
            (Oper (s,real_regs tsrc, real_regs tdest,sauts)::
             put_dests tdest
               (spill_code code))
      | Move (s, src, dest)::code -> spill_move src dest@spill_code code
      | i::code -> i::spill_code code
      | [] -> [] in

    let new_code = spill_code code in
    new_code

(* Extraire tous les temporaires d'un code *)
let rec all_regs = function
  | [] -> empty
  | Oper (_,src,dest,_)::code ->
      union (of_list src) (union (of_list dest) (all_regs code))
  | Move (_,src,dest)::code ->
      union (of_list [src]) (union (of_list [dest]) (all_regs code))
  | _::code -> all_regs code


(*************)
(* Interface *)
(*************)


let spill_fun to_spill ({code=code} as f) =
  let code =
    real_spill_fun
      (fun _ -> Gen.ephemere ())
      to_spill
      f in
  {f with code = code}
    




(* Registres de transfert fixe's *)
let transfert_all pos =
  try List.nth Spim.transfert_registers pos with
  | _ ->  failwith "Cannot spill"

(* Effectivement spiller tous les temporaires d'un code sauf
   les registres machine... *)
let but = of_list (Spim.registers @ Spim.special_registers)

let spill_all_fun but ({frame=frame ; code=code} as f)  =
  let to_spill = diff (all_regs code) but in
  let code = real_spill_fun transfert_all to_spill f in
  Oper
    (Frame.frame_size_label frame^"="
     ^string_of_int (Frame.frame_size frame),
     [],[],None)::code


let spill_all {prelude = prelude ; main = main ; procedures = procedures} =
  prelude::
  List.fold_right
    (fun ({frame=frame} as f) k ->
      Oper
        ("\n# FUNSTART: "^Gen.label_string (Frame.frame_name frame),
         [],[],None)::
      (spill_all_fun but f@k))
    procedures
    (Oper ("\n# FUNSTART: main",[],[],None)::
     spill_all_fun but main)



