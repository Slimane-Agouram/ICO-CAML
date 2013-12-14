open Code

(* commutation des expressions *)

(* une astuce: on représente la lecture ou écriture en mémoire comme la lecture ou l'écriture dans un registre particulier *)
let memory = Gen.new_temp();;

let rec lus_dans_exp = function
  | Const _ -> []
  | Name _ -> []
  | Bin (_, e1, e2) -> lus_dans_exp e1 @ lus_dans_exp e2
  | Mem e -> memory :: lus_dans_exp e
  | Temp t -> [t]
(* Cas impossible *)
  | Call (_, _) -> assert false

let rec écrits_dans_stm = function
  | [] -> []
  | h :: rest ->
      match h with 
      | Label _ -> écrits_dans_stm rest
      | Move_temp (t, Call (_,_)) -> t :: memory :: écrits_dans_stm rest
      | Move_temp (t, e) -> t :: écrits_dans_stm rest
      | Move_mem (_, e) -> memory :: écrits_dans_stm rest
(* Cas impossibles en Pseudo-Pascal *)
      | Exp _ | Jump _ | Cjump (_,_,_,_,_) -> assert false 
(* Cas impossible : le code est linéarisé *)
      | Seq _ -> assert false

(* une bonne approximation *)
let rec commute e stm =
  (* fait l'hypothèse que e et stm sont canoniques et linéaire *)
  (*
     (* une mauvaise approximation mais presque suffisante *)
     stm = [] || lus_dans_exp e = []
  *)
  let r = lus_dans_exp e and w = écrits_dans_stm stm in
  List.for_all (fun x -> not (List.mem x r)) w
;;        


(* Canonisation et linéarisation d'un coup d'un seul *)
let rec rewrite_exp = function
  | Bin (binop, e1, e2) ->
      let s, e1', e2' = rewrite_two_args e1 e2 in
      s, Bin (binop, e1', e2')
  | Mem e -> 
      let s, e' = rewrite_exp e in
      s, Mem e'
  | Call (f, el) ->
      let s, el' = rewrite_args el in
      let t = Gen.new_temp() in
      s @ [ Move_temp (t, Call (f, el')) ], Temp t
  | e -> [], e

and rewrite_stm = function
  | Cjump (relop, e1, e2, l1, l2) ->
      let s, e1', e2' = rewrite_two_args e1 e2 in
      s @ [ Cjump (relop, e1', e2', l1, l2)]
  | Move_mem (e1, e2) -> 
      let s, e1', e2' = rewrite_two_args e1 e2 in
      s @ [ Move_mem (e1', e2') ]
  | Move_temp (t, e1) ->
      let s, e1' = rewrite_exp e1 in
      s @ [ Move_temp (t, e1') ]
  | Exp (Call (f, args)) ->
      let s, args' = rewrite_args args in
      s @ [Exp (Call (f,args'))]
  | Exp e -> 
      let s, e' = rewrite_exp e in
      s @ [Exp e']
  | Seq s ->
      List.concat (List.map rewrite_stm s)
  | (Jump _ | Label _) as s -> [s]

and rewrite_args args =
  let rec rewrite_args_rec = function
    [] -> [], []
  | e :: el ->
      let s_el, el' = rewrite_args_rec el in
      let s_e, e' = rewrite_exp e in
      if commute e' s_el then s_e @ s_el, e' :: el'
      else 
        let t = Gen.new_temp() in
        s_e @ (Move_temp (t, e') :: s_el), Temp t :: el' in

  rewrite_args_rec args

and rewrite_two_args e1 e2 = match rewrite_args [e1 ; e2] with
| s,[e1 ; e2] -> s, e1, e2
| _ -> assert false

                                                             
let code c = rewrite_stm c
