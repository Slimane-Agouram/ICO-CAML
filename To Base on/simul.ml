open Code;;
open Gen;;
open Frame;;

type erreur =
  | Alignement of int 
  | Débordement of string | Étiquette of string
exception Erreur of erreur;;
exception Interne of string;;
exception Exit;;

let erreur x = raise (Erreur x);;
        
(* linking *)

let code = Hashtbl.create 20;;


let rec linearize = function
  | Seq l :: code -> linearize (l @ code)
  | instr :: code -> instr :: linearize code
  | [] -> []
;;
    
let rec load = function
    [] -> ()
  | h :: t ->
      begin match h with
        Code.Label lab  -> Hashtbl.add code lab t
      | Seq c -> load c
      | _ -> ()
      end;
      load t
;;

let frames = Hashtbl.create 17
open Smallset

let rec regs_exp = function
  | Temp t -> add t empty
  | Mem e -> regs_exp e
  | Bin (_,e1,e2) -> union (regs_exp e1) (regs_exp e2)
  | Call (_,es) -> union_list (List.map regs_exp es)
  | Const _ | Name _ -> empty

let rec regs_stmt = function
  | Label _ -> empty
  | Move_temp (r,e) -> add r (regs_exp e)
  | Move_mem (e1,e2) ->  union (regs_exp e1) (regs_exp e2)
  | Seq l ->
       union_list (List.map regs_stmt l)
  | Exp e -> regs_exp e
  | Jump _ -> empty
  | Cjump (_,e1,e2,_,_) ->  union (regs_exp e1) (regs_exp e2)
  
let set_frame f c =
  Hashtbl.add frames
    (Frame.frame_name f)
    (to_list (union_list (List.map regs_stmt c)))

and get_frame f =
  try Hashtbl.find frames (Frame.frame_name f) with
  | Not_found ->
      raise (Interne ("Pas de frame pour: "^
                      Gen.label_string (Frame.frame_name f)))
(* runtime *)

let mem = Array.create 10000 (0);;
let next_mem = ref 0;;
let mem_alloc size =
  let address = !next_mem in next_mem := !next_mem + size;
  wordsize * address;;
let initial_registers = Array.create 255 (0);;
let reg = ref initial_registers;;

let realloc t =
  let l = Array.length !t in
  let new_t = Array.create (l*2+1) !t.(0) in
  Array.blit !t 0 new_t 0 l ;
  t := new_t

let get s t n =
  try t.(n)
  with Invalid_argument _ ->
    erreur (Débordement s);;
let set s t n v = 
  try t.(n) <- v;()
  with Invalid_argument _ ->
    erreur (Débordement s);;

let rec get_temp t =  get "Temp" !reg (temp_int t)

let rec set_temp t v =
  try
    set "Temp" !reg (temp_int t) v
  with
  | Erreur (Débordement _) ->
      realloc reg ;
      set_temp t v

let get_mem a  =
  if a mod wordsize <> 0 then erreur (Alignement a)
  else get "Mem" mem (a / wordsize);;
let set_mem a v=
  if a mod wordsize <> 0 then erreur (Alignement a)
  else set "Mem" mem (a/wordsize) v;;

let new_reg f =
  let to_save = get_frame f in
  List.map (fun r -> r, get_temp r) to_save

and restore_reg to_restore =
  List.iter
    (fun (r,v) -> set_temp r v)
    to_restore


(* primitives *)

let bin b x y = 
  match b with
  | Minus -> x - y
  | Plus|Uplus -> x + y 
  | Times -> x * y 
  | Div -> x / y
  | Gt -> if x > y then 1 else 0
  | Ge -> if x >= y then 1 else 0
  | Lt -> if x < y then 1 else 0
  | Le -> if x <= y then 1 else 0
  | Eq -> if x = y then 1 else 0
  | Ne -> if x <> y then 1 else 0
        ;;

let branch b x y t f = 
  if 
    begin match b with
    | Req -> x =  y  
    | Rne -> x <> y 
    | Rle -> x <= y 
    | Rge -> x >= y 
    | Rlt -> x <  y  
    | Rgt -> x >  y  
    end
  then t else f
      ;;

exception Goto of label;;
exception Exit;;


let rec exp  = function
    Const n -> n
  | Name l when l = global_space -> 0
  | Name _ -> failwith "Name"
  | Temp t -> get_temp t
  | Mem e -> get_mem  (exp e)
  | Bin (binop, e1,e2) ->
      let v1 = exp e1 in let v2 = exp e2
      (* necessaire pour forcer l'ordre d'evaluation gauche-droite *)
      in bin binop v1 v2
  | Call (f, []) when f = read_int -> Pervasives.read_int()
  | Call (f, [e1]) when f = write_int ->
      print_int (exp e1); 0
  | Call (f, [e1]) when f = writeln_int ->
      print_int (exp e1); print_newline(); 0
  | Call (f, [e1]) when f = alloc ->
      mem_alloc (exp e1)
  | Call (f, args) ->
      let args = List.map exp args in
      let saved_reg = new_reg f in
      List.iter2 (fun v r -> set_temp r v) args (Frame.frame_args f);
      goto (Frame.frame_return f) (Frame.frame_name f);
      let v = match Frame.frame_result f with Some r -> get_temp r | _ -> 0 in
      restore_reg saved_reg;
      v
        
and stm return l = match l with
  | [] -> ()
  | Label _ :: next  -> stm return next
  | Move_temp (t, e) :: next ->
      let v = exp e in set_temp t  v; stm return next
  | Move_mem (e1, e2) :: next ->
      let v1 = exp e1 in let v2 = exp e2 in
      set_mem v1 v2; stm return next
  | Exp e :: next -> let _ = exp e in stm return next
  | Jump l :: _ -> goto return l
  | Cjump (cmp, e1, e2, l1, l2) :: _ ->
      let v1 = exp e1 in let v2 = exp e2 in
      (* necessaire pour forcer l'ordre d'evaluation gauche-droite *)
      goto return (branch cmp v1 v2 l1 l2)
  | Seq _ ::_ -> raise (Interne "sequence in linearized code")
        
and goto return l =
  if return=l then ()
  else
    let code = 
      try Hashtbl.find code l
      with Not_found -> erreur (Étiquette (label_string l))
    in stm return code


let reset() =
  next_mem := 0;
  Hashtbl.clear code;;
  Hashtbl.clear frames;;

let program p =
  try
    reset();
    let load_fun (f,c) =
      set_frame f c ;
      load (Label (Frame.frame_name f)::linearize c) in
    load_fun p.Trans.main ;
    List.iter load_fun  p.Trans.procedures;
    let _ = mem_alloc p.Trans.number_of_globals in
    let main_f,_ = p.Trans.main in
    goto (Frame.frame_return main_f) (Frame.frame_name main_f);
  with
  | Erreur x ->
      Printf.fprintf stderr "Simulation error\n";
      begin match x with
      | Alignement n -> Printf.fprintf stderr "Address is not aligned %d" n
      | Débordement s -> Printf.fprintf stderr "Débordement: %s" s
      | Étiquette s -> Printf.fprintf stderr "Undefined label: %s" s
      end;
      prerr_newline()
  | Env.Free s ->
      Printf.fprintf stderr "Erreur Simulation erreur\n";
      
        
  | Failure "int_of_string" ->
      Printf.fprintf stderr
        "Erreur de saisie: chaîne non convertible en un entier";
      print_newline()
  | Interne s ->
      Printf.fprintf stderr "Simulation erreur"; print_newline()
  | Exit -> ()
;;

