open Pp
open Code

type access = Local of Gen.temp | Address of Code.exp

(* compilation des binop *)
let cbinop = function
  | Pp.Plus -> Plus
  | Pp.Minus -> Minus
  | Pp.Times -> Times
  | Pp.Div -> Div
  | Pp.Lt -> Lt
  | Pp.Le -> Le
  | Pp.Gt -> Gt
  | Pp.Ge -> Ge
  | Pp.Eq -> Eq
  | Pp.Ne -> Ne

(* idem pour les relop *)
exception NotRelop of string
exception Error of string

let crelop = function
  | Pp.Lt -> Rlt
  | Pp.Le -> Rle
  | Pp.Gt -> Rgt
  | Pp.Ge -> Rge
  | Pp.Eq -> Req
  | Pp.Ne -> Rne
  | Pp.Plus -> raise (NotRelop "+")
  | Pp.Minus -> raise (NotRelop "-")
  | Pp.Times -> raise (NotRelop "*")
  | Pp.Div   -> raise (NotRelop "/")

let is_relop op =
  try let _ = crelop op in true
  with NotRelop _ -> false

(* Retrouver une r-value *)
let r_value a = match a with
  | Local t -> Temp t
  | Address a -> Mem a

(* expressions *)
let rec cexpr env = function
  | Int i  -> Const i
  | Bool b -> Const (if b then 1 else 0)
  | Get s -> r_value (Env.find_var env s)
  | Pp.Bin (op,e1,e2) ->  Bin (cbinop op, cexpr env e1, cexpr env e2)
  | Function_call (f,args) ->
      Call (Env.find_definition env f,List.map (cexpr env) args)
  | Geti (e1,e2) -> Mem (cindex env e1 e2)
  | Alloc (e,_) ->
      Call
        (Frame.alloc,[cexpr env e])

(* indexation d'un tableau *)
and cindex env e1 e2 =
  Bin
    (Uplus,
     cexpr env e1,
     (match cexpr env e2 with
     | Const i2 -> Const (Frame.wordsize * i2) (* petite optimisation... *)
     | ce2 -> Bin (Times,Const Frame.wordsize,ce2)))

let store a ec = match a with
| Local r   -> Move_temp (r,ec)
| Address a -> Move_mem (a,ec)


let rec cinstruction env i = match i with
  | Set (s,e) ->  store (Env.find_var env s) (cexpr env e)
  | Sequence is ->  Seq (cinstruction_list env is)
  | If (e,inst,insf) ->
      begin match e with
        Pp.Bin (op,e1,e2) when is_relop op ->
          ccond env op e1 e2 inst insf
      | _ ->
          ccond env Pp.Ne e (Bool false) inst insf
      end
  | While (e,i) ->
      begin match e with
        Pp.Bin (op,e1,e2) when is_relop op ->
          cwhile env op e1 e2 i
      | _ ->
          cwhile env Pp.Ne e (Bool false) i
      end
  | Procedure_call (f,args) ->
      Exp (Call (Env.find_definition env f,List.map (cexpr env) args))
  | Write_int e ->
      Exp (Call (Frame.write_int, [cexpr env e]))
  | Writeln_int e ->
      Exp (Call (Frame.writeln_int, [cexpr env e]))
  | Read_int s ->      
      store (Env.find_var env s) (Call (Frame.read_int,[]))
  | Seti (e1, e2, e3) ->
    Move_mem (cindex env e1 e2,cexpr env e3)

and cinstruction_list env is = List.map (cinstruction env) is


(* compilation de la conditionnelle ``If'' *)
and ccond env op e1 e2 inst insf =
  let lt = Gen.new_label ()
  and lf = Gen.new_label ()
  and lend = Gen.new_label () in
  Seq
    [Cjump (crelop op,cexpr env e1, cexpr env e2,lt,lf) ;
     Label lf ;
     cinstruction env insf ;
     Jump lend ; Label lt ;
     cinstruction env inst ;
     Label lend]
  
(* compilation de la boucle  ``While'' *)
and cwhile env op e1 e2 i =
  let enter = Gen.new_label ()
  and test = Gen.new_label ()
  and lout = Gen.new_label () in  
  Seq
    [Jump test ;
     Label enter ;
     cinstruction env i ;
     Label test ;
     Cjump (crelop op,cexpr env e1,cexpr env e2,enter,lout) ;
     Label lout]


let make_frame (s,{arguments = args ; result = r }) =
  s,Frame.named_frame s args r
;;


let cfun env
    (s,{arguments = args ; result = r ; local_vars = locs ; body = ins}) =
  let f = Env.find_definition env s in
  let new_env =
    Env.change_local_vars
      env
      (List.fold_right2
         (fun (s,_) t r -> (s,Local t)::r)
         args (Frame.frame_args f)
      ((fun r -> match Frame.frame_result f with
      | Some t -> (s,Local t)::r
      | _      -> r)
         (List.fold_right
            (fun (s,_) r -> (s, Local (Gen.new_temp ())) :: r)
            locs []))) in
  f,
  Seq (cinstruction_list new_env ins)
;;
         

                 
type 'a procedure = Frame.frame * 'a
type 'a program =
    { number_of_globals : int;
      main : 'a procedure;
      procedures : 'a procedure list
    } 

let make_global_loc i =
  Address
  (match i with
  | 0 -> Temp Frame.global_register
  | _ -> Bin (Uplus, Temp Frame.global_register, Const (i*Frame.wordsize)))

let mapi f l =
  let rec map_rec n = function
    | [] -> []
    | x::xs ->  f n x :: map_rec (n+1) xs in
  map_rec 0 l
;;

let cprog {global_vars = g ; definitions = defs ; Pp.main = p} =
  let globals = mapi
      (fun i (x,_) -> x,make_global_loc i)
      g in
  let main_def = ("main",{arguments = [] ; result = None ; local_vars = [] ; body = p}) in

  let env_init =
    Env.create_global
      globals
      (List.map make_frame (main_def :: defs)) in

  let funs = List.map (fun def -> cfun env_init def) defs in
  let principal = cfun env_init main_def in

  { number_of_globals = List.length globals ;
    main = principal ; procedures = funs }
;;

      


let program p =
try
  cprog p
with
| Env.Free s ->
    Printf.fprintf stderr
      "Program incorrect:\n\
      la variable %s  est libre" s;
      prerr_newline(); exit 1
| NotRelop op ->
    Printf.fprintf stderr
      "Program incorrect:\n\
      l'opérateur %s  est utlisé comme une relation" op ;
      prerr_newline(); exit 1
| Error s ->
    Printf.fprintf stderr
      "Internal error: %s\n" s ;
      prerr_newline(); exit 1
    

;;
