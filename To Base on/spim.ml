open Misc
open Gen
open Code
open Ass


(* conventions d'appel *)
let r = Array.sub registers 0 32;;
let zero, at, v0, v1, a0, a1, a2, a3 =
  r.(0), r.(1), r.(2), r.(3), r.(4),  r.(5), r.(6), r.(7)
let t0, t1, t2, t3, t4, t5, t6, t7 =
  r.(8), r.(9), r.(10), r.(11), r.(12), r.(13), r.(14), r.(15)
let s0, s1, s2, s3, s4, s5, s6, s7 =
  r.(16), r.(17),   r.(18), r.(19), r.(20), r.(21), r.(22), r.(23)
let t8, t9, k0, k1, gp, sp, fp, ra =
  r.(24),  r.(25), r.(26), r.(27), r.(28), r.(29), r.(30), r.(31)

let _  = assert (gp = Frame.global_register)

let name_of_register = [|
  "zero"; "at"; "v0"; "v1"; "a0"; "a1"; "a2"; "a3";
  "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7";
  "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7";
  "t8"; "t9"; "k0"; "k1"; "gp"; "sp"; "fp"; "ra";
|];;

let namer r =
  try "$"^name_of_register.(Gen.temp_int r)
  with Invalid_argument _ -> "$"^Ass.namer r
;;

(* Les catégories de registres dépendent d'une option donnée au compilateur *)
let arg_registers = match !Misc.nregs with
| 3|4 ->  [a0]
| 5   ->  [a0 ; a1]
| _   ->  [a0; a1; a2; a3]

let res_registers = [v0;]

let caller_save_registers = match !Misc.nregs with
| (3|4) -> []
| 5     -> [t0]
| _ -> [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9]
      
let callee_save_registers = match !Misc.nregs with
| 3 -> [ra]
| 4 -> [ra; s0]
| 5 -> [ra]
| _ -> [ra ; s0; s1; s2; s3; s4; s5; s6; s7]

(* Pour mémoire *)
let special_registers =  [fp; gp; sp; zero]
let unused_registers = [at; v1; k0; k1;]

let transfert_registers = [t8 ; t9] 

let registers =
  List.concat
    [ caller_save_registers; arg_registers; res_registers;
      callee_save_registers;
    ];;

(* Pour mémoire *)



let call_trash =
  ra ::
  List.concat [ caller_save_registers; arg_registers; res_registers;  ]

let anti_trash =
  List.concat [ List.tl callee_save_registers ]

let trash f =
  (* attention : les primitives utilisent des  v0 ou a0 en interne! *)
  if f = Frame.writeln_int || f = Frame.alloc then [ra; a0; v0]
  else if f = Frame.write_int || f = Frame.read_int then [ra; v0]
  else call_trash 

(* instruction qui ne fait rien *)
let nop = Oper ("nop",[],[],None)

(*
    Le code est emis a` l'aide d'un tableau qui augmente tout seul
   (cf. table.ml)
*)

let table = Table.create nop

let emit ins = Table.emit table ins

(* *)
let seize_bits i = -(1 lsl 15) <= i && i < (1 lsl 15)

(* Les mnemoniques en fonction des operateurs *)
let memo_of_op = function
  | Plus -> "add "
  | Uplus -> "addu"
  | Minus -> "sub "
  | Times -> "mul "
  | Div -> "div "
  | Lt -> "slt "
  | Le -> "sle "
  | Gt -> "sgt "
  | Ge -> "sge "
  | Eq -> "seq "
  | Ne -> "sne "

(* Un truc sur la commutativite' *)
exception No_commute

let do_commute = function
  | Uplus -> Uplus
  | Plus -> Plus
  | Times -> Times
  | Lt -> Gt
  | Le -> Ge
  | Gt -> Lt
  | Ge -> Le
  | Eq -> Eq
  | Ne -> Ne
  | _  -> raise No_commute

let is_commute op =
  try let _ = do_commute op in true with No_commute -> false

(* Emission de quelques instructions particulie`res *)
let emit_move d s =
  if d <> s then
    emit (Move ("move ^d0, ^s0", s, d))

(* Cas particulier des ope'aration avec deuxie`me argument constant *)
let emit_op2 op d s i =
  emit (Oper (memo_of_op op^" ^d0, ^s0, "^string_of_int i, [s], [d], None))

let emit_op3 op d s0 s1 =
  emit (Oper (memo_of_op op^" ^d0, ^s0, ^s1", [s0 ; s1], [d], None))

let emit_sll d s i =
  emit
    (Oper ("sll  ^d0, ^s0, "^string_of_int i, [s], [d], None))

let emit_sra d s i =
  emit
    (Oper ("sra  ^d0, ^s0, "^string_of_int i, [s], [d], None))

let emit_neg d s =
  emit
    (Oper ("sub ^d0, $0, ^s0", [s], [d], None))

let emit_add d s1 s2 =
  emit
    (Oper ("add ^d0, ^s0, ^s1", [s1 ; s2], [d], None))

let emit_store_sp_offset i s0 = 
    let offset = string_of_int i in
    emit (Oper ("sw   ^s0, "^offset^"($sp)", [s0], [], None))

let emit_store_fp_offset frame_size i s0 = 
  let sp_offset = (string_of_int i) ^ "+" ^ frame_size in
  emit (Oper ("sw   ^s0, "^sp_offset ^"($sp)", [s0], [], None))

let emit_load_fp_offset frame_size i d0 = 
  let sp_offset = (string_of_int i) ^ "+" ^ frame_size in
  emit (Oper ("lw   ^d0, "^sp_offset ^"($sp)", [], [d0], None))

let emit_label l =   emit (Label (Gen.label_string l^":",l))


(*******************)
(* Les expressions *)
(*******************)

(*
   emit_exp dest e 
     - dest est une option de temporaire
     - e est une expression
     * Le code est émis et un temporaire est renvoyé

NB:
 L'argument « dest » suggère un temporaire pour renvoyer le résultat,
 cela permet aux tuiles de emit_exp de servir pour l'instruction
 Move_temp (d, e)  sans cela, et à moins de dupliquer les motifs de emit_exp
 on aurait systématiquement un temporaire et un move en plus
 (pas trop grave en fait).
*)

let check_dest = function
  | Some r -> r
  | None   -> new_temp ()

let rec emit_exp dest = function
  | Temp t -> t
  | Name l ->
      let d = check_dest dest in
      emit (Oper ("la   ^d0, " ^label_string l, [], [d], None)) ; d
  | Const 0 -> zero (* optimisation tres mipsienne *)
  | Const n ->
      let d = check_dest dest and  si = string_of_int n in
      emit (Oper ("li   ^d0, " ^si, [], [d], None)) ; d
  | Mem e   -> 
      let d = check_dest dest and s,i = emit_addr e in      
      emit (Oper ("lw   ^d0, "^string_of_int i^"(^s0)", [s], [d], None)) ; d
  | Bin (op, Const i,e2) when is_commute op ->
      emit_bin dest (do_commute op) e2 (Const i)
  | Bin (op,e1,e2) -> emit_bin dest op e1 e2
  | Call (_, _)    -> assert false (* expressions canoniques *)

(* emit_addr renvoie un couple temporaire * offset
   raison : factoriser l'adressage indirect indexé du MIPS *)

and emit_addr  e = match e with
(* seuls cas intéressants à repérer *)
| Bin (Uplus, e1, Const i) when seize_bits i -> emit_exp None e1,i
| Bin (Uplus, Const i, e2) when seize_bits i -> emit_exp None e2,i
(* cas général *)
| _      -> emit_exp None e, 0

and emit_bin dest op e1 = function
  | Const i ->
      let s = emit_exp None e1 in
      let d = check_dest dest in
(* selection ad-hoc pour quelques cas importants *)
      begin match op,i with
      | Times,2 -> emit_sll d s 1
      | Times,4 -> emit_sll d s 2
      | Div, 2 -> emit_sra d s 1
      | Div, 4 -> emit_sra d s 2
      | _,_    -> emit_op2 op d s i
      end ;
      d
  | e2 ->
      let s0 = emit_exp None e1
      and s1 = emit_exp None e2
      and d = check_dest dest in
      emit_op3 op d s0 s1 ; d

(************************************)
(* Les fonctions, pas mal de boulot *)
(************************************)

let rec emit_args caller_frame args =

  let rec emit_args_rec args_regs args = match args_regs, args with
  | _,[] -> []
  | d::ds, arg::args ->
      let s = emit_exp (Some d) arg in
      emit_move d s ;
      d :: emit_args_rec ds args
  | [],_ ->
      let rec to_stack i = function
        | [] -> Frame.make_space_for_args caller_frame i
        | e :: es ->
            let s = emit_exp None e in
            emit_store_sp_offset (Frame.wordsize * i) s ;
            to_stack (i+1) es in
      to_stack 0 args ; [] in
  emit_args_rec arg_registers args

let emit_call frame f args =
  let arg_reg = emit_args frame args in
  let lab =  Gen.label_string (Frame.frame_name f) in
  emit (Oper ("jal  "^lab, arg_reg, trash f, None));
  begin match Frame.frame_result f with
  | Some _ -> v0
  | None   -> zero
  end

(********************)
(* Les instructions *)
(********************)

let branch_of_relop = function
  | Req -> "beq"
  | Rne -> "bne"
  | Rle -> "ble"
  | Rge -> "bge"
  | Rlt -> "blt"
  | Rgt -> "bgt"


let rec emit_stm frame = function
  | Code.Label l -> emit_label l
  | Cjump (op, e0, e1, l1, l2) ->
      let s0 = emit_exp None e0 in
      let s1 = emit_exp None e1 in
      let l = Gen.label_string l1 in
      emit
        (Oper
           (branch_of_relop op^"  ^s0, ^s1, "^l, [s0 ; s1], [], Some [l1 ; l2]))
  | Move_temp (d, Call (f,args)) ->
      let r = emit_call frame f args in
      emit_move d r
  | Move_temp (d, e) ->
      let s = emit_exp (Some d) e in
      emit_move d s
  | Move_mem (ed,es) ->
      let s0 = emit_exp None es in
      let s1,i = emit_addr ed in
      emit (Oper ("sw   ^s0, "^string_of_int i^"(^s1)", [s0 ; s1], [], None))
  | Exp (Call (f,args)) ->
      let _ = emit_call frame f args in
      ()
  | Exp e ->
      let _ = emit_exp None e in
      ()
  | Jump l ->
      emit (Oper ("b    "^Gen.label_string l, [], [], Some [l]))
  | Seq _ -> failwith "Code not linearized in Spim.emit_stm"



let emit_prolog f saved_callee_save decr_sp =
  emit_label (Frame.frame_name f) ;
  emit (Oper (decr_sp, [], [], None)) ;
  let rec save_sregs ss ds = match ss,ds with
  | [],[] -> ()
  | s::ss, d::ds ->
     emit_move d s ;
     save_sregs ss ds
  | _,_ -> assert false in

  save_sregs callee_save_registers saved_callee_save ;

  let rec copy_args ss ds = match ss,ds with
  | _, [] -> ()
  | s :: ss, d :: ds ->
      emit_move d s ;
      copy_args ss ds
  | [],ds  ->
      let frame_size = Frame.frame_size_label f in
      let rec from_stack i = function
        | [] -> ()
        | arg :: args ->
            emit_load_fp_offset frame_size (Frame.wordsize * i) arg ;
            from_stack (i+1) args in
      from_stack 0 ds in

  copy_args arg_registers (Frame.frame_args f)
;;

let emit_postlog f saved_regs incr_sp =
  let rec restore_regs saved restore = match saved, restore with
    [],[] -> ()
  | s :: saved, d :: restore ->
      emit_move d s ;
      restore_regs saved restore
  | _,_    -> failwith "restore_regs" in

  emit_label  (Frame.frame_return f) ;
  begin match (Frame.frame_result f) with
  | None -> ()
  | Some s -> emit_move v0 s
  end ;
  restore_regs saved_regs callee_save_registers ;
  let survivants =
    match (Frame.frame_result f) with
    | None -> anti_trash
    | Some _ -> v0::anti_trash in
  emit (Oper (incr_sp, [], [], None)) ;
  emit (Oper ("j    ^s0", ra::survivants, [], Some []))



type procedure = {
  frame : Frame.frame ;   (* Voir frame.mli *)
  code : Ass.instr list ; (* Code proprement dit *)
  remove_ifzero : string list ; 
  } 

let emit_fun f is =
(* temporaires pour sauver les callee_save *)
  let saved_callee_save =
    List.map (fun _ -> Gen.new_temp()) callee_save_registers in
  let frame_size = Frame.frame_size_label f in
  let decr_sp = "subu $sp, $sp, "^frame_size
  and incr_sp = "addu $sp, $sp, "^frame_size in

(* Le prologue *)
  emit_prolog f saved_callee_save decr_sp ;
(* code proprement dit *)
  List.iter (fun i -> emit_stm f i) is ;
(* L'épilogue *)
  emit_postlog f saved_callee_save incr_sp ;

  let r = Table.trim_to_list table in
  {frame=f ;
   remove_ifzero = [incr_sp ; decr_sp] ;
   code = r}

(* Le prologue du programme et son code de lancement *)
let taille_memoire = 4000

let align_word =
  string_of_int
    (match Frame.wordsize with
      1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3 |  _ -> assert false);;

let prelude glob lab = 
  Oper (String.concat "" ["
    .data	
nl:
    .asciiz \"\\n\"
    .align "; align_word; "
allocs:
    .asciiz \"Alloc: \"
    .align "; align_word; "
Glob:
    .space "; string_of_int (glob * Frame.wordsize); "
    .align "; align_word; "
Mem:
    .space "; string_of_int (taille_memoire * Frame.wordsize); "    
    .text
    .globl __start
    .align "; align_word; "

print_int:
    li  $v0, 1
    syscall 
    j   $ra
println_int:
    li  $v0, 1
    syscall 
    la  $a0, nl
    li  $v0, 4
    syscall
    j   $ra
read_int:
    li  $v0, 5
    syscall 
    j   $ra
alloc:
    sw  $a0, ($fp)              
    sll $a0, $a0, 2
    addu $v0, $fp, 4
    addu $fp, $v0, $a0
    j   $ra

__start:
    la    $fp, Mem
    la    $gp, Glob
    jal  "; Gen.label_string lab; "
    li    $v0, 10
    syscall

"], [], [], Some [])
;;



type program =
    { prelude : instr;
      main : procedure;
      procedures : procedure list;
    } 

let program p =
  let main_f,main_code = p.Trans.main in
  {
    prelude =
      prelude p.Trans.number_of_globals (Frame.frame_name main_f) ;
    main = emit_fun main_f main_code ;
    procedures =
      List.map
        (fun (f,code) -> emit_fun f code)
        p.Trans.procedures
   } 

let load_sp u i u' =
  Ass.Oper ("lw   ^d0, "^(string_of_int i)^"($sp) # load  "^namer u,
              [], [u'], None)

let save_sp u i u' =
  Ass.Oper ("sw   ^s0, "^(string_of_int i)^"($sp) # store "^namer u,
            [u'], [], None)

let move s d = Move ("move ^d0, ^s0",s,d)
