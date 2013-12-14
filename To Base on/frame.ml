type address = Gen.temp
(* Dans un contexte plus général, les variables qui s'échappent
   seraient allouées en pile *) 
type frame = 
    { name : Gen.label;
      return_label : Gen.label;
      args : address list;
      result : address option;
      mutable locals : int;
    }

let frame start stop args return =
  { name = start;
    return_label = stop;
    args = List.map (fun x -> Gen.new_temp()) args;
    result = (match return with Some _ ->  Some (Gen.new_temp()) | _ -> None);
    locals = 0;
  }

let new_frame x = frame (Gen.new_label()) (Gen.new_label()) x;;
let named_frame name =
  let start = Gen.prefixed_label name in
  let stop = Gen.prefixed_label (name^"_end") in
  frame start stop
;;

let bidon = named_frame "***" [] None
;;

let frame_args f = f.args
let frame_name f = f.name
let frame_result f = f.result
let frame_return f = f.return_label
let frame_size f =  f.locals;;

let frame_size_label f = Gen.label_string f.name^"_f"

let stupid_label = Gen.new_label();;
let new_primitive s args return =
  let t = Gen.new_temp() in
  { name = Gen.named_label s; return_label = stupid_label;
    args = Array.to_list (Array.create args t);
    result = if return then  Some t else None;
    locals = 0
  }

(* taille du mot *)
let wordsize = 4;;

(**********************************)
(* Allocation en pile (par mots)  *)
(**********************************)

(*
  Hum, il faut impérativemant allouer d'abors le sommet de la pile,
  puis le fond
*)

(* en bas *)
let alloc_local f =
  let n = f.locals in
  f.locals <- f.locals + wordsize;
  n

(* en haut *)
let make_space_for_args f n =  
  f.locals <- max f.locals (n * wordsize);;

(* primitives *)
let write_int = new_primitive "print_int" 1 false;;
let writeln_int = new_primitive "println_int" 1 false;;
let read_int = new_primitive "read_int" 0 true;;
let alloc = new_primitive "alloc" 1 true;;

(* adresses globales *)
let global_space = Gen.named_label "Glob"
and global_register = Gen.registers.(28)
