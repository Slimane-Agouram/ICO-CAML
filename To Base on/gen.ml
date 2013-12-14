let last_label = ref (-1)
let last_temp = ref (-1)

type label = {id : int; name : string}
type temp = int * bool

let new_temp() = incr last_temp; (!last_temp, false)
let ephemere () = incr last_temp; (!last_temp, true)
let is_ephemere (_,b) = b

let registers = Array.init 100 (fun i -> new_temp());;

let (limit,_) = new_temp ()

let is_temp (i,_) = i >= limit

let new_label() =
  incr last_label;
  {id = !last_label; name = "L"^(string_of_int !last_label)}

let names = Hashtbl.create 3
let prefixed_label s =
  let n, sn = 
    try let n = succ (Hashtbl.find names s) in n, s^"_"^(string_of_int n)
    with Not_found -> 0, s in
  incr last_label;
  let l = {id = !last_label; name = sn} in
  Hashtbl.add names s n; l
;;

let named_label s =
  try let _ = Hashtbl.find names s in failwith "named_label"
  with Not_found -> prefixed_label s
  ;;
  
(* pour imprimer les instructions et pour la mise au point *)
let label_string l = l.name
let temp_int (t,_) = t

let temp_string (t,_) = "t"^(string_of_int t)
