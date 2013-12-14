open Table

exception Error

type 'a node = int

type ('a, 'b) t_node =
   {
    info : 'a ;
    mutable succ : (int * 'b) list ;
    mutable pred : (int * 'b) list ;
   } 


type ('a, 'b) t = (('a, 'b) t_node) Table.t * (int * 'b, unit) Hashtbl.t

let create x = Table.create {info=x ; succ=[] ; pred=[]}, Hashtbl.create 17

let new_node (t,_) x =
  Table.emit_bis t {info=x ; succ=[] ; pred=[]}

let new_edge (t,e) n1 n2 l =
  try
    let key = (n1 lsl 15) + n2, l in
    if not (Hashtbl.mem e key) then begin
      Hashtbl.add e key () ;
      let t_n1 = Table.get t n1
      and t_n2 = Table.get t n2 in
      t_n1.succ <- (n2, l) :: t_n1.succ ;
      t_n2.pred <- (n1, l) :: t_n2.pred ;
    end
  with
  | Table.Error -> raise Error
  | Not_found -> ()

let rec interval i j =
  if i >= j then []
  else
    i::interval (i+1) j

let nodes (t,_) = interval 0 (Table.size t)

let info (t,_) n =
  try
    let t_n = Table.get t n in
    t_n.info
  with
  | Table.Error -> raise Error

let succ (t,_) n l = 
  try
    let t_n = Table.get t n in
    List.fold_right
      (fun (i,m) r -> if m=l then i::r else r) t_n.succ []
  with
  | Table.Error -> raise Error

let iter (t,_) f =
  for i=0 to Table.size t-1 do
    f i
  done


let debug chan f ((t,_) as g) =
  for i=0 to Table.size t-1 do
    Printf.fprintf chan "%d ->" i ;
    begin try
      let node = Table.get t i in
      List.iter (fun (i,_) -> Printf.fprintf chan " %d," i) node.succ ;
    with Table.Error -> assert false
    end ;
    output_char chan '\n' ;
    f chan i
  done

  
