type 'a set = 'a list

let rec eqset xs ys = match xs, ys with
| [],[] -> true
| x::xs, y::ys -> x=y && eqset xs ys
| _,_          -> false

let empty = []

let is_empty = function
  | [] -> true
  | _  -> false

let choose = function
  | x::_ -> Some x
  | []   -> None

let singleton x = [x]

let rec union l1 l2 =
  match l1, l2 with
  |  [], _ -> l2
  |  _, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c = 0 then union t1 l2
      else if c < 0 then h1 :: union t1 l2
      else h2 :: union l1 t2
                  
let rec union2 = function
    l1::l2::rest -> union l1 l2 :: union2 rest
  | x -> x  
        
let rec union_list = function
    [] -> []
  | [l] -> l
  | llist -> union_list (union2 llist) 
        
        
let rec list = function
    [] -> []
  | [e] -> [[e]]
  | e1:: e2::rest ->
      (let c = compare e1 e2 in
      if c = 0 then [e1]
      else if c < 0 then  [e1;e2]
      else [e2;e1])
      :: list rest
let of_list l = union_list (list l)
    
let to_list x = x
let mem s x = List.mem s x
    
let rec diff l1 l2 =
  match l1, l2 with
  |  [], _ -> []
  |  _, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c = 0 then diff t1 l2
      else if c < 0 then h1 :: diff t1 l2
      else diff l1 t2
          
let rec inter l1 l2 =
  match l1, l2 with
  |  [], _ -> []
  |  _, [] -> []
  | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c = 0 then h1 :: inter t1 t2 
      else if c < 0 then inter t1 l2
      else inter l1 t2          
          
let iter s f = List.iter f s

let add x s = union [x] s

let remove x s = diff s [x]
