exception Error of string

type ('a, 'b) t = ('a, 'b) Gbase.t


type 'a node = 'a Gbase.node

let create x = Gbase.create x

let new_node t x = Gbase.new_node t x

let new_edge g n1 n2 l =
  if n1 <> n2 then begin
    try
      Gbase.new_edge g n1 n2 l ;
      Gbase.new_edge g n2 n1 l
    with
    | Gbase.Error -> raise (Error "Sgraph.new_edge: non-existent node")
  end

let nodes g = Gbase.nodes g

let info t n =
  try
    Gbase.info t n
  with
  | Gbase.Error  -> raise (Error "Sgraph.info: non-existing node")

let adj t n =
  try
    Gbase.succ t n
  with
  | Gbase.Error -> raise (Error "Sgraph.adj on non-existing node")

let iter g f = Gbase.iter g f
