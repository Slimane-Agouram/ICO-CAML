
exception Error of string

type 'a node = 'a Gbase.node

type 'a t = ('a, unit) Gbase.t


let create x = Gbase.create x


let new_node t x = Gbase.new_node t x

let new_edge t n1 n2 =
  try
    Gbase.new_edge t n1 n2 () 
  with
  | Gbase.Error -> raise (Error "Graph.new_edge on non-existing nodes")

let nodes t = Gbase.nodes t

let info t n =
  try
    Gbase.info t n
  with
  | Gbase.Error -> raise (Error "info on non-existing node")

let succ t n = 
  try
    Gbase.succ t n ()
  with
  | Gbase.Error -> raise (Error "succ on non-existing node")

let iter g f = Gbase.iter g f

let debug chan f g = Gbase.debug chan f g

  
