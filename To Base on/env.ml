exception Free of string;;

type 'a table = (string * 'a) list
let cherche table x =
  try List.assoc x table with Not_found -> raise (Free x)

type ('a, 'b) environment = {
    definitions : 'b table;
    global_vars : 'a table;
    local_vars : 'a table;
  } 

let create_global vars defs = {
  definitions = defs;
  global_vars = vars;
  local_vars = [];
};;
let change_local_vars env b = {env with local_vars = b  }
let add_local_vars env b = {env with local_vars = b @ env.local_vars }

let find_definition env x = cherche env.definitions x
let find_var env x =
  try cherche env.local_vars x with Free _ -> cherche env.global_vars x

