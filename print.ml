open Pp
open Format

let rec type_expr = function
    Integer -> printf "integer"
  | Boolean -> printf "boolean"
  | Array t -> printf "array of "; type_expr t


let list sep f = function
    [] -> ()
  | h::t -> f h; List.iter (fun x -> printf sep; f x) t


let option f = function None -> () | Some x -> f x


let var (s, t) = 
  printf "%s : " s;
  type_expr t
    

let var_list l = list ", " var l

let declarations l = match l with
| [] -> ()
| _  ->
  printf "@[<v 2>var@;" ;
  List.iter (fun x -> var x; printf ";@ ") l ;
  printf"@]@;"

let binop op =
  printf "%s"
    begin match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    | Eq -> "="
    | Ne -> "<>"
    end


let priority = function
    Plus | Minus -> 2
  | Times | Div -> 3
  | _ -> 1

let lowest = 0
let highest = 4

let rec expr p = function
    Int n ->
      printf "%d" n
  | Bool b ->
      printf "%s" (if b then "true" else "false")
  | Bin (op, e1, e2) ->
      let p_op = priority op in
      let b = p > p_op in
      let p' = if b then lowest else p_op in
      if b then printf "(";
      expr p' e1;
      binop op;
      expr p' e2;
      if b then printf ")";
  | Get s ->
      printf "%s" s; 
  | Function_call (s, l) ->
      printf "%s(%a)" s (fun out l -> list ", " (expr lowest) l) l; ()
  | Geti (e1, e2) -> 
      expr highest e1;
      printf "[";
      expr lowest e2;
      printf "]";
  | Alloc (e, t) ->
      printf "alloc@;(";
      expr lowest e;
      printf " :@;";
      type_expr t;
      printf ")"
 

let expression p e =
  printf "@[";
  expr p e;
  printf "@]"
  
let rec instruction = function
  | Set (s,e) -> 
      printf "%s := %a" s (fun out -> expression lowest) e;
  | Sequence l ->
      printf "@[<hv>be@[<hv>gin@ ";
      list ";@ " instruction l;
      printf "@]@ end@]";
  | If (e, t, f) -> 
      printf "@[<hv>@[<hv>if@[<hv>@ ";
      expression lowest e;
      printf "@]@ th@[<hv>en@ ";
      instruction t;
      printf "@]@]@ el@[<hv>se@ ";
      instruction f;
      printf "@]@]";
  | While (e, i) -> 
      printf "wh@[ile@;";
      expression lowest e;
      printf " do@ ";
      instruction i;
      printf "@]";
      
  | Procedure_call (s, l) -> 
      printf "%s (" s;
      list ", " (expression lowest) l;
      printf ")";
  | Seti (e1, e2, e3)-> 
      expression highest e1;
      printf "[";
      expression lowest e2;
      printf "] := ";
      expression lowest e3;
  | Write_int n ->
      instruction (Procedure_call ("write", [n]));
  | Writeln_int n ->
      instruction (Procedure_call ("writeln", [n]));
  | Read_int x ->
      instruction (Procedure_call ("read", [Get x]))



let definition (s,f) =
  printf "@[<v>@[<hv 2>%s %s@ "
    (match f.result with None -> "procedure" | _ -> "function")
    s;
  printf "(@[" ; list ",@ " (fun x -> var x) f.arguments; printf ")@]";
  option (fun t -> (printf "@ : "; type_expr t)) f.result;
  printf ";@]@ ";
  declarations f.local_vars;
  instruction (Sequence f.body) ;
  printf ";@ @ "
    

let definitions list =
  List.iter definition list

let program p =
  printf "@[<v>program@ ";
  declarations p.global_vars;
  printf "@ ";
  definitions p.definitions;
  instruction (Sequence p.main) ;
  printf "@ @]";
  print_newline();


