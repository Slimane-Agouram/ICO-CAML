type temp = Gen.temp
type label = Gen.label

type instr =
  | Oper of string * temp list * temp list * label list option
  | Move of string * temp * temp
  | Label of string * label 
        
let nop = Oper ("", [], [], None)
let escape = '^'
    
exception Instruction of string * temp list * temp list
    
let digit string i =
  let n = Char.code string.[i] - Char.code '0' in
  if 0 <= n && n <= 3 then n else failwith "digit"
    
let substring s i j =  String.sub s i (j+1-i)
    
    
exception Format of instr

let namer t = string_of_int (Gen.temp_int t)


let rec output_instr chan namer string src dest i =
  if i < String.length string then
    begin try 
      let j = String.index_from string i escape in
      output_string chan (substring string i (j-1));
      let list =
        match string.[j+1] with
        | 'd' -> dest | 's' -> src
        | _ -> failwith "format" in
      let n = digit string (j+2) in
      output_string chan (namer (List.nth list n));
      output_instr chan namer string src dest (j+3);
    with
      Not_found -> 
        output_string chan (String.sub string i (String.length string -i))
    | Failure s ->
        raise (Instruction (s^":"^string, src, dest))
    end
      
let indent = "    ";;
let print chan namer instr =
  try
    begin match instr with
    | Oper (s, src, dest, labs) ->
        output_string chan indent;
        output_instr chan namer s src dest 0;
    | Label (s, l) ->
        output_string chan s
    | Move (s, src, dest) ->
        output_string chan indent;
        output_instr chan namer s [src] [dest] 0;
    end
  with
    x ->
      raise (Format instr)
        ;;

let nformat = print stdout
let format = nformat namer

(*
let print_reg t = print_string (Gen.temp_string t); print_string " ";;
let print_reg_list l = print_string "\t# "; List.iter print_reg l;;
let print_sd src dest = print_reg_list src; print_reg_list dest;;
*)
