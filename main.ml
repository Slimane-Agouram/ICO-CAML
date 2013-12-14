 (*File calc.ml*) 
    
(* file: main.ml *)
(* Assumes the parser file is "parser.mly" and the lexer file is "lexer.mll". *)
let main () =
  try
    let ic = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel stdin in
    (*while true do*)
      let pgm = Parser.main Lexer.token lexbuf in
      Printf.printf "Parsing OK\n" ; exit 0
    (*done*)
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()
;;

main();;
(*
            
            let proglistlist=
    let lexbuf = Lexing.from_channel (open_in "fact3.p") in
    Parser.main Lexer.token lexbuf;;*)
    
    (* file: main.ml 
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Parser.main Lexer.token lexbuf
    done
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()
*)