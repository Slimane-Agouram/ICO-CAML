  
(* file: main.ml *)
(* Assumes the parser file is "parser.mly" and the lexer file is "lexer.mll". *)
let main () =
  (*try*)
    let ic = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel ic in
    (*while true do*)
     let pgm = Parser.main Lexer.token lexbuf in
     let ()= Print.program pgm in 
     let() = print_newline() in 
     exit 0;;
      (*Printf.printf "Parsing OK\n" ; exit 0*)
    (*done*)
  (*with End_of_file -> print_endline ("Parse error unexcepcted end of string!")*)
      
let _ = Printexc.print main ();;


main();;