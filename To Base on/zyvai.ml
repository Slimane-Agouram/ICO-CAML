open Misc

let _ =
  if !what = Compile then
    what := Interpret

let main () = match !prog with
| "" -> ()
| name ->
  let lexbuf = Lexing.from_channel (open_in name)  in
  Location.set name lexbuf ;
  let r = Parser.main Lexer.token lexbuf in
  match !what with
  | Pretty ->  Print.program r
  | Interpret -> Interpret.eval r
  | _ ->
      prerr_endline "Pour la suite du cours !!!" ;
      exit 2

;;

try
  main ()
with
| Parsing.Parse_error ->
    Location.print_pos () ;
    prerr_endline "Syntax error" ;
    exit 2
| Lexer.Error ->
    Location.print_pos () ;
    prerr_endline "Lexing error" ;
    exit 2 
| e ->
    prerr_endline ("Exception baladeuse : "^Printexc.to_string e) ;
    exit 2
;;

