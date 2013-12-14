open Misc

let _ =
  if !what = Compile then
    what := Spim

let print_code =
  List.iter (fun i -> Ass.nformat Spim.namer i; print_newline())

let print_spim pfun p =
  let g {Spim.code=c} = pfun c in
  List.iter g (p.Spim.procedures) ; g p.Spim.main ;
  p

let ( * ) f g x = f (g x)


let compile =  
    Spim.program * Linear_opt.program * Trans.program

let compile_spill = Spill.spill_all * compile


let main () = match !prog with
| "" -> ()
| name ->
  let lexbuf = Lexing.from_channel (open_in name)  in
  Location.set name lexbuf ;
  let r = Parser.main Lexer.token lexbuf in
  match !what with
  | Pretty ->  Print.program r
  | Interpret -> Interpret.eval r
 | IPretty ->
     let p = Linear_opt.program (Trans.program r) in
     Iprint.program_canon p ;
 | Simul -> Simul.program (Linear_opt.program (Trans.program r))
 | Spim ->
     let r = compile r in
     let _ = print_spim
         (List.iter (fun i -> Ass.nformat Spim.namer i; print_newline()))
         r in
     ()
 | Spill ->
     print_code ((Spill.spill_all *  compile) r)
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

