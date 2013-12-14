open Misc

let _ =
  if !what = Compile then
    what := Liveness

let print_code =
  List.iter (fun i -> Ass.nformat Spim.namer i; print_newline())

let print_spim pfun p =
  let g {Spim.code=c} = pfun c in
  List.iter g (p.Spim.procedures) ; g p.Spim.main ;
  p

let ( * ) f g x = f (g x)


let compile =  
    Spim.program * Canon.program * Trans.program

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
     let p = Canon.program (Trans.program r) in
     Iprint.program_canon p
 | Simul -> Simul.program (Canon.program (Trans.program r))
 | Spim ->
     let r = compile r in
     let _ = print_spim
         (List.iter (fun i -> Ass.nformat Spim.namer i; print_newline()))
         r in
     ()
 | Spill ->
     print_code ((Spill.spill_all *  compile) r)
 | Liveness -> 
      let r = compile r in
      let _ = print_spim
        (fun c ->
          let fg = Liveness.flow c in
          print_endline "********** Graphe de flot *************" ;
          Liveness.print_flowgraph stdout fg ;
          let ig = Liveness.interference fg in
          print_endline "********** Graphe d'interférence **********" ;
          Liveness.print_interference stdout ig ;
          print_endline "********** Avec les moves **********" ;
          Liveness.print_moves stdout ig)
        r in
      ()
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

