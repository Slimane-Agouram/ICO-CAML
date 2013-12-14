open Misc


let ( * ) f g x = f (g x)
;;


let print_code =
  List.iter (fun i -> Ass.nformat Spim.namer i; print_newline())

let print_spim pfun p =
  let g {Spim.code=c} = pfun c in
  List.iter g (p.Spim.procedures) ; g p.Spim.main ;
  p

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
    | _ ->
        match !what with
        | Interpret -> Interpret.eval r
        | IPretty|IFlowGraph ->
            let _ =  Linear_opt.program (Trans.program r) in
            ()
        | Simul -> Simul.program (Linear_opt.program (Trans.program r))
        | Spim ->
            let r = compile r in
            let _ = print_spim
                (List.iter (fun i -> Ass.nformat Spim.namer i; print_newline()))
                r in
            ()
        | Liveness -> 
            let r = compile r in
            let _ = print_spim
                (fun c ->
                  let fg = Liveness.flow c in          
                  Liveness.print_flowcode stdout fg ;
                  Liveness.print_interference
                    stdout
                    (Liveness.interference fg))
                r in
            ()
        | (Compile|Spill) ->
            if !verbose then begin
              prerr_endline ("Compiling: "^ !prog)
            end  ;
            let alloc = match !what with
            | Spill   -> Spill.spill_all
            | Compile -> Alloc.program
            | _ -> assert false in
            let r = (alloc * compile) r in
            print_code r
        | _ -> assert false
;;

try
  main ()
with
| Parsing.Parse_error ->
    Location.print_pos () ;
    prerr_endline "Syntax error"
| Lexer.Error ->
    Location.print_pos () ;
    prerr_endline "Lexing error"
;;

