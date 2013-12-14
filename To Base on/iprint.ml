open Code;;
open Trans;;
open Frame;;
open Format;;

let temp chan t = fprintf chan "$%s" (Gen.temp_string t)

let temps chan ts =
  List.iter (fun t ->fprintf chan " %a" temp t) ts

let sop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div   -> "/"
  | Lt    -> "<"
  | Le    -> "<="
  | Gt    -> ">"
  | Ge    -> ">="
  | Eq    -> "="
  | Ne    -> "<>"
  | Uplus  -> "+u"


and srelop = function
  | Rlt    -> "<"
  | Rle    -> "<="
  | Rgt    -> ">"
  | Rge    -> ">="
  | Req    -> "="
  | Rne    -> "<>"

let rec do_code chan l =
  let rec code_rec = function
    | [] -> ()
    | [i] -> instr chan i
    | i::rem ->
        instr chan i ;
        fprintf chan "@ " ;
        code_rec rem in
  fprintf chan "@[<v>" ;
  code_rec l ;
  fprintf chan "@]"

and instr chan = function
  | Label l ->
      fprintf chan "%s:" (Gen.label_string l)
  | Move_temp (t,e) ->
      fprintf chan "  @[<2>(set %a@ @[%a@])@]" temp t expr e
  | Move_mem (e1,e2) ->
      fprintf chan "  @[<2>(setmem@ @[%a@]@ @[%a@])@]" expr e1 expr e2
  | Exp e ->
      fprintf chan "  @[<2>Exp@ @[%a@]@]" expr e
  | Seq l ->
      fprintf chan "  Seq@,  %a" do_code l
  | Jump l ->
      fprintf chan "  Jump %s" (Gen.label_string l)
  | Cjump (op,e1, e2, l1, l2) ->
      fprintf chan "  @[<2>Cjump %s %s@ @[<2>(%s@ %a@ %a)@]@]"
        (Gen.label_string l1) (Gen.label_string l2)
        (srelop op) expr e1 expr e2

and expr chan = function
  | Const i -> fprintf chan "%d" i
  | Name l  -> fprintf chan "%s" (Gen.label_string l)
  | Temp t  -> temp chan t
  | Mem e -> fprintf chan "@[<2>(mem@ %a)@]" expr e
  | Bin (op,e1,e2) ->
      fprintf chan "@[<2>(%s@ %a@ %a)@]" (sop op) expr e1 expr e2
  | Call (f,args) ->
      fprintf chan "@[<2>(call %s%a)@]"
        (Gen.label_string (Frame.frame_name f))
        exprs args

and exprs chan = function
  | [] -> ()
  | e::rem ->
      fprintf chan "@ %a%a"
        expr e exprs rem

let procedure pcode chan (f,l) =  
  fprintf chan
    "@ @[<v>%s %s@ "
    (match frame_result f with
    | None -> "procedure"
    | Some _ -> "function")
    (Gen.label_string (frame_name f)) ;
  fprintf chan "args = %a@ " temps (frame_args f) ;
  begin match frame_result f with
  | None -> ()
  | Some t ->
      fprintf chan "result = %a@ " temp t
  end ;      
  pcode chan l ;
  fprintf chan "@ @]"

let procedures chan pcode l =
  List.iter (procedure chan pcode) l

let exp e =
  let chan = std_formatter in
  fprintf chan "@[%a@]" expr e ;
  print_newline ()

let stm s =
  let chan = std_formatter in
  fprintf chan "@[%a@]" instr s ;
  print_newline ()

let do_program pcode p =
  let chan = std_formatter in
  fprintf chan "@[<v>program@ %d globals,@ " p.number_of_globals;
  procedures pcode chan p.procedures ;
  procedure pcode chan p.main ;
  fprintf chan "@]";
  print_newline()

let program p = do_program instr p
and program_canon p =  do_program do_code p

let frame_code f =
  let chan = std_formatter in
  procedure instr chan f ;
  print_newline ()

let code c =
  let chan = std_formatter in
  do_code chan c ;
  print_newline ()

