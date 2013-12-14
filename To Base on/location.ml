
type status_t = {
    mutable lexname : string ;
    mutable lexbuf : Lexing.lexbuf ;
    mutable file : in_channel option ;
    mutable pos : int ;
    mutable line : int}

let status = {
  lexname = "" ;
  lexbuf = Lexing.from_string "" ;
  file = None ;
  pos = 0 ;
  line = 1 ;
} 

let set name lexbuf =
  status.lexname <- name ;
  status.lexbuf <- lexbuf ;
  status.file<- (try Some (open_in name) with Sys_error _ -> None) ;
  status.pos <- 0 ;
  status.line <- 1


let rec find_line file r p = function
  | 0 -> r
  | n ->
      find_line file
        (match input_char file with
        | '\n' ->
            let line,_ = r in
            line+1,p+1
        | _ -> r)
        (p+1) (n-1)
;;

let do_get_pos () =  match status.file with
  None -> None
| Some file ->
    try 
      let lxm_start = Lexing.lexeme_start status.lexbuf
      and lxm_end = Lexing.lexeme_end status.lexbuf in
      let last_line, last_pos =
        if lxm_start < status.pos then 1,0
        else status.line, status.pos in
      seek_in file last_pos ;
      let new_line, new_pos =
        find_line
          file (last_line, last_pos)
          last_pos
          (lxm_start-last_pos) in
      status.line <- new_line ;
      status.pos <- new_pos ;
      Some (new_line, lxm_start-new_pos, lxm_end-new_pos)
    with Sys_error _ -> None


let do_print_pos s = function
  | None -> prerr_string (s^":")
  | Some (line, debut, fin) ->      
    Printf.fprintf stderr
        "File \"%s\", line %d, characters %d-%d:"
        s line debut fin

let print_pos () =  do_print_pos status.lexname (do_get_pos ())
