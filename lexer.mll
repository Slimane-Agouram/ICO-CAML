* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, and unknown characters
   and raises End_of_file on EOF. *)

{
  open Parser
  exception Eof
}
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']  { token lexbuf }
  | '\n'  { NEWLINE }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
    { NUM (float_of_string num) }
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { MULTIPLY }
  | '/'   { DIVIDE }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | ';'   { SEMICOLON }
  | _     { token lexbuf }
 (* | "and" { AND }
  | "or"  { OR }
  | "not" { NOT }
  | "<"   { LT }
  | "<="  { LE }
  | ">"   { GT }
  | ">="  { GE }
  | "="   { EQ }
  | "<>"  { NE }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "["   { LBRACKET }
  | "]"   { RBRACKET } 
  | ","   { COMMA }
  | ":="  { COLONEQ } 
  | ":"   { COLON }
  | "."   { DOT }
  | "program"  { PROGRAM } 
  | "begin"  { BEGIN }
  | "end"  { END }
  | "if"  { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "while"  { WHILE }
  | "do"  { DO }
  | "procedure"  { PROCEDURE }
  | "function"  { FUNCTION }
  | "var"  { VAR }
  | "new"  { NEW }
  | "readln"  { READLN }
  | "write"  { WRITE }
  | "writeln"  { WRITELN }
  | "integer"   { INTEGER }
  | "boolean"  { BOOLEAN }
  | "array"  { ARRAY }
  | "of"  { OF } 
  | eof   { raise Eof }
