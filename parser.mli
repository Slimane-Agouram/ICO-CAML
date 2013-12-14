type token =
  | INT of (int)
  | IDENT of (string)
  | STRING of (string)
  | BOOL of (bool)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LESSGREATER
  | LESSEQUAL
  | GREATEREQUAL
  | LESS
  | GREATER
  | EQUAL
  | EQUALEQUAL
  | COLON
  | COLONEQUAL
  | COMMA
  | SEMI
  | SEMISEMI
  | DOT
  | BEGIN
  | END
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | VAR
  | FUNCTION
  | PROCEDURE
  | PROGRAM
  | WRITE
  | WRITELN
  | READ
  | ALLOC
  | INTEGER
  | BOOLEAN
  | ARRAY
  | OF
  | TYPE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Pp.program
