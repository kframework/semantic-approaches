type token =
  | INT of (int)
  | ID of (string)
  | PLUS
  | DIV
  | LESSTHAN
  | NOT
  | AND
  | EQ
  | IF
  | ELSE
  | WHILE
  | TRUE
  | FALSE
  | READ
  | SPAWN
  | PRINT
  | HALT
  | INTDCLR
  | LPAREN
  | RPAREN
  | LCBRCT
  | RCBRCT
  | SEMICOLON
  | COMMA
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bigstep.stmt
