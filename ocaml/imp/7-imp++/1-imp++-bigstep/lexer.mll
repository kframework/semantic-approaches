{
  open Parser
}

rule token = parse
| [' ' '\n' '\t']  { token lexbuf }
| ['0'-'9']+  { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '+'         { PLUS }
| '/'         { DIV }
| "<="        { LESSTHAN }
| "!"         { NOT }
| "&&"        { AND }
| '='         { EQ }
| "if"        { IF }
| "else"      { ELSE }
| "while"     { WHILE }
| "true"      { TRUE }
| "false"     { FALSE }
| "read"      { READ }
| "spawn"     { SPAWN }
| "print"     { PRINT }
| "halt"      { HALT }
| "int"       { INTDCLR }
| ['a'-'z']+  { ID (Lexing.lexeme lexbuf) }
| "("         { LPAREN }
| ")"         { RPAREN }
| "{"         { LCBRCT }
| "}"         { RCBRCT }
| ';'         { SEMICOLON }
| ','         { COMMA }
| eof         { EOF }
