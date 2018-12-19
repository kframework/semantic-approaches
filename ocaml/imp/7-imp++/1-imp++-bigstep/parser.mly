%{
open Bigstep
%}

%token <int> INT
%token <string> ID
%token PLUS DIV LESSTHAN NOT AND EQ IF ELSE WHILE TRUE FALSE
%token READ SPAWN PRINT HALT INTDCLR
%token LPAREN RPAREN
%token LCBRCT RCBRCT
%token SEMICOLON
%token COMMA
%token EOF
%left AND
%nonassoc NOT LESSTHAN
%left PLUS
%left DIV
%start main
%type <Bigstep.stmt> main

%%
main:
| stmt EOF { $1 }
;

stmt:
| IF LPAREN bexp RPAREN block ELSE block { IfStmt ($3, $5, $7) }
| WHILE LPAREN bexp RPAREN block { WhileStmt ($3, $5) }
| PRINT LPAREN aexp RPAREN SEMICOLON { PrintStmt ($3) }
| HALT SEMICOLON { HaltStmt }
| INTDCLR ids_semicolon { DeclarationStmt ($2) }
| ID EQ aexp SEMICOLON { AssignStmt ($1, $3) }
| compound_stmt { $1 }
;

compound_stmt:
| LCBRCT RCBRCT { BlockStmt }
| LCBRCT stmt_sequence RCBRCT

block:
| LCBRCT block_cnt { $2 }
;

block_cnt:
| RCBRCT { EmptyBlock }
| stmt LCBRCT { StmtBlock ($1) }


aexp:
| INT { IntAExp ($1) }
| ID { IdAExp ($1) }
| aexp PLUS aexp { PlusAExp ($1, $3) }
| aexp DIV aexp { DivAExp ($1, $3) }
| PLUS PLUS ID { IncAExp ($3) }
| READ LPAREN RPAREN { ReadAExp }
;

bexp:
| TRUE { BoolBExp (true) }
| FALSE { BoolBExp (false) }
| aexp LESSTHAN aexp { LessThanBExp ($1, $3) }
| NOT bexp { NotBExp ($2) }
| bexp AND bexp { AndBExp ($1, $3) }
;

ids_semicolon:
| ID ids_semicolon_cnt { $1 :: $2 }
;

ids_semicolon_cnt:
| SEMICOLON { [] }
| COMMA ids_semicolon { $2 }
;

