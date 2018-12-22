#lang brag

prog    : stmt

id_list : ID
        | ID "," id_list

stmt    : block
        | "print" "(" aexp ")" ";"
        | "let" id_list "in" stmt
        | "int" id_list ";"
        | ID "=" aexp ";"
        | stmt stmt
        | "if" "(" bexp ")" block "else" block
        | "while" "(" bexp ")" block

block   : "{" "}"
        | "{" stmt "}"

aexp    : INT
        | ID
        | aexp "+" aexp
        | aexp "/" aexp
        | "++" ID

bexp    : BOOL
        | aexp "<=" aexp
        | "!" bexp
        | bexp "&&" bexp
