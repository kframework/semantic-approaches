#lang brag

prog    : seq

seq     : stmt seq
        | stmt

id_list : ID
        | ID "," id_list

stmt    : ID "=" aexp ";"
        | "while" "(" bexp ")" block
        | "if" "(" bexp ")" block "else" block
        | block
        | "spawn" block
        | "print" "(" aexp ")" ";"
        | "halt" ";"
        | "int" id_list ";"

block   : "{" "}"
        | "{" seq "}"

aexp    : INT
        | ID
        | "(" aexp ")"
        | "read" "(" ")"
        | "++" ID
        | aexp ("+" | "/") aexp

bexp    : BOOL
        | "!" bexp
        | "(" bexp ")"
        | bexp "&&" bexp
        | aexp "<=" aexp
