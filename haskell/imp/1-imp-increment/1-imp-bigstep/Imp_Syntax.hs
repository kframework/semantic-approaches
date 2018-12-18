-- This module defines imp data structures to be used in the semantics. We now need to derive Eq to throw away duplicate states when finding nondeterminism - see the semantics.
module Imp_Syntax where
    -- Arithmetic Binary Operators (operators for binary arithmetic expressions)
    data ABinOp =
        Plus |
        Divide
        deriving (Show, Eq)
    -- Arithmentic Expressions
    data AExp =
        Id      String |
        AConst  Integer |
        PlusPlus AExp | -- NEW. The argument here is an arithmetic expression only for the parser expression generator. The semantics will fail if it is not Id String
        ABinExp ABinOp AExp AExp
        deriving (Show, Eq)

    -- Boolean Expressions
    data BExp =
        BConst  Bool |
        BAndExp BExp BExp |
        BLtEqExp AExp AExp |
        BNotExp BExp
        deriving (Show, Eq)

    -- Statements
    data Stmt =
        Assignment  String AExp |
        SeqComp     Stmt Stmt |
        If          BExp Stmt Stmt |
        While       BExp Stmt |
        Block       Stmt |
        Skip
        deriving (Show, Eq)

    -- Program
    data Pgm =
        Init [String] Stmt
        deriving (Show, Eq)