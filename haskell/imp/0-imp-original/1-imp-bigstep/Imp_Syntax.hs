-- This module defines imp data structures to be used in the semantics
module Imp_Syntax where
    -- Arithmetic Binary Operators (operators for binary arithmetic expressions)
    data ABinOp =
        Plus |
        Divide
        deriving Show
    -- Arithmentic Expressions
    data AExp =
        Id      String |
        AConst  Integer |
        ABinExp ABinOp AExp AExp
        deriving Show

    -- Boolean Expressions
    data BExp =
        BConst  Bool |
        BAndExp BExp BExp |
        BLtEqExp AExp AExp |
        BNotExp BExp
        deriving Show

    -- Statements
    data Stmt =
        Assignment  String AExp |
        SeqComp     Stmt Stmt |
        If          BExp Stmt Stmt |
        While       BExp Stmt |
        Block       Stmt |
        Skip
        deriving Show

    -- Program
    data Pgm =
        Init [String] Stmt
        deriving Show