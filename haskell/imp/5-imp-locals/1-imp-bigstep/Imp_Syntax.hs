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
        Let         String AExp Stmt | -- NEW
        Skip
        deriving Show

    -- Program
    data Pgm =
        Init [String] Stmt -- We include the variables defined at the start of the program so we can actually see the final state at the end, rather than evaluating to the empty state, although they are not needed because we convert them to let in the parser. See Parser.hs for more info
        deriving Show