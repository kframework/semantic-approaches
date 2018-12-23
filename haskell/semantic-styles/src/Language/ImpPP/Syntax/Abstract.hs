{-# LANGUAGE StandaloneDeriving #-}
module Language.ImpPP.Syntax.Abstract where

import Data.List ( intercalate )

class Pretty a where
  pretty :: a -> String

type Id = String
type DecList = [Id]

data Exp a where
  (:<=:) :: Exp Integer -> Exp Integer -> Exp Bool
  (:&&:) :: Exp Bool    -> Exp Bool    -> Exp Bool
  Not    :: Exp Bool                   -> Exp Bool
  (:+:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:/:)  :: Exp Integer -> Exp Integer -> Exp Integer
  Read   ::                               Exp Integer
  Inc    :: Id                         -> Exp Integer
  Var    :: Id                         -> Exp Integer
  Lit    :: a                          -> Exp a
  Error  ::                               Exp a
deriving instance Show a => Show (Exp a)

instance Show a => Pretty (Exp a) where
  pretty (Error)      = "<error: division by 0>"
  pretty (Read)       = "read()"
  pretty (Lit val)    = show val
  pretty (Var var)    = var 
  pretty (Not e)      = "!" ++ pretty e
  pretty (Inc var)    = "++"++var
  pretty (e1 :+: e2)  = "(" ++ pretty e1 ++ " + "  ++ pretty e2 ++ ")"
  pretty (e1 :/: e2)  = "(" ++ pretty e1 ++ " / "  ++ pretty e2 ++ ")"
  pretty (e1 :<=: e2) = "(" ++ pretty e1 ++ " <= " ++ pretty e2 ++ ")"
  pretty (e1 :&&: e2) = "(" ++ pretty e1 ++ " && " ++ pretty e2 ++ ")"

data Stmt
  = Id := Exp Integer
  | Decl [Id] Stmt
  | Print (Exp Integer)
  | Spawn Stmt
  | Seq Stmt Stmt
  | IfElse (Exp Bool) Stmt Stmt
  | While (Exp Bool) Stmt
  | Empty
  | Halt
  deriving( Show )

instance Pretty Stmt where
  pretty (Spawn stmt)              = "spawn { "++pretty stmt++" }"
  pretty (Print aexp)              = "print( "++pretty aexp++" ) ;"
  pretty (x := aexp)               = x ++ " := " ++ pretty aexp ++ " ;"
  pretty (Seq stmt1 stmt2)         = pretty stmt1 ++ "  " ++ pretty stmt2
  pretty (IfElse bexp stmt1 stmt2) = "if "++pretty bexp++" then { "++pretty stmt1++" } else { "++pretty stmt2++" }"
  pretty (While bexp stmt)         = "while "++pretty bexp++" { "++pretty stmt++" }"
  pretty Empty                     = "{}"
  pretty Halt                      = "halt ;"
  pretty (Decl [] stmt)            = pretty stmt
  pretty (Decl ids stmt)           = "int "++varList++" ;  "++ pretty stmt
    where varList = intercalate ", " ids

  
