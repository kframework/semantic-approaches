module Language.Imp.Syntax.Abstract where

import Data.List ( intercalate )

type Id = String
type DecList = [Id]

data Exp a where
  (:+:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:/:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:<=:) :: Exp Integer -> Exp Integer -> Exp Bool
  (:&&:) :: Exp Bool    -> Exp Bool    -> Exp Bool
  Not    :: Exp Bool                   -> Exp Bool
  Var    :: Id                         -> Exp Integer
  Lit    :: a                          -> Exp a

instance Show a => (Show (Exp a)) where
  show (Lit val)    = show val
  show (Var var)    = show var 
  show (Not e)      = "!" ++ show e
  show (e1 :+: e2)  = "(" ++ show e1 ++ " + "  ++ show e2 ++ ")"
  show (e1 :/: e2)  = "(" ++ show e1 ++ " / "  ++ show e2 ++ ")"
  show (e1 :<=: e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
  show (e1 :&&: e2) = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"

data Stmt
  = Id := Exp Integer
  | Seq Stmt Stmt
  | IfElse (Exp Bool) Stmt Stmt
  | While (Exp Bool) Stmt
  | Empty
  
instance Show Stmt where
  show (x := aexp) = x ++ " := " ++ show aexp ++ " ;"
  show (Seq stmt1 stmt2) = show stmt1 ++ "  " ++ show stmt2
  show (IfElse bexp stmt1 stmt2) = "if "++show bexp++" then { "++show stmt1++" } else { "++show stmt2++" }"
  show (While bexp stmt) = "while "++show bexp++" {"++show stmt++" }"
  show Empty = "{}"

data Pgm = Pgm DecList Stmt

instance Show Pgm where
  show (Pgm ids stmt) = "int "++varList++" ;"++show stmt
    where varList = intercalate ", " $  map show ids
