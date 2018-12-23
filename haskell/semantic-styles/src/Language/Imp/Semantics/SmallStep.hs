module Language.Imp.Semantics.SmallStep
--  ( runImpTrace
--  , execPgm
--  )
where

import Language.Imp.Syntax.Abstract
import Language.Imp.State


import SemanticModel.SmallStep

type ImpTrace = Trace ImpEnv

runImpTrace :: ImpTrace syntax -> [(syntax, ImpEnv)]
runImpTrace impTrace = runTrace emptyEnv impTrace

--------------
-- Programs --
{- The rule for programs is special. For the sake of consistency
every rule should have to follow through a top level program rule,
but for practicality sake (the Pgm construct can be erased after
the first application) we simply use it to build the initial state. -}
pgmRule :: Pgm -> ImpTrace Stmt
pgmRule (Pgm ids stmt) = put (initialEnv ids) >> return stmt

execPgm :: Pgm -> ImpTrace Stmt
execPgm pgm = manyStep $ pgmRule pgm

----------------
-- Arithmetic --
aexpRules = [ lookupRule
            , addRule, addLRule, addRRule
            , divRule, divLRule, divRRule
            ]

instance Transition ImpEnv (Exp Integer) where
  isLiteral (Lit _) = True
  isLiteral _       = False
  rules = aexpRules

-- lookup
lookupRule :: Exp Integer -> ImpTrace (Exp Integer)
lookupRule (Var v) =
  do { mi <- gets $ lookupEnv v
     ; case mi of
         Nothing -> mzero
         Just i  -> return $ Lit i
     }
lookupRule _ = mzero

-- addition
addRule :: Exp Integer -> ImpTrace (Exp Integer)
addRule (Lit i1 :+: Lit i2) = return $ Lit $ i1 + i2
addRule _ = mzero

addLRule :: Exp Integer -> ImpTrace (Exp Integer)
addLRule (aexp1 :+: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :+: aexp2
      }
addLRule _ = mzero

addRRule :: Exp Integer -> ImpTrace (Exp Integer)
addRRule (aexp1 :+: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ aexp1 :+: aexp2'
      }
addRRule _ = mzero

-- division
divRule :: Exp Integer -> ImpTrace (Exp Integer)
divRule (Lit i1 :/: Lit 0) = mzero
divRule (Lit i1 :/: Lit i2) = return $ Lit $ i1 `div` i2
divRule _ = mzero

divLRule :: Exp Integer -> ImpTrace (Exp Integer)
divLRule (aexp1 :/: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :/: aexp2
      }
divLRule _ = mzero

divRRule :: Exp Integer -> ImpTrace (Exp Integer)
divRRule (aexp1 :/: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ aexp1 :/: aexp2'
      }
divRRule _ = mzero




-----------
-- Logic --

bexpRules = [ leqRule, leqLRule, leqRRule
            , andRule, andLRule
            , notRule, not'Rule
            ]
            
instance Transition ImpEnv (Exp Bool) where
  isLiteral (Lit _) = True
  isLiteral _       = False
  rules = bexpRules

-- less than or equal to
leqRule :: Exp Bool -> ImpTrace (Exp Bool)
leqRule (Lit i1 :<=: Lit i2) = return $ Lit $ i1 <= i2
leqRule _ = mzero

leqLRule :: Exp Bool -> ImpTrace (Exp Bool)
leqLRule (aexp1 :<=: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :<=: aexp2
      }
leqLRule _ = mzero

leqRRule :: Exp Bool -> ImpTrace (Exp Bool)
leqRRule (Lit i :<=: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ Lit i :<=: aexp2'
      }
leqRRule _ = mzero

-- and
andRule :: Exp Bool -> ImpTrace (Exp Bool)
andRule (Lit True :&&: bexp) = return $ bexp
andRule (Lit False :&&: _)   = return $ Lit False
andRule _ = mzero

andLRule :: Exp Bool -> ImpTrace (Exp Bool)
andLRule (bexp1 :&&: bexp2)
  | isNotLiteral bexp1 = do
      { bexp1' <- o bexp1
      ; return $ bexp1' :&&: bexp2
      }
andLRule _ = mzero


-- not
notRule :: Exp Bool -> ImpTrace (Exp Bool)
notRule (Not (Lit True)) = return $ Lit False
notRule (Not (Lit False)) = return $ Lit True
notRule _ = mzero

not'Rule :: Exp Bool -> ImpTrace (Exp Bool)
not'Rule (Not bexp) = do
  { bexp' <- o bexp
  ; return $ Not bexp'
  }
not'Rule _ = mzero

----------------
-- Statements --
stmtRules = [ assignRule, assign'Rule
            , seqRule, seq'Rule
            , ifRule, if'Rule
            , whileRule
            ]

instance Transition ImpEnv Stmt where
  isLiteral Empty = True
  isLiteral _     = False
  rules = stmtRules

-- assignment
assignRule :: Stmt -> ImpTrace Stmt
assignRule (x := Lit i) = do
  { mi <- gets $ lookupEnv x
  ; case mi of
      Nothing -> mzero
      _ -> (modify $ insertEnv x i) >> pure Empty
  }
assignRule _ = mzero

assign'Rule :: Stmt -> ImpTrace Stmt
assign'Rule (x := aexp)
  | isNotLiteral aexp = do
      { aexp' <- o aexp
      ; return $ x := aexp'
      }
assign'Rule _ = mzero

-- sequence
seqRule :: Stmt -> ImpTrace Stmt
seqRule (Seq Empty s2) = return s2
seqRule _ = mzero

seq'Rule :: Stmt -> ImpTrace Stmt
seq'Rule (Seq s1 s2)
  | isNotLiteral s1 = do
      { s1' <- o s1
      ; return $ Seq s1' s2
      }
seq'Rule _ = mzero

-- if then else
ifRule :: Stmt -> ImpTrace Stmt
ifRule (IfElse (Lit  True) s1 _) = return s1
ifRule (IfElse (Lit False) _ s2) = return s2
ifRule _ = mzero

if'Rule :: Stmt -> ImpTrace Stmt
if'Rule (IfElse bexp s1 s2)
  | isNotLiteral bexp = do
      bexp' <- o bexp
      return $ IfElse bexp' s1 s2
if'Rule _ = mzero

-- while
whileRule :: Stmt -> ImpTrace Stmt
whileRule while@(While bexp s) =
  return $ IfElse bexp (Seq s  while) Empty
whileRule _ = mzero
