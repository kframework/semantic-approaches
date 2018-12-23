module Language.ImpPP.Semantics.SmallStep
--  ( runImpPPTrace
--  , execPgm
--  )
where


-- TODO
-- ~abrupt termination~
-- ~IO~
-- ~increment~
-- ~local~
-- ~spawn~

import Language.ImpPP.Syntax.Abstract
import Language.ImpPP.State


import SemanticModel.SmallStep

type ImpPPTrace = Trace ImpPPEnv

runImpPPTrace :: ImpPPTrace syntax -> [(syntax, ImpPPEnv)]
runImpPPTrace = runTrace initialEnv

stepRunImpPP :: Transition ImpPPEnv syntax => [Integer] -> syntax -> [[(syntax, ImpPPEnv)]]
stepRunImpPP input syntax = stepRun (initialEnvWithInput input) syntax

--------------
-- Programs --
{- The rule for programs is special. For the sake of consistency
every rule should have to follow through a top level program rule,
but for practicality sake (the Pgm construct can be erased after
the first application) we simply use it to build the initial state. -}
-- pgmRule :: Pgm -> ImpPPTrace Stmt
-- pgmRule (Pgm ids stmt) = put (initialEnv ids) >> return stmt

execPgm :: Stmt -> ImpPPTrace Stmt
execPgm pgm = manyStep $ put initialEnv >> pure pgm

----------------
-- Arithmetic --
aexpRules = [ lookupRule
            , readRule, incRule
            , addRule, addLRule, addRRule, addERule
            , divRule, divLRule, divRRule, divERule
            ]

instance Transition ImpPPEnv (Exp Integer) where
  isLiteral (Lit _) = True
  isLiteral (Error) = True
  isLiteral _       = False
  rules = aexpRules

-- read
readRule :: Exp Integer -> ImpPPTrace (Exp Integer)
readRule Read = do
  inputs <- gets input
  case inputs of
    []     -> mzero
    i:rest -> do
      modify $ updateIn rest
      return $ Lit i
readRule _ = mzero

-- lookup -- dnc?
lookupRule :: Exp Integer -> ImpPPTrace (Exp Integer)
lookupRule (Var v) = do
  mi <- gets $ lookupEnv v
  case mi of
    Nothing -> mzero
    Just i  -> return $ Lit i
                    
lookupRule _ = mzero

-- increment
incRule :: Exp Integer -> ImpPPTrace (Exp Integer)
incRule (Inc var) = do
  mVal <- gets $ lookupEnv var
  case mVal of
    Nothing  -> mzero
    Just val -> do
      modify $ insertEnv var (val + 1)
      return $ Lit $ val + 1

incRule _ = mzero

-- addition
addERule :: Exp Integer -> ImpPPTrace (Exp Integer)
addERule (Error :+: _) = return Error
addERule (_ :+: Error) = return Error
addERule _ = mzero

addRule :: Exp Integer -> ImpPPTrace (Exp Integer)
addRule (Lit i1 :+: Lit i2) = return $ Lit $ i1 + i2
addRule _ = mzero

addLRule :: Exp Integer -> ImpPPTrace (Exp Integer)
addLRule (aexp1 :+: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :+: aexp2
      }
addLRule _ = mzero

addRRule :: Exp Integer -> ImpPPTrace (Exp Integer)
addRRule (aexp1 :+: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ aexp1 :+: aexp2'
      }
addRRule _ = mzero

-- division
divERule :: Exp Integer -> ImpPPTrace (Exp Integer)
divERule (Error :/: _) = return Error
divERule (_ :/: Error) = return Error
divERule _ = mzero

divRule :: Exp Integer -> ImpPPTrace (Exp Integer)
divRule (Lit i1 :/: Lit 0) = return Error
divRule (Lit i1 :/: Lit i2) = return $ Lit $ i1 `div` i2
divRule _ = mzero

divLRule :: Exp Integer -> ImpPPTrace (Exp Integer)
divLRule (aexp1 :/: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :/: aexp2
      }
divLRule _ = mzero

divRRule :: Exp Integer -> ImpPPTrace (Exp Integer)
divRRule (aexp1 :/: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ aexp1 :/: aexp2'
      }
divRRule _ = mzero


-----------
-- Logic --

bexpRules = [ leqRule, leqLRule, leqRRule, leqERule
            , andRule, andLRule, andERule
            , notRule, not'Rule, notERule
            ]
            
instance Transition ImpPPEnv (Exp Bool) where
  isLiteral (Lit _) = True
  isLiteral _       = False
  rules = bexpRules

-- less than or equal to
leqERule :: Exp Bool -> ImpPPTrace (Exp Bool)
leqERule (Error :<=: _) = return Error
leqERule (_ :<=: Error) = return Error
leqERule _ = mzero

leqRule :: Exp Bool -> ImpPPTrace (Exp Bool)
leqRule (Lit i1 :<=: Lit i2) = return $ Lit $ i1 <= i2
leqRule _ = mzero

leqLRule :: Exp Bool -> ImpPPTrace (Exp Bool)
leqLRule (aexp1 :<=: aexp2)
  | isNotLiteral aexp1 = do
      { aexp1' <- o aexp1
      ; return $ aexp1' :<=: aexp2
      }
leqLRule _ = mzero

leqRRule :: Exp Bool -> ImpPPTrace (Exp Bool)
leqRRule (Lit i :<=: aexp2)
  | isNotLiteral aexp2 = do
      { aexp2' <- o aexp2
      ; return $ Lit i :<=: aexp2'
      }
leqRRule _ = mzero

-- and
andERule :: Exp Bool -> ImpPPTrace (Exp Bool)
andERule (Error :&&: _) = return Error
-- andERule (_ :&&: Error) = return Error -- shadowed by short circuiting
andERule _ = mzero

andRule :: Exp Bool -> ImpPPTrace (Exp Bool)
andRule (Lit True :&&: bexp) = return $ bexp
andRule (Lit False :&&: _)   = return $ Lit False
andRule _ = mzero

andLRule :: Exp Bool -> ImpPPTrace (Exp Bool)
andLRule (bexp1 :&&: bexp2)
  | isNotLiteral bexp1 = do
      { bexp1' <- o bexp1
      ; return $ bexp1' :&&: bexp2
      }
andLRule _ = mzero


-- not
notERule :: Exp Bool -> ImpPPTrace (Exp Bool)
notERule (Not Error) = return Error
notERule _ = mzero

notRule :: Exp Bool -> ImpPPTrace (Exp Bool)
notRule (Not (Lit True)) = return $ Lit False
notRule (Not (Lit False)) = return $ Lit True
notRule _ = mzero

not'Rule :: Exp Bool -> ImpPPTrace (Exp Bool)
not'Rule (Not bexp) = do
  { bexp' <- o bexp
  ; return $ Not bexp'
  }
not'Rule _ = mzero

----------------
-- Statements --
stmtRules = [ decRule, dec'Rule, decHRule
            , assignRule, assign'Rule, assignERule
            , printRule, print'Rule, printERule
            , seqRule, seq'Rule, seqHRule, seqSRule
            , spawnRule, spawn'Rule, spawnHRule
            , ifRule, if'Rule, ifERule
            , whileRule
            ]

instance Transition ImpPPEnv Stmt where
  isLiteral Empty = True
  isLiteral _     = False
  rules = stmtRules

-- local declarations
decHRule :: Stmt -> ImpPPTrace Stmt
decHRule (Decl _ Halt) = return Halt
decHRule _ = mzero

decRule :: Stmt -> ImpPPTrace Stmt
decRule (Decl [] Empty) = do
  modify popEnv
  return Empty
decRule (Decl ids@(_:_) stmt) = do
  modify $ zeroEnv ids . pushEnv
  return $ Decl [] stmt 
decRule _ = mzero

dec'Rule :: Stmt -> ImpPPTrace Stmt
dec'Rule (Decl [] stmt)
  | isNotLiteral stmt = do
      stmt' <- o stmt
      return $ Decl [] stmt'
dec'Rule _ = mzero

-- assignment
assignERule :: Stmt -> ImpPPTrace Stmt
assignERule (_ := Error) = return Halt
assignERule _ = mzero

assignRule :: Stmt -> ImpPPTrace Stmt
assignRule (x := Lit i) = do
  { mi <- gets $ lookupEnv x
  ; case mi of
      Nothing -> mzero
      _ -> (modify $ insertEnv x i) >> pure Empty
  }
assignRule _ = mzero

assign'Rule :: Stmt -> ImpPPTrace Stmt
assign'Rule (x := aexp)
  | isNotLiteral aexp = do
      { aexp' <- o aexp
      ; return $ x := aexp'
      }
assign'Rule _ = mzero

-- print
printERule :: Stmt -> ImpPPTrace Stmt
printERule (Print Error) = return Halt
printERule _ = mzero

printRule :: Stmt -> ImpPPTrace Stmt
printRule (Print (Lit i)) = do
  modify $ writeOut i
  return Empty
printRule _ = mzero

print'Rule  :: Stmt -> ImpPPTrace Stmt
print'Rule (Print aExp)
  | isNotLiteral aExp = do
      aExp' <- o aExp
      return $ Print aExp'
print'Rule _ = mzero

-- spawn
spawnHRule :: Stmt -> ImpPPTrace Stmt
spawnHRule (Spawn Halt) = return Halt
spawnHRule _ = mzero

spawnRule :: Stmt -> ImpPPTrace Stmt
spawnRule (Spawn Empty) = return Empty
spawnRule _ = mzero

spawn'Rule :: Stmt -> ImpPPTrace Stmt
spawn'Rule (Spawn stmt)
  | isNotLiteral stmt = do
      stmt' <- o stmt
      return $ Spawn stmt'
spawn'Rule _  = mzero

-- seq-spawn rule
seqSRule :: Stmt -> ImpPPTrace Stmt
seqSRule (Seq spwn@(Spawn _) stmt2)
  | isNotLiteral stmt2 = do
      stmt2' <- o stmt2
      return $ Seq spwn stmt2'
seqSRule _ = mzero

-- sequence
seqHRule :: Stmt -> ImpPPTrace Stmt
seqHRule (Seq Halt _) = return Halt
seqHRule _ = mzero

seqRule :: Stmt -> ImpPPTrace Stmt
seqRule (Seq Empty s2) = return s2
seqRule _ = mzero

seq'Rule :: Stmt -> ImpPPTrace Stmt
seq'Rule (Seq s1 s2) 
  | isNotLiteral s1 = do
      { s1' <- o s1
      ; return $ Seq s1' s2
      }
seq'Rule _ = mzero

-- if then else
ifERule :: Stmt -> ImpPPTrace Stmt
ifERule (IfElse Error _ _) = return Halt
ifERule _ = mzero

ifRule :: Stmt -> ImpPPTrace Stmt
ifRule (IfElse (Lit  True) s1 _) = return s1
ifRule (IfElse (Lit False) _ s2) = return s2
ifRule _ = mzero

if'Rule :: Stmt -> ImpPPTrace Stmt
if'Rule (IfElse bexp s1 s2)
  | isNotLiteral bexp = do
      { bexp' <- o bexp
      ; return $ IfElse bexp' s1 s2
      }
if'Rule _ = mzero

-- while -- un-changed
whileRule :: Stmt -> ImpPPTrace Stmt
whileRule while@(While bexp s) =
  return $ IfElse bexp (Seq s while) Empty
whileRule _ = mzero
