{-# LANGUAGE NamedFieldPuns #-}

module Language.ImpPP.State
{-  ( ImpPPEnv(..)
  , initialEnv
  , lookupEnv
  , insertEnv
  , pushEnv
  , popEnv
  , writeOut
  ) -}
where

-- TODO: lens tomfoolery, but does that make this MSOS?

import Prelude hiding ( lookup )
import Data.Map.Strict ( Map, lookup, insert,  empty, fromList, union)

import Language.ImpPP.Syntax.Abstract ( Id )

data ImpPPEnv
  = ImpPPEnv
  { varEnv   :: Map Id Integer
  , envStack :: [Map Id Integer]
  , input    :: [Integer]
  , output   :: [Integer]
  }
  deriving( Eq )
instance Show ImpPPEnv where
  show ImpPPEnv{varEnv, envStack, input, output}
    = "< state: "++show varEnv++
      ", input: "++show input++
      ", output: "++show output++" >"

initialEnv :: ImpPPEnv
initialEnv = ImpPPEnv empty [] [] []

initialEnvWithInput :: [Integer] -> ImpPPEnv
initialEnvWithInput input = ImpPPEnv empty [] input []

lookupEnv :: Id -> ImpPPEnv -> Maybe Integer
lookupEnv var = lookup var . varEnv

insertEnv :: Id -> Integer -> ImpPPEnv -> ImpPPEnv
insertEnv var val env = env { varEnv = insert var val $ varEnv env }

pushEnv :: ImpPPEnv -> ImpPPEnv
pushEnv env = env { envStack = varEnv env : envStack env }

zeroEnv :: [Id] -> ImpPPEnv -> ImpPPEnv
zeroEnv ids env =  env { varEnv = union newEnv (varEnv env) }
  where newEnv = fromList $ zip ids [0,0..]

popEnv :: ImpPPEnv -> ImpPPEnv
popEnv env = env { varEnv = vars, envStack = envs }
  where (vars:envs) = envStack env -- non-total should be unreachable

updateIn :: [Integer] -> ImpPPEnv -> ImpPPEnv
updateIn ins env = env {input = ins} 

writeOut :: Integer -> ImpPPEnv -> ImpPPEnv
writeOut n env = env { output = n : output env }
