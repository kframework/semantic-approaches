module Language.Imp.State
  ( ImpEnv(..)
  , emptyEnv
  , initialEnv
  , lookupEnv
  , insertEnv
  )
where

import Prelude hiding ( lookup )
import Data.Map.Strict ( Map, lookup, insert,  empty, fromList )

import Language.Imp.Syntax.Abstract

type ImpEnv = Map Id Integer

initialEnv :: DecList -> ImpEnv
initialEnv ids = fromList $ zip ids $ [0,0..]

lookupEnv :: Id -> ImpEnv -> Maybe Integer
lookupEnv = lookup

insertEnv :: Id -> Integer -> ImpEnv -> ImpEnv
insertEnv = insert

emptyEnv :: ImpEnv
emptyEnv = empty
