module SemanticModel.SmallStep
  ( Transition, o, nStep
  , manyStep
  , stepRun
  , isLiteral, isNotLiteral, rules
  , Trace(..), runTrace
  , get, put, modify, runStateT, gets, mzero, mplus
  , state
  )
where

import Data.Hashable ( hash )
import Control.Monad.State.Lazy
import Data.List.Extra ( nubOn )

class (Eq config, Show syntax) => Transition config syntax | syntax -> config where
  isLiteral    :: syntax -> Bool

  isNotLiteral :: syntax -> Bool
  isNotLiteral = not . isLiteral

  rules :: [syntax -> Trace config syntax]
  
  o :: syntax -> Trace config syntax
  o = buildStep rules

  nStep :: Int -> syntax -> Trace config syntax
  nStep n syntax
    | n >= 0 = foldl ( >>= ) (return syntax) $ replicate n o
  nStep _ syntax = error "nStep: Integer argument must be positive."

  allStep :: syntax -> [Trace config syntax]
  allStep stmt = map (flip nStep stmt) [0..]  

  buildStep
    :: [syntax -> Trace config syntax]
    ->  syntax -> Trace config syntax
  buildStep rules syntax
    = foldl (\l r -> l `mplus` r syntax) mzero rules

  stepRun :: config -> syntax -> [[(syntax, config)]]
  stepRun config syntax
    = takeWhile (not . null) $ [ nubOn convert
                               $ runTrace config
                               $ flip nStep syntax n | n <- [0..]]
    where convert (l,r) = (show l, r)
 
  manyStep :: Trace config syntax -> Trace config syntax
  manyStep trace = results `mplus` (toRecurse >>= (\_ -> manyStep toRecurse))
    where results = mfilter isLiteral trace
          intermediate = (mfilter isNotLiteral trace) >>= o
          toRecurse = rebuildState $ nubOn snd $ runTrace undefined intermediate
          conversion (syntax, env) = (hash $ show syntax, env)

  rebuildState :: [(syntax, config)] -> Trace config syntax
  rebuildState = foldl (\l (syntax,state) -> l `mplus` (put state >> return syntax)) mzero
  
  
type Trace state = StateT state []

runTrace :: config -> Trace config syntax -> [(syntax, config)]
runTrace = flip runStateT
