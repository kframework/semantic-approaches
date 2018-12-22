module RunImp
  ( runImp )
where

import Prelude hiding ( readFile )
import Data.Text ( Text )
import Data.Text.IO (readFile)

import Options.Applicative.MainOptions 
import Language.Imp.Syntax.Parser ( parseImp )
import Language.Imp.Semantics.SmallStep ( runImpTrace, execPgm )

import Language.Imp.Programs

runImp :: Text -> MainOptions -> IO ()
runImp srcText options = do
  { let file = sourceFile options
        semantics = semanticModel options
  ; putStrLn $ "Parsing file \'" ++ file ++ "\'"
  ; case parseImp file srcText of
      Left errorOut -> putStr errorOut
      Right pgm -> do
        putStrLn $ "Executing file \'" ++ file ++ "\'"
        mapM_ print $ runImpTrace $ execPgm pgm
  }

