module ParserMain where

import qualified Data.Text as T
import Text.Megaparsec
import Language.Imp.Syntax.Parser
import Language.Imp.Programs

mainParser :: IO ()
mainParser = do
  { print program1
  ; parseTest programParser program1

  ; print sumPgm
  ; parseTest programParser sumPgm

  ; print collatzStmt
  ; parseTest stmt collatzStmt

  ; print collatzPgm
  ; parseTest programParser collatzPgm
  }
