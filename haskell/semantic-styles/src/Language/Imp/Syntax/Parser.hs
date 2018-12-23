module Language.Imp.Syntax.Parser
  ( parseImp
  , programParser
  , program
  , decList
  , stmt
  , aExp
  , bExp )
where

import           Data.Void
import           Data.Text ( Text )
import qualified Data.Text as T

import Prelude hiding (and, not)


import Text.Megaparsec ( Parsec, try, notFollowedBy, many, between, eof, (<|>), sepBy1, parse, parseErrorPretty')

import Control.Monad.Combinators.Expr

import Language.Imp.Syntax.Abstract
import Language.Imp.Syntax.Lexer


type Parser = Parsec Void Text


------------------------
-- Source File Parser --
parseImp :: String -> Text -> Either String Pgm
parseImp file input = case parse programParser file input of
  Left  err -> Left $ parseErrorPretty' input err
  Right pgm -> Right pgm

---------------------
-- Main & Preamble --

programParser :: Parser Pgm
programParser = between space eof program

program :: Parser Pgm
program = Pgm <$> decList <*> stmt

decList :: Parser DecList
decList = between
          intLex
          semicolon
          $ (:) <$> identifier
                <*> many (comma *> identifier)


----------------
-- Statements --

stmt :: Parser Stmt
stmt = foldl1 Seq <$> sepBy1 stmt' space

stmt' :: Parser Stmt
stmt'
  =  assignStmt
 <|> whileStmt
 <|> ifStmt
 
assignStmt, whileStmt, ifStmt :: Parser Stmt
assignStmt = (:=) <$> identifier <* equal <*> aExp <* semicolon
whileStmt  = While  <$ whileLex <*> parentheses bExp <*> block
ifStmt     = IfElse <$ ifLex <*> parentheses bExp <*> block <* elseLex <*> block

block :: Parser Stmt
block = emptyLex *> pure Empty <|> braces stmt

-----------------
-- Expressions --

-- Arithmetic
aExp :: Parser (Exp Integer)
aExp = makeExprParser aTerm aOpTable

aOpTable :: [[Operator Parser (Exp Integer)]]
aOpTable
  = [ [ InfixR $ (:/:) <$ divide ]
    , [ InfixR $ (:+:) <$ plus ]
    ]

aTerm :: Parser (Exp Integer)
aTerm
  =  parentheses aExp
 <|> Var <$> identifier
 <|> Lit <$> integer


-- Logical
bExp :: Parser (Exp Bool)
bExp = makeExprParser bTerm bOpTable

bOpTable :: [[Operator Parser (Exp Bool)]]
bOpTable
  = [ [ Prefix $ Not <$ not ]
    , [ InfixR $ (:&&:) <$ and ]
    ]
    
bTerm :: Parser (Exp Bool)
bTerm
  =  parentheses bExp
 <|> Lit <$> boolean
 <|> rExp

-- Relations
rExp :: Parser (Exp Bool)
rExp = (:<=:) <$> aExp <* lequal <*> aExp
