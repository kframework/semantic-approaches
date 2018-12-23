module Language.ImpPP.Syntax.Parser
  ( parseImpPP
  , programParser
  , stmt
  , aExp
  , bExp )
where

import           Data.Void
import           Data.Text ( Text )
import qualified Data.Text as T

import Prelude hiding (and, not, print)


import Text.Megaparsec ( Parsec, try, notFollowedBy, between, eof, (<|>), many, sepBy1, parse, parseErrorPretty')

import Control.Monad.Combinators.Expr

import Language.ImpPP.Syntax.Abstract
import Language.ImpPP.Syntax.Lexer


type Parser = Parsec Void Text


------------------------
-- Source File Parser --
parseImpPP :: String -> Text -> Either String Stmt
parseImpPP file input = case parse programParser file input of
  Left  err -> Left $ parseErrorPretty' input err
  Right pgm -> Right pgm

---------------------
-- Main & Preamble --

programParser :: Parser Stmt
programParser = between space eof stmt

----------------
-- Statements --

stmt :: Parser Stmt
stmt = foldr1 compose <$> sepBy1 stmt' space
  where compose (Decl ids Empty) stmtAcc = Decl ids stmtAcc
        compose (Decl ids _)    _        = error "I've done something horribly wrong"
        compose stmtL stmtAcc            = Seq stmtL stmtAcc

stmt' :: Parser Stmt
stmt'
  =  assignStmt
 <|> whileStmt
 <|> ifStmt
 <|> printStmt
 <|> spawnStmt
 <|> declsStmt
 <|> haltStmt


assignStmt, whileStmt, ifStmt, printStmt :: Parser Stmt
assignStmt = (:=) <$> identifier <* equal <*> aExp <* semicolon
whileStmt  = While  <$ whileLex <*> parentheses bExp <*> block
ifStmt     = IfElse <$ ifLex <*> parentheses bExp <*> block <* elseLex <*> block
printStmt  = Print <$ printLex <*> parentheses aExp <* semicolon
spawnStmt  = Spawn <$ spawnLex <*> block
haltStmt   = pure Halt <* haltLex <* semicolon
declsStmt  = Decl <$> decList <*> pure Empty
--  where stmts = foldl Seq <$> pure Empty <*> many stmt' 

block :: Parser Stmt
block = emptyLex *> pure Empty <|> braces stmt

decList :: Parser DecList
decList = between
          intLex
          semicolon
          $ (:) <$> identifier
                <*> many (comma *> identifier)

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
 <|> Inc <$ increment <*> identifier
 <|> Var <$> identifier
 <|> Lit <$> integer
 <|> Read <$ readLex


-- Logical
bExp :: Parser (Exp Bool)
bExp = makeExprParser bTerm bOpTable

bOpTable :: [[Operator Parser (Exp Bool)]]
bOpTable
  = [ [ Prefix $  Not   <$ not ]
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
