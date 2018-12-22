module Language.Imp.Syntax.Lexer
  ( space
  , integer
  , boolean
  , semicolon
  , comma
  , equal
  , lequal
  , plus
  , divide
  , not
  , and
  , parentheses
  , braces
  , identifier
  , intLex
  , ifLex
  , elseLex
  , whileLex
  , emptyLex )
where

import Prelude hiding ( and, not )

import           Data.Void
import           Data.Text ( Text )
import qualified Data.Text as T

import           Text.Megaparsec      ( Parsec, try, notFollowedBy, many, between, (<|>) )
import           Text.Megaparsec.Char ( space1, alphaNumChar, string, letterChar  )
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void Text

--------------
-- Lexicals --

space = Lex.space space1 lineComment blockComment
  where
    lineComment  = Lex.skipLineComment "//"
    blockComment = Lex.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

parentheses :: Parser a -> Parser a
parentheses = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

symbol :: Text -> Parser Text
symbol = Lex.symbol space

integer :: Parser Integer
integer = Lex.signed space $ lexeme Lex.decimal

boolean :: Parser Bool
boolean = (rword "true" *> pure True <|> rword "false" *> pure False )

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*>  many alphaNumChar
    check x = if x `elem` map T.unpack rwords
                then fail $ "keyword " ++ show x
                     ++ " cannot be an identifier"
                else return x


--------------------
-- Reserved Words --

intLex, ifLex, elseLex, whileLex :: Parser ()
intLex   = rword "int"
ifLex    = rword "if"
elseLex  = rword "else"
whileLex = rword "while"

emptyLex = rword "{}"

---------------
-- Operators --
semicolon, comma, equal, not, and, lequal, plus, divide :: Parser Text
semicolon = symbol ";"
comma     = symbol ","
equal     = symbol "="
not       = symbol "!"
and       = symbol "&&"
lequal    = symbol "<="
plus      = symbol "+"
divide    = symbol "/"

-------------
-- Helpers --
rword :: Text -> Parser ()
rword w
  = lexeme
  $ try
  $ string w
  *> notFollowedBy alphaNumChar

rwords :: [Text]
rwords = ["if","else","while", "int", "true", "false"]

{-
---* Maude syntax module* ---

mod IMP-SYNTAX is including INT + BOOL + ID .
--- AExp
  sort AExp .  subsorts Int Id < AExp .
  op _+_ : AExp AExp -> AExp [prec 33 gather (E e) format (d b o d)] .
  op _/_ : AExp AExp -> AExp [prec 31 gather (E e) format (d b o d)] .
--- BExp
  sort BExp .  subsort Bool < BExp .
  op _<=_ : AExp AExp -> BExp  [prec 37 format (d b o d)] .
  op !_ : BExp -> BExp [prec 53 format (b o d)] .
  op _&&_ : BExp BExp -> BExp [prec 55 gather (E e) format (d b o d)] .
--- Block and Stmt
  sorts Block Stmt .  subsort Block < Stmt .
  op {} : -> Block [format (b b o)] .
  op {_} : Stmt -> Block [format (d n++i n--i d)] .
  op _=_; : Id AExp -> Stmt [prec 40 format (d b o b o)] .
  op __ : Stmt Stmt -> Stmt [prec 60 gather (e E) format (d ni d)] .
  op if(_)_else_ : BExp Block Block -> Stmt [prec 59 format (b so d d s nib o d)] .
  op while(_)_ : BExp Block -> Stmt [prec 59 format (b so d d s d)] .
--- Pgm
  sort Pgm .
  op int_;_ : List{Id} Stmt -> Pgm [prec 70 format (nb o d ni d)] .
endm
-}
