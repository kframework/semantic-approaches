module Language.ImpPP.Syntax.Lexer
  ( space
  , integer
  , boolean
  , semicolon
  , comma
  , equal
  , lequal
  , plus
  , increment
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
  , emptyLex
  , readLex
  , printLex
  , spawnLex
  , haltLex)
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
printLex, readLex, haltLex, spawnLex :: Parser ()
intLex   = rword "int"
ifLex    = rword "if"
elseLex  = rword "else"
whileLex = rword "while"
printLex = rword "print"
readLex  = rword "read()"
haltLex  = rword "halt"
spawnLex = rword "spawn"
emptyLex = rword "{}"

---------------
-- Operators --
semicolon, comma, equal, not, and :: Parser Text
lequal, plus, divide, increment :: Parser Text
semicolon = symbol ";"
comma     = symbol ","
equal     = symbol "="
not       = symbol "!"
and       = symbol "&&"
lequal    = symbol "<="
plus      = symbol "+"
divide    = symbol "/"
increment = symbol "++"

-------------
-- Helpers --
rword :: Text -> Parser ()
rword w
  = lexeme
  $ try
  $ string w
  *> notFollowedBy alphaNumChar

rwords :: [Text]
rwords = [ "if","else","while"
         , "int", "true", "false"
         , "print", "spawn", "halt"
         , "read"]

