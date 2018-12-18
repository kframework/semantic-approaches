-- This module defines a parser for imp. In doing so, it defines the syntax for the language.
-- I followd a guide at https://wiki.haskell.org/Parsing_a_simple_imperative_language for how to build the parser.
module Parser (parseString, parseFile) where
    import System.IO
    import Control.Monad
    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Expr
    import Text.ParserCombinators.Parsec.Language
    import qualified Text.ParserCombinators.Parsec.Token as Token

    import Imp_Syntax

    languageDef =
        emptyDef    {   Token.commentStart      = "",
                        Token.commentEnd        = "",
                        Token.commentLine       = "",
                        Token.identStart        = letter,
                        Token.identLetter       = alphaNum,
                        Token.reservedNames     = [
                                                    "if",
                                                    "else",
                                                    "while",
                                                    "true",
                                                    "false",
                                                    "int",
                                                    "skip",
                                                    "read",
                                                    "print"
                                                ],
                       Token.reservedOpNames    = ["+", "/", "<=", "!", "&&", "="]
                    }

    lexer = Token.makeTokenParser languageDef

    -- Definitions for convenience
    identifier = Token.identifier lexer
    reserved   = Token.reserved   lexer
    reservedOp = Token.reservedOp lexer
    parens     = Token.parens     lexer
    braces     = Token.braces     lexer
    integer    = Token.integer    lexer
    semi       = Token.semi       lexer
    whiteSpace = Token.whiteSpace lexer
    commaSep   = Token.commaSep   lexer

    -- BEGIN Arithmetic Expression parser. Uses parsec's built in expression parser generator.
    aTerm :: Parser AExp
    aTerm =  parens aExpression
        <|> liftM Id identifier
        <|> liftM AConst integer
        <|> readExp

    -- NEW
    readExp :: Parser AExp
    readExp = 
        do  {
                reserved "read" ;
                parens whiteSpace;
                return $ Read
            }

    aOperators =    [   [], 
                        [Infix  (reservedOp "/"   >> return (ABinExp Divide   )) AssocLeft],
                        [Infix  (reservedOp "+"   >> return (ABinExp Plus     )) AssocLeft]
                    ]

    aExpression :: Parser AExp
    aExpression = buildExpressionParser aOperators aTerm
    -- END Arithmetic Expression parser

    -- BEGIN Boolean Expression parser. Uses parsec's built in expression parser generator.
    bTerm :: Parser BExp
    bTerm = parens bExpression
      <|> (reserved "true"  >> return (BConst True ))
      <|> (reserved "false" >> return (BConst False))
      <|> lessThanExp

    lessThanExp =
        do  {
                a1 <- aExpression ;
                reservedOp "<=" ;
                a2 <- aExpression ;
                return $ BLtEqExp a1 a2
            }

    bOperators =    [
                        [Prefix (reservedOp "!"  >> return (BNotExp))          ], 
                        [Infix  (reservedOp "&&" >> return (BAndExp)) AssocLeft]
                    ]

    bExpression :: Parser BExp
    bExpression = buildExpressionParser bOperators bTerm
    -- END Boolean Expression parser

    -- BEGIN Statement parsing
    -- A parser for each type of statement
    ifStmt :: Parser Stmt
    ifStmt =
        do  {   
                reserved "if" ;
                cond  <- bExpression ;
                stmt1 <- blockStmt ;
                reserved "else" ;
                stmt2 <- blockStmt ;
                return $ If cond stmt1 stmt2
            }

    skipStmt :: Parser Stmt
    skipStmt =
        do  {
                reserved "skip" ;
                return $ Skip
            }

    whileStmt :: Parser Stmt
    whileStmt =
        do  {
            reserved "while" ;
            cond <- bExpression ;
            stmt <- blockStmt ;
            return $ While cond stmt
        }
        
 
    assignStmt :: Parser Stmt
    assignStmt =
        do {
            var <- identifier ;
            reservedOp "=" ;
            expr <- aExpression ;
            return $ Assignment var expr
        }

    blockStmt :: Parser Stmt
    blockStmt =
        do {
            stmt <- braces statement ;
            return $ Block stmt
        }

    -- NEW
    printStmt :: Parser Stmt
    printStmt = 
        do  {
                reserved "print" ;
                expr <- parens aExpression ;
                return $ Print expr
        }

    seqComp :: Parser (Stmt -> Stmt -> Stmt)
    seqComp = 
        do {
            semi ;
            return SeqComp
        }

    -- This parses left-recursive sequential composition, using ';' as a separator between statements.
        -- Unlike in Maude, parsec runs into an infinite loop using a definition of statement = statement statement | ...
        -- And chainl is used to prevent that. A result of this is that the syntax is slightly different than the given imp,
        -- there needs to be a ; between statements.
    statement :: Parser Stmt
    statement =  chainl singleStatement seqComp Skip

    singleStatement = ifStmt
                <|> skipStmt
                <|> whileStmt
                <|> assignStmt
                <|> printStmt
                <|> blockStmt
    -- END Statements

    -- BEGIN Program parsing
    program :: Parser Pgm
    program =
        do  {
                reserved "int" ;
                vars <- commaSep identifier ;
                semi ;
                stmt <- statement ;
                return $ Init vars stmt
            }

    -- A final parser that first removes leading whitespace and then parses a program.
    parser :: Parser Pgm
    parser = whiteSpace >> program
    -- END Program parsing

    -- BEGIN Module exports
    parseString :: String -> Pgm
    parseString str = 
        case parse parser "" str of
            Left e  -> error $ show e
            Right r -> r

    parseFile :: String -> IO Pgm
    parseFile file =
        do  {
                program  <- readFile file ;
                case parse parser "" program of
                    Left e  -> fail (show e)
                    Right r -> return r
            }
    -- END Module exports