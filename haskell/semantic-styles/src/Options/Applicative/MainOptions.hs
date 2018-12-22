module Options.Applicative.MainOptions
 ( Language(..)
 , SemanticModel(..)
 , Method(..)
 , MainOptions(..)
 , commandLineParse
 )
where

import Options.Applicative
import Options.Applicative.Builder.Extra



------------------------------------
-- Parsing Command-Line Arguments --

data Language
  = Imp | ImpPP

data SemanticModel
  = SmallStep | BigStep

data Method
  = Interpreter | Execute

data MainOptions
  = MainOptions
    { language             :: Language
    , semanticModel        :: SemanticModel
    , interpreterOrExecute :: Method
    , sourceFile           :: String
    , inputFile            :: Maybe String
    }

commandLineParse :: IO MainOptions
commandLineParse = execParser commandLineParserInfo

commandLineParserInfo :: ParserInfo MainOptions
commandLineParserInfo
  = info commandLineParser $ fullDesc
  <> header "semantic-styles - a framework for exploring implementations of languages and semantic models"
  <> progDesc "Parses then Executes or Interprets a variety of languages using a variety of semantic models"

commandLineParser :: Parser MainOptions
commandLineParser = MainOptions
  <$> enumFlag [ (Imp,   Nothing, "imp",  "Select IMP language implementation [default]") 
               , (ImpPP, Nothing, "imp++","Select IMP++ language implementation"        )
               ]
  <*> enumFlag [ (SmallStep, Just 't', "transition", "Select transition semantic model [default]")
               , (BigStep,   Just 'n', "natural",    "Select natural semantic model"             )
               ]
  <*> enumFlag [ (Execute, Just 'e', "execute"
                 , "Execute a source file with the given language and semantic model [default]")
               , (Interpreter, Just 'i', "interpreter"
                 , "Run an interpreter for the given language and semantic model")
               ]
  <*> strArgument (metavar "SRCFILE")
  <*> maybeStrOption (  long "input"
                     <> metavar "INPUTFILE"
                     <> help "File containing input to Program.  [optional]")
  <**> helper
