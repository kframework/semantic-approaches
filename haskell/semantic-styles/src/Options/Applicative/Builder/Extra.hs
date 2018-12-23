module Options.Applicative.Builder.Extra
  ( enumFlag
  , maybeStrOption
  )
where

import Data.String ( IsString )
import Options.Applicative ( Parser, flag, flag', strOption, long, short, help
                           , Mod, OptionFields
                           , (<|>) )

-- | optparse helpers

enumFlag :: [(value, Maybe Char, String, String)] -> Parser value
enumFlag (defaultOpt:optList) = foldl (\l r -> buildFlag r <|> l) defaultFlag optList
  where buildFlag (val, Nothing, name, helpTxt)
          = flag' val (long name <> help helpTxt)
        buildFlag (val, Just shortName, name, helpTxt)
          = flag' val (short shortName <> long name <> help helpTxt)
         
        buildDefFlag (val, Nothing, name, helpTxt)
          = flag val val (long name <> help helpTxt)
        buildDefFlag (val, Just shortName, longName, helpTxt)
          = flag val val (short shortName <> long longName <> help helpTxt) 

        defaultFlag = buildDefFlag defaultOpt


maybeStrOption :: IsString s => Mod OptionFields s -> Parser (Maybe s)
maybeStrOption options = Just <$> (strOption options) <|> pure Nothing 
