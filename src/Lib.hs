module Lib (
  Mode(..), LineLength, RibbonsPerLine, Script,
  eitherParse,
  format
) where

import           Data.Bifunctor
import           Data.Maybe
import qualified Language.Bash.Syntax as Bash
import qualified Language.Bash.Parse  as Bash
import qualified Language.Bash.Pretty as Bash
import qualified Text.Parsec.Error    as Parsec              
import qualified Text.Parsec.Pos      as Parsec
import qualified Text.PrettyPrint     as PrettyPrint

-- | Formatting mode, either normal or one-lined output
data Mode = Normal | OneLine

-- | Maximum line length of the formatted output
type LineLength = Maybe Int

-- | Maximum ratio of line length to characters on a line excluding indentation
type RibbonsPerLine = Maybe Float

-- | A compount list of statements
type Script = Bash.List

-- | Parse a Bash script, return an error message when parsing fails.
eitherParse :: String -> Either String Script 
eitherParse content = 
  let sourceName = "<ignored>"
  in  first mapErrorToString $ Bash.parse sourceName content
  where
    mapErrorToString :: Parsec.ParseError -> String
    mapErrorToString e = 
      "error " ++ showSourcePos (Parsec.errorPos e) ++ ":" ++ showMessages (Parsec.errorMessages e)

    showSourcePos :: Parsec.SourcePos -> String
    showSourcePos pos =
      let line   = show $ Parsec.sourceLine   pos
          column = show $ Parsec.sourceColumn pos
      in  "(line " ++ line ++ ", column " ++ column ++ ")"
    
    showMessages :: [Parsec.Message] -> String
    showMessages =
      Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"

-- | Format and render a Bash script to a string
format :: Mode -> LineLength -> RibbonsPerLine -> Script -> String
format m l r script = 
  let postamble = ""
  in  PrettyPrint.fullRender (mode m) (lineLength l) (ribbonsPerLine r) printer postamble (Bash.pretty script)
  where
    mode :: Mode -> PrettyPrint.Mode
    mode Normal  = PrettyPrint.PageMode
    mode OneLine = PrettyPrint.OneLineMode

    lineLength :: LineLength -> Int
    lineLength = fromMaybe 80

    ribbonsPerLine :: RibbonsPerLine -> Float
    ribbonsPerLine = fromMaybe 1.5

    printer :: PrettyPrint.TextDetails -> String -> String
    printer (PrettyPrint.Chr  c) s  = c :  s
    printer (PrettyPrint.Str  s) s' = s ++ s'
    printer (PrettyPrint.PStr s) s' = s ++ s'
