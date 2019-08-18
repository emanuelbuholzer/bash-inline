module Main where

import Lib
import Paths_bash_inline (version)

import Data.Semigroup ((<>))
import Data.Version (showVersion)
import qualified Options.Applicative  as Opts
import System.Exit
import System.IO           

data Args = Args {
    argFile           :: Maybe FilePath
  , argOutput         :: Maybe FilePath
  , argOneLine        :: Bool
  , argLineLength     :: LineLength
  , argRibbonsPerLine :: RibbonsPerLine 
  } 

runWithArgs :: Args -> IO ()
runWithArgs (Args maybeInPath maybeOutPath oneLine lineLength ribbonsPerLine) = do
  src <- maybe getContents readFile maybeInPath 
  case eitherParse src of
    Left  err  -> putStrLn err >> exitFailure
    Right src' -> output maybeOutPath $ format (modeOf oneLine) lineLength ribbonsPerLine src'
  where
    output :: Maybe FilePath -> String -> IO ()
    output Nothing     src = putStrLn src
    output (Just path) src = writeFile path $ "#!/usr/bin/env bash\n" ++ src

    modeOf :: Bool -> Mode
    modeOf True  = OneLine
    modeOf False = Normal

main :: IO ()
main = Opts.execParser optsParser >>= runWithArgs
  where
    optsParser :: Opts.ParserInfo Args
    optsParser = Opts.info (Opts.helper <*> versionOpt <*> programOpts) $
         Opts.fullDesc 
      <> Opts.header "bash-inline - format a Bash script for inlining" 

    versionOpt :: Opts.Parser (a -> a)
    versionOpt = Opts.infoOption ("bash-inline " ++ showVersion version) $ 
         Opts.long "version" 
      <> Opts.short 'v' 
      <> Opts.help "Output version information and exit"

    oneLineOpt :: Opts.Parser Bool
    oneLineOpt = Opts.switch $
          Opts.long "one-line" 
       <> Opts.short '1'
       <> Opts.help "Output script in one line"

    lineLengthOpt :: Opts.Parser LineLength
    lineLengthOpt = Opts.optional . Opts.option Opts.auto $
          Opts.long "lineLength"
       <> Opts.short 'l'
       <> Opts.help "Maximum line length of the formatted output [80]"

    ribbonsPerLineOpt :: Opts.Parser RibbonsPerLine
    ribbonsPerLineOpt = Opts.optional . Opts.option Opts.auto $
          Opts.long "ribbonsPerLine"
       <> Opts.short 'r'
       <> Opts.help "Maximum ratio of line length to characters on a line excluding indentation [1.5]"

    fileOpt :: Opts.Parser (Maybe FilePath)
    fileOpt = Opts.optional . Opts.strOption $
          Opts.long "file" 
       <> Opts.short 'f' 
       <> Opts.help "Bash script file to read instead of standard input"

    outputOpt :: Opts.Parser (Maybe FilePath)
    outputOpt = Opts.optional . Opts.strOption $
          Opts.long "output"
       <> Opts.short 'o'
       <> Opts.help "Bash script file to output the formatted script"

    programOpts :: Opts.Parser Args
    programOpts = Args <$> fileOpt <*> outputOpt <*> oneLineOpt <*> lineLengthOpt <*> ribbonsPerLineOpt 
