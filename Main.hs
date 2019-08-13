module Main where

import Paths_bash_inline (version)
import Data.Bifunctor
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import qualified Language.Bash.Syntax as Bash
import qualified Language.Bash.Parse  as Bash
import qualified Language.Bash.Pretty as Bash
import qualified Options.Applicative  as Opts
import qualified System.Environment   as Environment
import qualified System.Exit          as Exit
import qualified System.IO            as IO
import qualified Text.Parsec.Error    as Parsec              
import           Text.Replace                        (Replace(..), replaceWithList)

data Args = Args {
    argOneLine :: !Bool
  , argFile    :: Maybe String
  } 

data Script = Script {
    scriptSource  :: String
  , scriptContent :: String
  } deriving Show

printStatements :: Bool -> Bash.List -> IO ()
printStatements oneLine statements = do
  let src = show $ Bash.pretty statements
  if oneLine
    then putStrLn $ replaceWithList [Replace "\n" " ", Replace "\t" " ", Replace "\r" ""] src 
    else putStrLn src 

parseScript :: Script -> Either Parsec.ParseError Bash.List 
parseScript (Script source content) = Bash.parse source content

getScript :: Maybe String -> IO Script 
getScript (Just path) = Script path <$> IO.readFile path 
getScript (Nothing)   = Script "stdin" <$> IO.hGetContents IO.stdin

runWithArgs :: Args -> IO ()
runWithArgs (Args oneLine mfile) = do
  script <- getScript mfile 
  case parseScript script of
    Left err         -> print err >> Exit.exitFailure 
    Right statements -> printStatements oneLine statements

main :: IO ()
main = Opts.execParser optsParser >>= runWithArgs
  where
    optsParser :: Opts.ParserInfo Args
    optsParser = Opts.info (Opts.helper <*> versionOpt <*> programOpts) $
         Opts.fullDesc 
      <> Opts.header "bash-inline" 
      <> Opts.progDesc "Inline a Bash script to standard output, by default read from standard input."

    versionOpt :: Opts.Parser (a -> a)
    versionOpt = Opts.infoOption ("bash-inline " ++ (showVersion version)) $ 
         Opts.long "version" 
      <> Opts.short 'v' 
      <> Opts.help "Output version information and exit"

    oneLineOpt :: Opts.Parser Bool
    oneLineOpt = Opts.switch $
          Opts.long "one-line" 
       <> Opts.short 'o'
       <> Opts.help "Output script in one line"

    fileOpt :: Opts.Parser (Maybe String)
    fileOpt = Opts.optional . Opts.strOption $
          Opts.long "file" 
       <> Opts.short 'f' 
       <> Opts.metavar "FILE" 
       <> Opts.help "Bash script FILE to read instead of standard input"

    programOpts :: Opts.Parser Args
    programOpts = Args <$> oneLineOpt <*> fileOpt
