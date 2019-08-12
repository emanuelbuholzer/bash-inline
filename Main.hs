module Main where

import Paths_bash_inline (version)
import Data.Version (showVersion)
import qualified Language.Bash.Syntax as Bash
import qualified Language.Bash.Parse  as Bash
import qualified Language.Bash.Pretty as Bash
import qualified System.Environment   as Environment
import qualified System.Exit          as Exit
import qualified System.IO            as IO

printScript :: Bash.List -> IO ()
printScript src = print (Bash.pretty src) 

printError :: Show a => a -> IO ()
printError err = print err >> Exit.exitFailure

inlineScript :: String -> IO.Handle -> IO ()
inlineScript script hdl = do
  parseResult <- Bash.parse script <$> IO.hGetContents hdl 
  either printError printScript parseResult

printUsage :: IO ()
printUsage = putStrLn $
  "Usage: bash-inline [FILE]\n" ++
  "Inline a Bash script FILE, or standard input, to standard output.\n\n" ++
  "  --help     display this usage and exit\n" ++
  "  --version  output version information and exit\n\n" ++
  "With no FILE, read standard input."
  
main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    ["--help"]    -> printUsage >> Exit.exitFailure
    ["--version"] -> putStrLn $ "bash-inline " ++ showVersion version
    [path]        -> IO.openFile path IO.ReadMode >>= inlineScript path
    []            -> inlineScript "stdin" IO.stdin
