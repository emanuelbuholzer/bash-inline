module Main where

import Data.List
import Lib
import System.Directory
import Test.HUnit

-- | A very hard to read, but basic and probably not very effective test 
-- skeleton with the first value in the tuple being the script input, the 
-- second being the output in normal mode and the third being in one-line mode.
getTestSkeletons :: IO [(String, String, String)]
getTestSkeletons = do
  filePaths <- listDirectory =<< (pure . (++ "/test") =<< getCurrentDirectory)
  let scriptPaths = filter (\filePath -> ".sh" `isSuffixOf` filePath) filePaths
  let outputPaths = map (\script -> filter (\fname -> script `isPrefixOf` fname) filePaths) scriptPaths
  let testPaths   = zipWith (\scriptPath outputPaths' -> (scriptPath, head outputPaths', last outputPaths')) scriptPaths outputPaths
  mapM (\(scriptPath, outputPath, outputOnelinePath) -> do
      script <- readFile $ "test/" ++ scriptPath
      output <- readFile $ "test/" ++ outputPath
      outputOneline <- readFile $ "test/" ++ outputOnelinePath
      return (script, output, outputOneline)
    ) testPaths 

-- | Again some hard to read and probably not very effective tests.
-- This could defenitely improve, but hopefully better than nothing.
main :: IO Counts
main = do
  testSkeletons <- getTestSkeletons
  let parseFormatNormalTests = map (\(script, output, _) -> TestCase (assertEqual "script parsed and formatted normal" output (format' Normal (parse' script)))) testSkeletons
  let parseFormatOnelineTests = map (\(script, _, outputOneline) -> TestCase (assertEqual "script parsed and formatted one-line" outputOneline (format' OneLine (parse' script)))) testSkeletons
  runTestTT $ TestList (parseFormatNormalTests ++ parseFormatOnelineTests)
  where
    format' mode = format mode Nothing Nothing 
    parse' script = 
      case eitherParse script of
        Left _   -> undefined
        Right s' -> s'
