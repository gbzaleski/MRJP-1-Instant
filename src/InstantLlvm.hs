module Main where
-- Grzegorz B. Zaleski (418494)

import System.IO ( hPutStrLn, stderr )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant as I
import Llvm as L

getFilePath :: String -> String
getFilePath fs = take (length fs - 4) fs -- 4 for ".ins"

runFile :: FilePath -> IO()
runFile fs = readFile fs >>= runProgram (getFilePath fs)

runProgram :: String -> String -> IO ()
runProgram filepath s = case pProgram tokenised of
    Left parseError -> do
      hPutStrLn stderr $ "Programme parsing failure!\n" ++ parseError
      exitFailure
    Right programmeTree -> L.runInstant programmeTree filepath
  where
    tokenised = myLexer s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Instant file not given!"
    fs -> mapM_ runFile fs