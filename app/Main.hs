module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  (cmd, cfg, fileName) <- parseArgs . take 3 <$> getArgs
  case cmd of
    "decode" -> decode fileName
    "run"    -> run cfg fileName
    "trace"  -> runTrace cfg fileName
    _        -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "\tminips decode program_name"
  putStrLn "\tminips run program_name"
  putStrLn "\tminips trace program_name"

parseArgs :: [String] -> (String, Int, String)
parseArgs [cmd, fileName]         = (cmd, 0, fileName)
parseArgs [cmd, config, fileName] = (cmd, read config - 1, fileName)
