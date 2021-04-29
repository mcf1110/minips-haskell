module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  args <- take 2 <$> getArgs
  case args of
    ["decode", fileName] -> decode fileName
    ["run", fileName]    -> run 0 fileName
    ["trace", fileName]  -> runTrace 0 fileName
    _                    -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "\tminips decode program_name"
  putStrLn "\tminips run program_name"
