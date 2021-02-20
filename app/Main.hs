module Main where

import           Lib
import           Lib.Print
import           Lib.Run

main :: IO ()
main = do
  st <- Lib.loadState "./inputs/01.soma"
  runState st
