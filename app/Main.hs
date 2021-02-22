module Main where

import           Lib
import           Lib.Print
import           Lib.Run

main :: IO ()
main = do
  st <- Lib.loadComputer "./inputs/04.branches"
  printComputer st
  runComputer st
