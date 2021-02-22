module Main where

import           Lib
import           Lib.Print
import           Lib.Run

main :: IO ()
main = do
  st <- Lib.loadComputer "./inputs/03.input"
  printComputer st
  runComputer st
