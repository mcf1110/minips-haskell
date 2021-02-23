module Main where

import           Lib
import           Lib.Print
import           Lib.Run

main :: IO ()
main = do
  st <- Lib.loadComputer "./inputs/05.fibo"
  printComputer st
  runComputer st
