module Main where

import           Lib
import           Lib.Print
import           Lib.Run

main :: IO ()
main = do
  computer <- loadComputer "./inputs/08.sort"
  printComputer computer
  runComputer computer
