module Main where

import Lib
import Lib.Print

main :: IO ()
main = do
    pgm <- Lib.loadProgram "./inputs/01.soma"
    printProgram pgm