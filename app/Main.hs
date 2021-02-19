module Main where

import Lib
import Lib.Memory
import Lib.Registers
import Lib.Operations
import Lib.Decode

main :: IO ()
main = do
    -- state <- Lib.loadProgram "./inputs/01.soma"
    -- printState state
    print $ wordToBV 0x00af8020