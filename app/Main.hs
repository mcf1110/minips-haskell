module Main where

import Lib
import Lib.Memory
import Lib.Registers

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W
import Text.Printf (printf)
import qualified Data.Vector as V

type Segment = [W.Word32]


main :: IO ()
main = do
    (reg, mem) <- Lib.loadProgram "./inputs/03.input"
    showMemory mem
    print reg