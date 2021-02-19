module Lib.Registers where

import qualified Data.Word as W
import qualified Data.Vector as V

type Registers = V.Vector W.Word32

startingRegisters :: Registers
startingRegisters = V.replicate 35 0 V.// 
                    [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]
