module Lib.Registers where

import qualified Data.Vector as V
import qualified Data.Word   as W

type Registers = V.Vector W.Word32

startingRegisters :: Registers
startingRegisters =
  V.replicate 35 0 V.// [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]

get :: Enum a => a -> Registers -> W.Word32
get ix r = r V.! (fromEnum ix)

set :: Enum a => a -> W.Word32 -> Registers -> Registers
set ix v r = r V.// [((fromEnum ix), v)]
