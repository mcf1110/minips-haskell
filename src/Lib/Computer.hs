module Lib.Computer where

import qualified Data.IntMap.Lazy as IM

import           Lib.Memory
import           Lib.Registers
import           Lib.Segment

type Computer = (Registers, Memory)

initialComputer :: Segment -> Segment -> Segment -> Computer
initialComputer dataSegment textSegment roDataSegment =
  (startingRegisters, startingMemory)
  where
    startingMemory =
      IM.filter (/= 0) $
      loadSegment 0x00800000 roDataSegment <>
      loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
