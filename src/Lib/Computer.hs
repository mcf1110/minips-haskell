module Lib.Computer where

import qualified Data.IntMap.Lazy   as IM

import qualified Data.Vector        as V
import qualified Data.Word          as W
import           Lib.Computer.Types
import           Lib.Segment

initialComputer :: Segment -> Segment -> Segment -> Computer
initialComputer dataSegment textSegment roDataSegment =
  Computer initialRegisters initialMemory initialStats
  where
    initialMemory =
      IM.filter (/= 0) $
      loadSegment 0x00800000 roDataSegment <>
      loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
    initialRegisters :: Registers
    initialRegisters = Registers gpr coprocessor flags
      where
        gpr =
          V.replicate 35 0 V.//
          [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]
        coprocessor = V.replicate 32 0
        flags = V.replicate 8 False
    initialStats :: Stats
    initialStats = Stats counter [] 0
      where
        counter = InstructionCounter 0 0 0 0 0
