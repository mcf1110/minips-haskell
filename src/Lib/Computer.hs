module Lib.Computer where

import qualified Data.IntMap.Lazy    as IM

import qualified Data.Vector.Unboxed as V
import qualified Data.Word           as W
import           Lib.Computer.Types
import           Lib.Memory.Configs  (configs)
import           Lib.Segment
import           System.Random       (mkStdGen, randoms)

initialComputer ::
     Bool -> Int -> Int -> Segment -> Segment -> Segment -> Computer
initialComputer shouldTrace seed conf dataSegment textSegment roDataSegment =
  Computer iRegisters iHierarchy iMemory iStats iRng
  where
    iMemory =
      IM.filter (/= 0) $
      loadSegment 0x00800000 roDataSegment <>
      loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
    iHierarchy = configs !! conf
    iRegisters :: Registers
    iRegisters = Registers gpr coprocessor flags
      where
        gpr =
          V.replicate 35 0 V.//
          [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]
        coprocessor = V.replicate 32 0
        flags = V.replicate 8 False
    iStats :: Stats
    iStats = Stats counter trace 0
      where
        trace =
          if shouldTrace
            then Just []
            else Nothing
        counter = InstructionCounter 0 0 0 0 0
    iRng = randoms $ mkStdGen seed
