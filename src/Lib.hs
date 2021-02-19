module Lib (loadProgram) where

import qualified Data.IntMap.Lazy as IM
import Lib.Registers
import Lib.Memory
import Lib.Segment
import qualified Lib.File as F

type Program = (Registers, Memory)

loadProgram :: FilePath -> IO Program
loadProgram fp = do
    textSegment <- F.readFile $ fp <> ".text"
    dataSegment <- F.readFile $ fp <> ".data"
    let memory =  IM.filter (/= 0) $ loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
    return (startingRegisters, memory)
