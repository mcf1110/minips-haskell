module Lib
  ( loadProgram
  , loadState
  ) where

import qualified Data.IntMap.Lazy as IM

import Lib.Decode
import Lib.Memory
import Lib.Registers
import Lib.Segment
import Lib.State

import qualified Lib.File as F

loadState :: FilePath -> IO State
loadState fp = do
  textSegment <- F.readFile $ fp <> ".text"
  dataSegment <- F.readFile $ fp <> ".data"
  let memory =
        IM.filter (/= 0) $
        loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
  return (startingRegisters, memory)

loadProgram :: FilePath -> IO Program
loadProgram fp = fmap decodeProgram $ F.readFile $ fp <> ".text"
