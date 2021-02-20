module Lib
  ( loadProgram
  , loadState
  ) where

import           Lib.Decode
import           Lib.Memory
import           Lib.Registers
import           Lib.Segment
import           Lib.State

import qualified Lib.File      as F

loadState :: FilePath -> IO State
loadState fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  return $ initialState dataSegment textSegment

loadProgram :: FilePath -> IO Program
loadProgram fp = fmap decodeProgram $ F.readFile $ fp <> ".text"
