module Lib
  ( loadProgram
  , loadComputer
  ) where

import           Lib.Computer
import           Lib.Decode
import           Lib.Memory
import           Lib.Registers
import           Lib.Segment

import qualified Lib.File      as F

loadComputer :: FilePath -> IO Computer
loadComputer fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  return $ initialComputer dataSegment textSegment

loadProgram :: FilePath -> IO Program
loadProgram fp = fmap decodeProgram $ F.readFile $ fp <> ".text"
