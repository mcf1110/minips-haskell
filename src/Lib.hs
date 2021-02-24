module Lib
  ( decode
  , run
  ) where

import           Lib.Computer

import           Lib.Decode    (Program, decodeProgram)
import           Lib.Memory
import           Lib.Print
import           Lib.Registers
import           Lib.Run
import           Lib.Segment

import qualified Lib.File      as F

import           Control.Monad ((>=>))

run :: FilePath -> IO ()
run = loadComputer >=> runComputer

decode :: FilePath -> IO ()
decode = loadProgram >=> printProgram

loadComputer :: FilePath -> IO Computer
loadComputer fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  return $ initialComputer dataSegment textSegment

loadProgram :: FilePath -> IO Program
loadProgram fp = fmap decodeProgram $ F.readFile $ fp <> ".text"
