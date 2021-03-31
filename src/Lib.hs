module Lib
  ( decode
  , run
  ) where

import           Lib.Computer

import           Lib.Decode        (Program, decodeProgram)
import           Lib.Memory
import           Lib.Print
import           Lib.Registers
import           Lib.Run
import           Lib.Segment

import qualified Lib.File          as F

import           Control.Exception (SomeException (SomeException), try)
import           Control.Monad     ((>=>))
import           Data.Either

run :: FilePath -> IO ()
run = loadComputer >=> runComputer

decode :: FilePath -> IO ()
decode = loadProgram >=> printProgram

loadComputer :: FilePath -> IO Computer
loadComputer fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  roDataSegment <-
    fromRight [] <$>
    (try $ F.readFile $ fp <> ".rodata" :: IO (Either SomeException Segment))
  return $ initialComputer dataSegment textSegment roDataSegment

loadProgram :: FilePath -> IO Program
loadProgram fp = fmap decodeProgram $ F.readFile $ fp <> ".text"
