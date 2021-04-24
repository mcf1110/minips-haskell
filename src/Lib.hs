module Lib
  ( decode
  , run
  ) where

import           Lib.Computer       (initialComputer)
import           Lib.Computer.Types (Computer)
import           Lib.Decode         (Program, decodeProgram)
import qualified Lib.File           as F
import           Lib.Print
import           Lib.Run
import           Lib.Segment

import           Control.Exception  (IOException, try)
import           Control.Monad      ((>=>))
import           Data.Either        (fromRight)
import           Data.Maybe         (fromMaybe)

run :: FilePath -> IO ()
run = loadComputer >=> runComputer

loadComputer :: FilePath -> IO Computer
loadComputer fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  roDataSegment <-
    fromRight [] <$>
    (try $ F.readFile $ fp <> ".rodata" :: IO (Either IOException Segment))
  return $ initialComputer dataSegment textSegment roDataSegment

decode :: FilePath -> IO ()
decode = readSegment >=> printProgramWithHexes

readSegment :: FilePath -> IO Segment
readSegment fp = F.readFile $ fp <> ".text"
