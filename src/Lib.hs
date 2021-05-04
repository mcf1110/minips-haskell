module Lib
  ( decode
  , run
  , runTrace
  ) where

import           Lib.Computer          (initialComputer)
import           Lib.Computer.Types    (Computer, memTrace, stats)
import           Lib.Decode            (Program, decodeProgram)
import qualified Lib.File              as F
import           Lib.Print
import           Lib.Run
import           Lib.Segment

import           Control.Exception     (IOException, try)
import           Control.Monad         (when, (>=>))
import           Data.Either           (fromRight)
import           Data.Maybe            (fromJust, fromMaybe, isJust)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO             (IOMode (WriteMode), hClose, openFile)

run :: Int -> FilePath -> IO ()
run conf path = do
  startTime <- getPOSIXTime
  computer <- loadComputer False (floor startTime) conf path
  runComputer Nothing startTime computer
  return ()

runTrace :: Int -> FilePath -> IO ()
runTrace conf path = do
  startTime <- getPOSIXTime
  computer <- loadComputer True (floor startTime) conf path
  f <- openFile "minips.trace" WriteMode
  runComputer (Just f) startTime computer
  hClose f
  return ()

loadComputer :: Bool -> Int -> Int -> FilePath -> IO Computer
loadComputer shouldTrace seed conf fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  roDataSegment <-
    fromRight [] <$>
    (try $ F.readFile $ fp <> ".rodata" :: IO (Either IOException Segment))
  return $
    initialComputer shouldTrace seed conf dataSegment textSegment roDataSegment

decode :: FilePath -> IO ()
decode = readSegment >=> printProgramWithHexes

readSegment :: FilePath -> IO Segment
readSegment fp = F.readFile $ fp <> ".text"
