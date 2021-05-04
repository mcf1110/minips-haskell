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
import           Control.Monad         ((>=>))
import           Data.Either           (fromRight)
import           Data.Maybe            (fromMaybe)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Optics                ((%), (^.))

run :: Int -> FilePath -> IO ()
run conf path = do
  startTime <- getPOSIXTime
  computer <- loadComputer (floor startTime) conf path
  runComputer startTime computer
  return ()

runTrace :: Int -> FilePath -> IO ()
runTrace conf path = do
  startTime <- getPOSIXTime
  computer <- loadComputer (floor $ realToFrac $ startTime) conf path
  finalComputer <- runComputer startTime computer
  writeTraceToFile finalComputer

loadComputer :: Int -> Int -> FilePath -> IO Computer
loadComputer seed conf fp = do
  dataSegment <- F.readFile $ fp <> ".data"
  textSegment <- F.readFile $ fp <> ".text"
  roDataSegment <-
    fromRight [] <$>
    (try $ F.readFile $ fp <> ".rodata" :: IO (Either IOException Segment))
  return $ initialComputer seed conf dataSegment textSegment roDataSegment

decode :: FilePath -> IO ()
decode = readSegment >=> printProgramWithHexes

readSegment :: FilePath -> IO Segment
readSegment fp = F.readFile $ fp <> ".text"

writeTraceToFile :: Computer -> IO ()
writeTraceToFile c = do
  let trace = reverse $ c ^. (stats % memTrace)
  writeFile "my-minips.trace" $ unlines $ printTrace <$> trace
