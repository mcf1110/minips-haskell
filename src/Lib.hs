module Lib
  ( decode
  , run
  , runTrace
  ) where

import           Lib.Computer       (initialComputer)
import           Lib.Computer.Types (Computer, memTrace, stats)
import           Lib.Decode         (Program, decodeProgram)
import qualified Lib.File           as F
import           Lib.Print
import           Lib.Run
import           Lib.Segment

import           Control.Exception  (IOException, try)
import           Control.Monad      ((>=>))
import           Data.Either        (fromRight)
import           Data.Maybe         (fromMaybe)
import           Data.Time          (getCurrentTime)
import           Optics             ((%), (^.))

run :: FilePath -> IO ()
run path = do
  computer <- loadComputer path
  startTime <- getCurrentTime
  runComputer startTime computer
  return ()

runTrace :: FilePath -> IO ()
runTrace path = do
  computer <- loadComputer path
  startTime <- getCurrentTime
  finalComputer <- runComputer startTime computer
  writeTraceToFile finalComputer

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

writeTraceToFile :: Computer -> IO ()
writeTraceToFile c = do
  let trace = reverse $ c ^. (stats % memTrace)
  writeFile "my-minips.trace" $ unlines $ printTrace <$> trace
