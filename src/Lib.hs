module Lib (loadProgram, printState) where

import qualified Data.IntMap.Lazy as IM
import Lib.Registers
import Lib.Memory
import Lib.Segment
import qualified Lib.File as F

import Data.Maybe (fromMaybe)

type State = (Registers, Memory)

loadProgram :: FilePath -> IO State
loadProgram fp = do
    textSegment <- F.readFile $ fp <> ".text"
    dataSegment <- F.readFile $ fp <> ".data"
    let memory =  IM.filter (/= 0) $ loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment
    return (startingRegisters, memory)

printState :: State -> IO ()
printState (r,m) = do
  mapM_ putStrLn $ zipWithDefault (showMemory m) (showRegisters r)
  where
  -- based on https://stackoverflow.com/a/21350444
  memSpacing = '\t':(replicate 23 ' ') <> "\t"
  infPad ls = (map Just ls) <> (repeat Nothing)
  zipWithDefault xs ys = 
    map (\(x,y) -> (fromMaybe memSpacing x <> fromMaybe "" y)) $ 
      takeWhile (/= (Nothing, Nothing)) $ 
      zip (infPad xs) (infPad ys)
