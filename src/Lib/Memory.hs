module Lib.Memory where

import qualified Data.BitVector           as BV
import qualified Data.IntMap.Lazy         as IM
import qualified Data.Word                as W

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromMaybe)
import           Debug.Trace
import           Lib.Computer.Types       (Computer, Latency, Memory (RAM),
                                           MemoryTraceType (InstrFetch, Read, Write),
                                           hits, info, mem, memTrace, nCycles,
                                           stats, total)
import           Lib.Operation.Types      (Operation)
import           Optics                   (Each (each), assign, over, (%), (.~),
                                           (^.))
import           Optics.State             (modifying)

get :: Enum i => i -> Operation W.Word32
get = updatingLatencyAndTrace Read fetchMemory

getQuarter :: Enum i => i -> Operation W.Word8
getQuarter = updatingLatencyAndTrace Read fetchQuarter

getInstruction :: Enum i => i -> Operation W.Word32
getInstruction = updatingLatencyAndTrace InstrFetch fetchMemory

getString :: Enum i => i -> Operation String
getString n = do
  chars <- toChars <$> get address
  let untilNull = takeWhile (/= '\NUL') chars
  if untilNull /= chars
    then return untilNull -- we've reached the end
    else (chars <>) <$> getString (address + 4) -- do another reading
  where
    address = 4 * div (fromEnum n) 4
    offset = mod (fromEnum n) 4
    toChars :: W.Word32 -> String
    toChars w32 =
      let bv = BV.bitVec 32 w32
       in drop
            offset
            [ toEnum $ fromEnum $ bv BV.@: ix
            | ix <- map reverse $ chunksOf 8 [0 .. 31]
            ]

set :: (Eq i, Num i, Enum i) => i -> W.Word32 -> Operation ()
set ix v = updatingLatencyAndTrace Write (writeMemory v) ix

updatingLatencyAndTrace ::
     Enum i
  => MemoryTraceType
  -> (i -> Computer -> (Latency, Memory, a))
  -> i
  -> Operation a
updatingLatencyAndTrace traceType operation ix = do
  (lat, newMem, val) <- S.gets $ operation ix
  assign mem newMem
  updateLatencyAndTrace traceType ix lat
  return val

updateLatencyAndTrace ::
     Enum i => MemoryTraceType -> i -> Latency -> Operation ()
updateLatencyAndTrace accessType n lat = do
  modifying (stats % memTrace) ((accessType, w32, w32 `div` 32) :) -- add trace
  modifying (stats % nCycles) (+ lat) -- add latency
  where
    w32 = toEnum $ fromEnum n

writeMemory :: Enum i => W.Word32 -> i -> Computer -> (Latency, Memory, ())
writeMemory v ix comp = go 0 $ comp ^. mem
  where
    go lat (RAM info im) =
      (lat + 100, countHit $ RAM info $ IM.insert (fromEnum ix) v im, ())

fetchMemory :: Enum i => i -> Computer -> (Latency, Memory, W.Word32)
fetchMemory n comp = go 0 $ comp ^. mem
  where
    go lat m@(RAM i im) =
      (lat + 100, countHit m, fromMaybe 0 $ im IM.!? fromEnum n)

countHit :: Memory -> Memory
countHit = over (info % hits) (+ 1) . countMiss

countMiss :: Memory -> Memory
countMiss = over (info % total) (+ 1)

fetchQuarter :: Enum i => i -> Computer -> (Latency, Memory, W.Word8)
fetchQuarter n m = extractQuarter <$> fetchMemory s m
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    extractQuarter v = toEnum $ fromEnum $ quarterWords !! i
      where
        bv = BV.bitVec 32 v
        quarterWords = [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]
