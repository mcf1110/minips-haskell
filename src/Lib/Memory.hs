module Lib.Memory where

import qualified Data.BitVector           as BV
import qualified Data.IntMap.Lazy         as IM
import qualified Data.Word                as W

import qualified Control.Monad.State.Lazy as S
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromMaybe)
import           Debug.Trace
import           Lib.Computer.Types       (Computer, Memory,
                                           MemoryTraceType (FetchInstr, Read),
                                           mem, memTrace, stats)
import           Lib.Operation.Types      (Operation)
import           Optics                   (over, (%), (^.))
import           Optics.State             (modifying)

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Operation ()
set ix v = S.modify (over mem $ IM.insert (fromEnum ix) v)

getInstruction :: Enum a => a -> Operation W.Word32
getInstruction = _addLatencyAndTrace FetchInstr _getWithoutLatency

get :: Enum a => a -> Operation W.Word32
get = _addLatencyAndTrace Read _getWithoutLatency

getQuarter :: Enum a => a -> Operation W.Word8
getQuarter = _addLatencyAndTrace Read _getQuarterWithoutLatency

_addLatencyAndTrace ::
     Enum a => MemoryTraceType -> (a -> Computer -> b) -> a -> Operation b
_addLatencyAndTrace accessType f n = do
  modifying (stats % memTrace) ((accessType, toEnum $ fromEnum n, 0) :)
  S.gets (f n)

_getWithoutLatency :: Enum a => a -> Computer -> W.Word32
_getWithoutLatency n comp = fromMaybe 0 $ (comp ^. mem) IM.!? fromEnum n

_getQuarterWithoutLatency :: Enum a => a -> Computer -> W.Word8
_getQuarterWithoutLatency n m = toEnum $ fromEnum $ quarterWords !! i
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    quarterWords =
      let bv = BV.bitVec 32 (_getWithoutLatency s m)
       in [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]

getString :: (Show a, Enum a) => a -> Operation String
getString n = do
  let address = 4 * div (fromEnum n) 4
      offset = mod (fromEnum n) 4
      toChars :: W.Word32 -> String
      toChars w32 =
        let bv = BV.bitVec 32 w32
         in drop
              offset
              [ toEnum $ fromEnum $ bv BV.@: ix
              | ix <- map reverse $ chunksOf 8 [0 .. 31]
              ]
  chars <- toChars <$> get address
  let untilNull = takeWhile (/= '\NUL') chars
  if untilNull /= chars
    then return untilNull -- we've reached the end
    else (chars <>) <$> getString (address + 4) -- do another reading
