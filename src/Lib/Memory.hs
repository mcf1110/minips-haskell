module Lib.Memory where

import qualified Data.BitVector           as BV
import qualified Data.IntMap.Lazy         as IM
import qualified Data.Word                as W

import qualified Control.Monad.State.Lazy as S
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromMaybe)
import           Lib.Computer.Types       (Computer, Memory, mem)
import           Lib.Operation.Types      (Operation)
import           Optics                   (over, (^.))

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Operation ()
set ix v = S.modify (over mem $ IM.insert (fromEnum ix) v)

get :: Enum a => a -> Operation W.Word32
get n = S.gets (_getWithoutLatency n)

getQuarter :: Enum a => a -> Operation W.Word8
getQuarter n = S.gets (_getQuarterWithoutLatency n)

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

_getStringWithoutLatency :: Enum a => a -> Computer -> String
_getStringWithoutLatency n m =
  map (toEnum . fromEnum) $
  takeWhile (/= 0) $ [_getQuarterWithoutLatency a m | a <- [ix ..]]
  where
    ix = fromEnum n
