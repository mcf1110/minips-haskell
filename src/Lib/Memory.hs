module Lib.Memory where

import qualified Data.BitVector   as BV
import qualified Data.IntMap.Lazy as IM
import qualified Data.Word        as W
import           Debug.Trace

import           Data.List.Split  (chunksOf)
import           Data.Maybe       (fromMaybe)

type Memory = IM.IntMap W.Word32

get :: Enum a => a -> Memory -> W.Word32
get n m = fromMaybe 0 $ m IM.!? fromEnum n

getQuarter :: Enum a => a -> Memory -> W.Word8
getQuarter n m = toEnum $ fromEnum $ quarterWords !! i
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    quarterWords =
      let bv = BV.bitVec 32 (get s m)
       in [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]

getString :: Enum a => a -> Memory -> String
getString n m =
  map (toEnum . fromEnum) $ takeWhile (/= 0) $ [getQuarter a m | a <- [ix ..]]
  where
    ix = fromEnum n
