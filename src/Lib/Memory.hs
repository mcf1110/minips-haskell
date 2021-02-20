module Lib.Memory where

import qualified Data.BitVector   as BV
import qualified Data.IntMap.Lazy as IM
import qualified Data.Word        as W
import           Debug.Trace

import           Data.List.Split  (chunksOf)
import           Data.Maybe       (fromMaybe)

type Memory = IM.IntMap W.Word32

get :: Enum a => a -> Memory -> W.Word32
get n m = fromMaybe 0 $ m IM.!? (fromEnum n)

getString :: Enum a => a -> Memory -> String
getString n m = map (toEnum . fromEnum . snd) $ qs
  where
    bits = 8 * byte
    byte = mod (fromEnum n) 4
    aligned = (4 * (div (fromEnum n) 4))
    starts = [aligned,(aligned + 4) ..]
    qs =
      takeWhile (\(_, v) -> v /= 0) $
      filter (\(a, _) -> a >= (fromEnum n)) $
      concat [zip [s + 24, s + 16, s + 8, s] $ quarterWords s | s <- starts]
    quarterWords s =
      [(bv BV.@: ix) | ix <- (map reverse) $ chunksOf 8 [0 .. 31]]
      where
        bv = BV.bitVec 32 (get s m)
