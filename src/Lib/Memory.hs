module Lib.Memory where

import qualified Data.BitVector     as BV
import qualified Data.IntMap.Lazy   as IM
import qualified Data.Word          as W

import           Data.List.Split    (chunksOf)
import           Data.Maybe         (fromMaybe)
import           Lib.Computer.Types (Computer, Memory, mem)
import           Optics             (over, (^.))

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Computer -> Computer
set ix v = over mem $ IM.insert (fromEnum ix) v

get :: Enum a => a -> Computer -> W.Word32
get n comp = fromMaybe 0 $ (comp ^. mem) IM.!? fromEnum n

getQuarter :: Enum a => a -> Computer -> W.Word8
getQuarter n m = toEnum $ fromEnum $ quarterWords !! i
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    quarterWords =
      let bv = BV.bitVec 32 (get s m)
       in [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]

getString :: Enum a => a -> Computer -> String
getString n m =
  map (toEnum . fromEnum) $ takeWhile (/= 0) $ [getQuarter a m | a <- [ix ..]]
  where
    ix = fromEnum n
