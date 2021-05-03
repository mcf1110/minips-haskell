module Lib.Memory.Pure where

import qualified Data.BitVector     as BV
import qualified Data.IntMap        as IM
import           Data.List.Split    (chunksOf)
import           Data.Maybe
import qualified Data.Word          as W
import           Lib.Computer.Types
import           Optics             (modifying, over, (^.))

pureGet :: Enum i => i -> Computer -> W.Word32
pureGet n comp = fromMaybe 0 $ (comp ^. ram) IM.!? fromEnum n

pureGetQuarter :: Enum i => i -> Computer -> W.Word8
pureGetQuarter n comp = toEnum $ fromEnum $ quarterWords !! i
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    quarterWords =
      let bv = BV.bitVec 32 (pureGet s comp)
       in [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]

pureSet :: Enum i => i -> W.Word32 -> Computer -> Computer
pureSet ix v = over ram (IM.insert (fromEnum ix) v)
