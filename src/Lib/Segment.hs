module Lib.Segment where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W
import Lib.Memory

type Segment = [W.Word32]

loadSegment :: Int -> Segment -> Memory
loadSegment starting = IM.fromAscList . zip addr
  where
    addr = iterate (+ 4) starting
