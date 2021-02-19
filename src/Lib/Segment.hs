module Lib.Segment where

import qualified Data.Word as W
import Lib.Memory
import qualified Data.IntMap.Lazy as IM

type Segment = [W.Word32]

loadSegment :: Int -> Segment -> Memory
loadSegment starting = IM.fromAscList . zip addr
    where addr = iterate (+4) starting