module Lib.Segment where

import qualified Data.IntMap.Lazy   as IM
import qualified Data.Word          as W
import           Lib.Computer.Types (RAMMap)

type Segment = [W.Word32]

loadSegment :: Int -> Segment -> RAMMap
loadSegment starting = IM.fromAscList . zip addr
  where
    addr = iterate (+ 4) starting
