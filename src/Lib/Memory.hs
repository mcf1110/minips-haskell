module Lib.Memory where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word        as W

import           Data.Maybe       (fromMaybe)

type Memory = IM.IntMap W.Word32

get :: Enum a => a -> Memory -> W.Word32
get n m = fromMaybe 0 $ m IM.!? (fromEnum n)
