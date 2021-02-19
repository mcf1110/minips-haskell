module Lib.Memory where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W

type Memory = IM.IntMap W.Word32