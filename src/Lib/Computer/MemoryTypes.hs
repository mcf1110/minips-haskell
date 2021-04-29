{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.MemoryTypes where

import qualified Data.IntMap as IM
import qualified Data.Word   as W
import           Optics      (makeLenses)

type RAMMap = IM.IntMap W.Word32

data MemInfo =
  MemInfo
    { _hits  :: Int
    , _total :: Int
    }

data Memory =
  RAM
    { _info :: MemInfo
    , _im   :: RAMMap
    }

makeLenses ''MemInfo

makeLenses ''Memory
