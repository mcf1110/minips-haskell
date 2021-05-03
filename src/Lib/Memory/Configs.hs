module Lib.Memory.Configs where

import           Lib.Computer.Types
import           Optics             (over, set, (.~))

configs :: [Memory]
configs =
  mkConfig <$>
  [ []
  , [mkUnifiedCache L1 Random 1024 1 32]
  , [mkSplitCache L1 Random 512 1 32]
  , [mkSplitCache L1 LRU 512 1 32]
  , [mkSplitCache L1 LRU 512 4 32]
  , [mkSplitCache L1 LRU 512 4 64, mkUnifiedCache L2 LRU 2048 8 64]
  ]

config2 :: [Memory -> Memory]
config2 = [mkUnifiedCache L1 Random 1024 1 32]

config3 :: [Memory -> Memory]
config3 = [mkSplitCache L1 Random 512 1 32]

mkConfig :: [Memory -> Memory] -> Memory
mkConfig = go
  where
    go :: [Memory -> Memory] -> Memory
    go []     = mkRam
    go (m:ms) = m $ go ms
    -- TODO: ^^^ clearly a fold
