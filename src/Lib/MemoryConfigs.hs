module Lib.MemoryConfigs where

import           Lib.Computer.Types
import           Optics             (over, set, (.~))

configs :: [RAMMap -> Memory]
configs = mkConfig <$> [config2]

config2 :: [Memory -> Memory]
config2 = [mkUnifiedCache L1 Random 1024 1 32]

setRam :: Memory -> RAMMap -> Memory
setRam = flip (ram .~)

mkConfig :: [Memory -> Memory] -> RAMMap -> Memory
mkConfig = setRam . go
  where
    go :: [Memory -> Memory] -> Memory
    go []     = mkRam
    go (m:ms) = m $ go ms
    -- TODO: ^^^ clearly a fold
