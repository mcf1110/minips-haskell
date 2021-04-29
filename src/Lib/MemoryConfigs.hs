module Lib.MemoryConfigs where

import           Lib.Computer.Types
import           Optics             (over, set, (.~))

configs :: [RAMMap -> Memory]
configs = setRam <$> [config2]

-- configs = setRam <$> [mkRam]
config2 :: Memory
config2 = mkUnifiedCache L1 Random 1024 1 32 mkRam

setRam :: Memory -> RAMMap -> Memory
setRam = flip (ram .~)
