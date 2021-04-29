module Lib.MemoryConfigs where

import           Lib.Computer.Types
import           Optics             (over, set, (.~))

configs :: [RAMIntMap -> Memory]
configs = setRam <$> [onlyRam]

onlyRam :: Memory
onlyRam = RAM emptyInfo mempty

setRam :: Memory -> RAMIntMap -> Memory
setRam = flip (ram .~)

emptyInfo :: MemInfo
emptyInfo = MemInfo 0 0
