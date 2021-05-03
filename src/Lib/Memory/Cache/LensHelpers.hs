module Lib.Memory.Cache.LensHelpers where

import           Optics                  (AffineTraversal', atraversal, set,
                                          (%))
import           Optics.Operators.Unsafe ((^?!))

import           Lib.Computer.Types

getInfo :: MemoryAccessType -> AffineTraversal' Memory MemInfo
getInfo mat = mkAffine l
  where
    l :: Memory -> AffineTraversal' Memory MemInfo
    l RAM {} = ramInfo
    l _      = getUnit mat % info

getUnit, getOtherUnit :: MemoryAccessType -> AffineTraversal' Memory CacheUnit
getUnit mat = mkAffine l
  where
    l Cache {} = unit
    l SplitCache {} =
      if mat == InstrFetch
        then instUnit
        else dataUnit
    l RAM {} = error "RAM does not have Unit, should never be called"

getOtherUnit mat = mkAffine l
  where
    l SplitCache {} =
      if mat /= InstrFetch
        then instUnit
        else dataUnit
    l _ = error "RAM/Unified does not have OtherUnit, should never be called"

mkAffine :: (s -> AffineTraversal' s a) -> AffineTraversal' s a
mkAffine l =
  atraversal
    (\mem -> Right (mem ^?! l mem))
    (\mem mInfo -> set (l mem) mInfo mem)
