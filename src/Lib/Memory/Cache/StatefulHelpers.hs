module Lib.Memory.Cache.StatefulHelpers where

import qualified Control.Monad.State.Lazy     as S
import qualified Data.IntMap.Lazy             as IM
import           Data.Maybe                   (fromJust, isJust)
import qualified Data.Vector                  as V
import qualified Data.Word                    as W
import           Optics                       (Field1 (_1), Field2 (_2), Lens',
                                               assign, modifying, over, use,
                                               (%), (.~), (^.))
import           Optics.Operators.Unsafe      ((^?!))

import           Lib.Computer.Types
import           Lib.Memory.Cache.LensHelpers
import           Lib.Memory.Cache.PureHelpers

setDirtyIfNeeded ::
     Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) ()
setDirtyIfNeeded Write ix = updateOnExistingWord Write ix isDirty True
setDirtyIfNeeded _ _      = return ()

updateLatency :: S.State (Latency, Memory, [Int]) ()
updateLatency = do
  (l0, m0, rng0) <- S.get
  assign _1 (l0 + getLatency m0)

removeFromInstructionCache ::
     Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) ()
removeFromInstructionCache Write ix = do
  mem <- use _2
  case mem of
    SplitCache {} -> assign _2 mem'
      where cm = mem ^?! (instUnit % cacheMap)
            nInt = fromEnum ix
            block = lineNumber ix cm
            way = getWaysAtLine block cm
            withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
            hasAddress v = (v ^. address) == withoutOffset
            maybeIdx = V.findIndex (maybe False hasAddress) way
            newWay =
              if isJust maybeIdx
                then way V.// [(fromJust maybeIdx, Nothing)]
                else way
            mem' =
              over
                (instUnit % cacheMap % addresses)
                (IM.insert block newWay)
                mem
    _ -> return ()
removeFromInstructionCache _ _ = return ()

trySnooping ::
     Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) Bool
trySnooping mat ix = do
  memory <- use _2
  return $
    case memory of
      SplitCache {} ->
        isJust $ getCacheMapIndex (memory ^?! getOtherUnit mat % cacheMap) ix
      _ -> False

updateOnExistingWord ::
     Enum i
  => MemoryAccessType
  -> i
  -> Lens' CacheLine a
  -> a
  -> S.State (Latency, Memory, [Int]) ()
updateOnExistingWord mat addr lens val = do
  mem <- use _2
  case mem of
    RAM {} -> return ()
    _ -> assign _2 mem'
      where cm = mem ^?! (getUnit mat % cacheMap)
            line = lineNumber addr cm
            way = getWaysAtLine line cm
            idx = fromJust $ getCacheMapIndex cm addr
            newWay = way V.// [(idx, (lens .~ val) <$> way V.! idx)]
            mem' =
              over
                (getUnit mat % cacheMap % addresses)
                (IM.insert line newWay)
                mem

propagateToNext ::
     S.State (Latency, Memory, [Int]) () -> S.State (Latency, Memory, [Int]) ()
propagateToNext statefulFunction = do
  (l1, m1, rng) <- S.get
  let (v, (l2, m2, rng')) =
        S.runState statefulFunction (l1, m1 ^?! nextMem, rng) -- is safe because only Caches can have misses
  S.put (l2, (nextMem .~ m2) m1, rng')

countHit, countMiss, countTotal ::
     MemoryAccessType -> S.State (Latency, Memory, [Int]) ()
countHit mat = do
  count mat hits
  count mat total

countMiss = countTotal

countTotal mat = count mat total

count ::
     MemoryAccessType
  -> Lens' MemInfo Int
  -> S.State (Latency, Memory, [Int]) ()
count mat lens = modifying (_2 % getInfo mat % lens) (+ 1)

checkHit ::
     Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) Bool
checkHit mat n = S.gets (check . (^. _2))
  where
    check RAM {} = True
    check c      = isJust $ getCacheMapIndex (c ^?! (getUnit mat % cacheMap)) n
