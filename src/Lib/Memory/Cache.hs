module Lib.Memory.Cache where

import qualified Data.BitVector           as BV
import qualified Data.IntMap.Lazy         as IM
import qualified Data.Vector              as V
import qualified Data.Word                as W

import           Control.Monad            (join, when)
import qualified Control.Monad.State.Lazy as S
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromJust, fromMaybe, isJust,
                                           isNothing)
import           Debug.Trace
import           Lib.Computer.Types
import qualified Lib.Memory.Pure          as P
import           Lib.Operation.Types      (Operation)
import           Optics                   (AffineTraversal', Field2 (_2),
                                           Field3 (_3), Lens', assign,
                                           atraversal, lens, modifying, over,
                                           set, use, view, (%), (.~), (^.))
import           Optics.Operators.Unsafe  ((^?!))

triggerInCache :: Enum i => MemoryAccessType -> i -> Operation ()
triggerInCache mat ix = do
  comp <- S.get
  -- state within a state, yay!
  let (val, (lat, newMem, newRng)) =
        S.runState (statefulFunction ix) (0, comp ^. mem, comp ^. rng)
  assign mem newMem
  assign rng newRng
  modifying (stats % nCycles) (+ lat) -- update latency
  addTrace mat ix
  where
    statefulFunction =
      case mat of
        Write      -> triggerWrite
        Read       -> triggerFetch
        InstrFetch -> triggerFetchInstruction

addTrace :: Enum i => MemoryAccessType -> i -> Operation ()
addTrace mat ix = modifying (stats % memTrace) ((mat, w32, w32 `div` 32) :)
  where
    w32 = toEnum $ fromEnum ix

triggerWrite :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerWrite ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit Write ix m0
    then do
      S.put (l1, countHit Write m0, rng0)
      updateOnExistingWord Write ix lastUsed 0
      updateOnExistingWord Write ix isDirty True
    else do
      S.put (l1, countMiss Write m0, rng0)
      propagateToNext $ triggerFetch ix
      addToCurrentCache Write ix True -- dirty, just written

triggerFetch :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerFetch ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit Read ix m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit Read m0, rng0)
      updateOnExistingWord Read ix lastUsed 0
    else do
      S.put (l1, countMiss Read m0, rng0)
      propagateToNext $ triggerFetch ix
      addToCurrentCache Read ix False -- notDirty, just loaded

triggerFetchInstruction :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerFetchInstruction ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit InstrFetch ix m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit InstrFetch m0, rng0)
      updateOnExistingWord InstrFetch ix lastUsed 0
    else do
      S.put (l1, countMiss InstrFetch m0, rng0)
      propagateToNext $ triggerFetchInstruction ix
      addToCurrentCache InstrFetch ix False -- notDirty, just loaded

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
            nInt = fromEnum addr
            block = lineNumber addr cm
            way = getWay block cm
            withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
            hasAddress v = (v ^. address) == withoutOffset
            idx = fromJust $ V.findIndex (maybe False hasAddress) way
            newWay = way V.// [(idx, (lens .~ val) <$> way V.! idx)]
            mem' =
              over
                (getUnit mat % cacheMap % addresses)
                (IM.insert block newWay)
                mem

propagateToNext ::
     S.State (Latency, Memory, [Int]) () -> S.State (Latency, Memory, [Int]) ()
propagateToNext statefulFunction = do
  (l1, m1, rng) <- S.get
  let (v, (l2, m2, rng')) =
        S.runState statefulFunction (l1, m1 ^?! nextMem, rng) -- is safe because only Caches can have misses
  S.put (l2, (nextMem .~ m2) m1, rng')

lineNumber :: Enum i => i -> CacheMap -> Int
lineNumber ix cm = ifCacheWasInfinite `mod` linesInCache
  where
    addr = fromEnum ix
    linesInCache = cm ^. nLines
    wordsInALine = cm ^. wordsPerLine
    bytesInAWord = 4
    ifCacheWasInfinite = addr `div` (bytesInAWord * wordsInALine)

addToCurrentCache ::
     Enum i
  => MemoryAccessType
  -> i
  -> Bool
  -> S.State (Latency, Memory, [Int]) ()
addToCurrentCache mat ix setDirty = do
  (lat, memory, rng) <- S.get
  let cm = memory ^?! (getUnit mat % cacheMap)
      block = lineNumber ix cm
      w0 = getWay block cm
      firstEmptyIndex = V.findIndex isNothing w0
      (strategyIndex, rng') =
        selectIndexThroughStrategy (memory ^?! strategy) w0 rng
      selectedIndex = fromMaybe strategyIndex firstEmptyIndex
      newCacheLine =
        CacheLine
          { _isDirty = setDirty
          , _lastUsed = 0
          , _address = fromEnum ix `div` (cm ^. wordsPerLine * 4)
          }
      w1 = w0 V.// [(selectedIndex, Just newCacheLine)]
      w2 = fmap (over lastUsed (+ 1)) <$> w1
      updateAddresses = IM.insert block w2
      justDeleted = w0 V.! selectedIndex
      shouldWriteBack = maybe False (^. isDirty) justDeleted
      writeBack =
        propagateToNext $
        triggerWrite
          (fromJust justDeleted ^. address * (cm ^. wordsPerLine * 4))
  modifying _2 (over (getUnit mat % cacheMap % addresses) updateAddresses)
  assign _3 rng'
  when shouldWriteBack writeBack

selectIndexThroughStrategy ::
     CacheStrategy -> V.Vector (Maybe CacheLine) -> [Int] -> (Int, [Int])
selectIndexThroughStrategy Random ways (r:rs) = (abs r `mod` V.length ways, rs)
selectIndexThroughStrategy LRU ways rng =
  (V.maxIndex $ V.mapMaybe (fmap (^. lastUsed)) ways, rng)
    --mapMaybe is equivalent to map, because none of the positions will be empty anyway

isHit :: Enum i => MemoryAccessType -> i -> Memory -> Bool
isHit mat n RAM {} = True
isHit mat n c = isJust $ V.findIndex (maybe False hasAddress) way
  where
    cm = c ^?! (getUnit mat % cacheMap)
    nInt = fromEnum n
    block = lineNumber n cm
    way = getWay block cm
    withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
    hasAddress v = (v ^. address) == withoutOffset

getWay :: Int -> CacheMap -> V.Vector (Maybe CacheLine)
getWay block cm =
  fromMaybe (V.replicate (cm ^. nWays) Nothing) $ (cm ^. addresses) IM.!? block

countHit :: MemoryAccessType -> Memory -> Memory
countHit mat = over (getInfo mat % hits) (+ 1) . countMiss mat

countMiss :: MemoryAccessType -> Memory -> Memory
countMiss mat = over (getInfo mat % total) (+ 1)

getInfo :: MemoryAccessType -> AffineTraversal' Memory MemInfo
getInfo mat =
  atraversal
    (\mem -> Right (mem ^?! l mem))
    (\mem mInfo -> set (l mem) mInfo mem)
  where
    l :: Memory -> AffineTraversal' Memory MemInfo
    l RAM {} = ramInfo
    l _      = getUnit mat % info

getUnit :: MemoryAccessType -> AffineTraversal' Memory CacheUnit
getUnit mat =
  atraversal
    (\mem -> Right (mem ^?! l mem))
    (\mem mInfo -> set (l mem) mInfo mem)
  where
    l :: Memory -> AffineTraversal' Memory CacheUnit
    l Cache {} = unit
    l SplitCache {} =
      if mat == InstrFetch
        then instUnit
        else dataUnit
    l RAM {} = undefined
