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

triggerInCache :: Enum i => MemoryTraceType -> i -> Operation ()
triggerInCache mtt ix = do
  comp <- S.get
  -- state within a state, yay!
  let (val, (lat, newMem, newRng)) =
        S.runState (statefulFunction ix) (0, comp ^. mem, comp ^. rng)
  assign mem newMem
  assign rng newRng
  modifying (stats % nCycles) (+ lat) -- update latency
  addTrace mtt ix
  where
    statefulFunction =
      case mtt of
        Write      -> triggerWrite
        Read       -> triggerFetch
        InstrFetch -> triggerFetchInstruction

addTrace :: Enum i => MemoryTraceType -> i -> Operation ()
addTrace mtt ix = modifying (stats % memTrace) ((mtt, w32, w32 `div` 32) :)
  where
    w32 = toEnum $ fromEnum ix

triggerWrite :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerWrite ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
    then do
      S.put (l1, countHit m0, rng0)
      updateOnExistingWord ix lastUsed 0
      updateOnExistingWord ix isDirty True
    else do
      S.put (l1, countMiss m0, rng0)
      propagateToNext $ triggerFetch ix
      addToCurrentCache ix True -- dirty, just written

triggerFetch :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerFetch ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit m0, rng0)
      updateOnExistingWord ix lastUsed 0
    else do
      S.put (l1, countMiss m0, rng0)
      propagateToNext $ triggerFetch ix
      addToCurrentCache ix False -- notDirty, just loaded

triggerFetchInstruction :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
triggerFetchInstruction ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit m0, rng0)
      updateOnExistingWord ix lastUsed 0
    else do
      S.put (l1, countMiss m0, rng0)
      propagateToNext $ triggerFetchInstruction ix
      addToCurrentCache ix False -- notDirty, just loaded

updateOnExistingWord ::
     Enum i
  => i
  -> Lens' CacheLine a
  -> a
  -> S.State (Latency, Memory, [Int]) ()
updateOnExistingWord addr lens val = do
  mem <- use _2
  let cm = mem ^?! (unit % cacheMap)
      nInt = fromEnum addr
      block = lineNumber addr cm
      way = getWay block cm
      withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
      hasAddress v = (v ^. address) == withoutOffset
      idx = fromJust $ V.findIndex (maybe False hasAddress) way
      newWay = way V.// [(idx, (lens .~ val) <$> way V.! idx)]
      mem' = over (unit % cacheMap % addresses) (IM.insert block newWay) mem
  assign _2 mem'

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

addToCurrentCache :: Enum i => i -> Bool -> S.State (Latency, Memory, [Int]) ()
addToCurrentCache ix setDirty = do
  (lat, memory, rng) <- S.get
  let cm = memory ^?! (unit % cacheMap)
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
  modifying _2 (over (unit % cacheMap % addresses) updateAddresses)
  assign _3 rng'
  when shouldWriteBack writeBack

selectIndexThroughStrategy ::
     CacheStrategy -> V.Vector (Maybe CacheLine) -> [Int] -> (Int, [Int])
selectIndexThroughStrategy Random ways (r:rs) = (abs r `mod` V.length ways, rs)
selectIndexThroughStrategy LRU ways rng =
  (V.maxIndex $ V.mapMaybe (fmap (^. lastUsed)) ways, rng)
    --mapMaybe is equivalent to map, because none of the positions will be empty anyway

isHit :: Enum i => i -> Memory -> Bool
isHit n RAM {} = True
isHit n c@Cache {} = isJust $ V.findIndex (maybe False hasAddress) way
  where
    cm = c ^?! (unit % cacheMap)
    nInt = fromEnum n
    block = lineNumber n cm
    way = getWay block cm
    withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
    hasAddress v = (v ^. address) == withoutOffset

getWay :: Int -> CacheMap -> V.Vector (Maybe CacheLine)
getWay block cm =
  fromMaybe (V.replicate (cm ^. nWays) Nothing) $ (cm ^. addresses) IM.!? block

countHit :: Memory -> Memory
countHit = over (getInfo % hits) (+ 1) . countMiss

countMiss :: Memory -> Memory
countMiss = over (getInfo % total) (+ 1)

getInfo :: AffineTraversal' Memory MemInfo
getInfo =
  atraversal
    (\mem -> Right (mem ^?! l mem))
    (\mem mInfo -> set (l mem) mInfo mem)
  where
    l :: Memory -> AffineTraversal' Memory MemInfo
    l RAM {}        = ramInfo
    l Cache {}      = unit % info
    l SplitCache {} = dataUnit % info
