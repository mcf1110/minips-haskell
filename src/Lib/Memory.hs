module Lib.Memory where

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
import           Lib.Operation.Types      (Operation)
import           Optics                   (Field2 (_2), Field3 (_3), Lens',
                                           assign, lens, modifying, over, use,
                                           (%), (.~), (^.))
import           Optics.Operators.Unsafe  ((^?!))

get :: Enum i => i -> Operation W.Word32
get = updatingLatencyAndTrace Read fetchMemory

getQuarter :: Enum i => i -> Operation W.Word8
getQuarter = updatingLatencyAndTrace Read fetchQuarter

getInstruction :: Enum i => i -> Operation W.Word32
getInstruction = updatingLatencyAndTrace InstrFetch fetchMemory

getString :: Enum i => i -> Operation String
getString n = do
  chars <- toChars <$> get address
  let untilNull = takeWhile (/= '\NUL') chars
  if untilNull /= chars
    then return untilNull -- we've reached the end
    else (chars <>) <$> getString (address + 4) -- do another reading
  where
    address = 4 * div (fromEnum n) 4
    offset = mod (fromEnum n) 4
    toChars :: W.Word32 -> String
    toChars w32 =
      let bv = BV.bitVec 32 w32
       in drop
            offset
            [ toEnum $ fromEnum $ bv BV.@: ix
            | ix <- map reverse $ chunksOf 8 [0 .. 31]
            ]

set :: (Eq i, Num i, Enum i) => i -> W.Word32 -> Operation ()
set ix v = do
  updatingLatencyAndTrace Write writeMemory ix
  modifying (mem % ram) (IM.insert (fromEnum ix) v)

updatingLatencyAndTrace ::
     Enum i
  => MemoryTraceType
  -> (i -> S.State (Latency, Memory, [Int]) a)
  -> i
  -> Operation a
updatingLatencyAndTrace traceType statefulFunction ix = do
  comp <- S.get
  -- state within a state, yay!
  let (val, (lat, newMem, newRng)) =
        S.runState (statefulFunction ix) (0, comp ^. mem, comp ^. rng)
  assign mem newMem
  assign rng newRng
  updateLatencyAndTrace traceType ix lat
  return val

updateLatencyAndTrace ::
     Enum i => MemoryTraceType -> i -> Latency -> Operation ()
updateLatencyAndTrace accessType n lat = do
  modifying (stats % memTrace) ((accessType, w32, w32 `div` 32) :) -- add trace
  modifying (stats % nCycles) (+ lat) -- add latency
  where
    w32 = toEnum $ fromEnum n

-- despite the name, does not actually write anything!!
writeMemory :: Enum i => i -> S.State (Latency, Memory, [Int]) ()
writeMemory ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
    then do
      S.put (l1, countHit m0, rng0)
      updateOnExistingWord ix lastUsed 0
      updateOnExistingWord ix isDirty True
    else do
      S.put (l1, countMiss m0, rng0)
      propagateToNext $ fetchMemory ix
      addToCurrentCache ix True -- dirty, just written

fetchMemory :: Enum i => i -> S.State (Latency, Memory, [Int]) W.Word32
fetchMemory ix = do
  (l0, m0, rng0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit m0, rng0)
      updateOnExistingWord ix lastUsed 0
      return $ readFromRam $ m0 ^. ram
    else do
      S.put (l1, countMiss m0, rng0)
      v <- propagateToNext $ fetchMemory ix
      addToCurrentCache ix False -- notDirty, just loaded
      return v
  where
    readFromRam im = fromMaybe 0 $ im IM.!? fromEnum ix

updateOnExistingWord ::
     Enum i
  => i
  -> Lens' CacheLine a
  -> a
  -> S.State (Latency, Memory, [Int]) ()
updateOnExistingWord addr lens val = do
  mem <- use _2
  let cm = mem ^?! cacheMap
      nInt = fromEnum addr
      block = lineNumber addr cm
      way = getWay block cm
      withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
      hasAddress v = (v ^. address) == withoutOffset
      idx = fromJust $ V.findIndex (maybe False hasAddress) way
      newWay = way V.// [(idx, (lens .~ val) <$> way V.! idx)]
      mem' = over (cacheMap % addresses) (IM.insert block newWay) mem
  assign _2 mem'

propagateToNext ::
     S.State (Latency, Memory, [Int]) a -> S.State (Latency, Memory, [Int]) a
propagateToNext statefulFunction = do
  (l1, m1, rng) <- S.get
  let (v, (l2, m2, rng')) =
        S.runState statefulFunction (l1, m1 ^?! nextMem, rng) -- is safe because only Caches can have misses
  S.put (l2, (nextMem .~ m2) m1, rng')
  return v

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
  let cm = memory ^?! cacheMap
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
        writeMemory (fromJust justDeleted ^. address * (cm ^. wordsPerLine * 4))
  modifying _2 (over (cacheMap % addresses) updateAddresses)
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
    cm = c ^?! cacheMap
    nInt = fromEnum n
    block = lineNumber n cm
    way = getWay block cm
    withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
    hasAddress v = (v ^. address) == withoutOffset

getWay :: Int -> CacheMap -> V.Vector (Maybe CacheLine)
getWay block cm =
  fromMaybe (V.replicate (cm ^. nWays) Nothing) $ (cm ^. addresses) IM.!? block

countHit :: Memory -> Memory
countHit = over (info % hits) (+ 1) . countMiss

countMiss :: Memory -> Memory
countMiss = over (info % total) (+ 1)

fetchQuarter :: Enum i => i -> S.State (Latency, Memory, [Int]) W.Word8
fetchQuarter n = extractQuarter <$> fetchMemory s
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    extractQuarter :: W.Word32 -> W.Word8
    extractQuarter v = toEnum $ fromEnum $ quarterWords !! i
      where
        bv = BV.bitVec 32 v
        quarterWords = [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]
