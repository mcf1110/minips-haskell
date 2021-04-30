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
import           Optics                   (Field2 (_2), Lens', assign, lens,
                                           modifying, over, (%), (.~), (^.))
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
set ix v = updatingLatencyAndTrace Write (writeMemory (const v)) ix

updatingLatencyAndTrace ::
     Enum i
  => MemoryTraceType
  -> (i -> S.State (Latency, Memory) a)
  -> i
  -> Operation a
updatingLatencyAndTrace traceType statefulFunction ix = do
  comp <- S.get
  -- state within a state, yay!
  let (val, (lat, newMem)) = S.runState (statefulFunction ix) (0, comp ^. mem)
  assign mem newMem
  updateLatencyAndTrace traceType ix lat
  return val

updateLatencyAndTrace ::
     Enum i => MemoryTraceType -> i -> Latency -> Operation ()
updateLatencyAndTrace accessType n lat = do
  modifying (stats % memTrace) ((accessType, w32, w32 `div` 32) :) -- add trace
  modifying (stats % nCycles) (+ lat) -- add latency
  where
    w32 = toEnum $ fromEnum n

writeMemory ::
     Enum i => (W.Word32 -> W.Word32) -> i -> S.State (Latency, Memory) ()
writeMemory f ix = do
  (l0, m0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit ix m0
    then do
      S.put (l1, countHit m0)
      updateOnExistingWord ix lastUsed 0
      updateOnExistingWord ix isDirty True
    else do
      S.put (l1, countMiss m0)
      propagateToNext (fetchMemory ix)
      substituteInCurrentCache ix True -- dirty, just written
  writeToRam
  where
    writeToRam = do
      modifying (_2 % ram) (IM.update (Just . f) (fromEnum ix))

fetchMemory :: Enum i => i -> S.State (Latency, Memory) W.Word32
fetchMemory n = do
  (l0, m0) <- S.get
  let l1 = l0 + getLatency m0
  if isHit n m0
      -- all done, just count a hit and go away
    then do
      S.put (l1, countHit m0)
      updateOnExistingWord n lastUsed 0
      return $ readFromRam $ m0 ^. ram
    else do
      S.put (l1, countMiss m0)
      -- fetch it from the next level
      v <- propagateToNext $ fetchMemory n
      substituteInCurrentCache n False -- notDirty, just loaded
      return v
  where
    readFromRam im = fromMaybe 0 $ im IM.!? fromEnum n

updateOnExistingWord ::
     Enum i => i -> Lens' CacheLine a -> a -> S.State (Latency, Memory) ()
updateOnExistingWord addr lens val = do
  (l, mem) <- S.get
  let cm = mem ^?! cacheMap
      nInt = fromEnum addr
      block = lineNumber addr cm
      way = getWay block cm
      hasAddress v = (v ^. address) == nInt
      idx = fromJust $ V.findIndex (maybe False hasAddress) way
      newWay = way V.// [(idx, (lens .~ val) <$> way V.! idx)]
      mem' = over (cacheMap % addresses) (IM.insert block newWay) mem
  S.put (l, mem')
  return ()

propagateToNext :: S.State (Latency, Memory) a -> S.State (Latency, Memory) a
propagateToNext statefulFunction = do
  (l1, m1) <- S.get
  let (v, (l2, m2)) = S.runState statefulFunction (l1, m1 ^?! nextMem) -- is safe because only Caches can have misses
  S.put (l2, (nextMem .~ m2) m1)
  return v

lineNumber :: Enum i => i -> CacheMap -> Int
lineNumber ix cm = (addr `div` (bytesInAWord * wordsInALine)) `mod` linesInCache
  where
    addr = fromEnum ix
    linesInCache = cm ^. nLines
    wordsInALine = cm ^. wordsPerLine
    bytesInAWord = 4

-- TODO: consider policy
substituteInCurrentCache :: Enum i => i -> Bool -> S.State (Latency, Memory) ()
substituteInCurrentCache ix setDirty = do
  (lat, memory) <- S.get
  let cm = memory ^?! cacheMap
      block = lineNumber ix cm
      w0 = getWay block cm
      firstEmptyIndex = V.findIndex isNothing w0
    --mapMaybe is equivalent to map, because none of the positions will be empty anyway
      lruIndex = V.maxIndex $ V.mapMaybe (fmap (^. lastUsed)) w0
      selectedIndex = fromMaybe lruIndex firstEmptyIndex
      newCacheLine =
        CacheLine {_isDirty = setDirty, _lastUsed = 0, _address = fromEnum ix}
      w1 = w0 V.// [(selectedIndex, Just newCacheLine)]
      w2 = fmap (over lastUsed (+ 1)) <$> w1 -- incrementLastUsedByOne
      updateAddresses = IM.insert block w2
      shouldWriteBack = maybe False (^. isDirty) (w0 V.! selectedIndex)
      writeBack = propagateToNext $ writeMemory id ix
  modifying _2 (over (cacheMap % addresses) updateAddresses)
  when shouldWriteBack writeBack

isHit :: Enum i => i -> Memory -> Bool
isHit n RAM {} = True
isHit n c@Cache {} = isJust $ V.findIndex (maybe False hasAddress) way
  where
    cm = c ^?! cacheMap
    nInt = fromEnum n
    block = lineNumber n cm
    way = getWay block cm
    hasAddress v = (v ^. address) == nInt

getWay :: Int -> CacheMap -> V.Vector (Maybe CacheLine)
getWay block cm =
  fromMaybe (V.replicate (cm ^. nWays) Nothing) $ (cm ^. addresses) IM.!? block

countHit :: Memory -> Memory
countHit = over (info % hits) (+ 1) . countMiss

countMiss :: Memory -> Memory
countMiss = over (info % total) (+ 1)

fetchQuarter :: Enum i => i -> S.State (Latency, Memory) W.Word8
fetchQuarter n = extractQuarter <$> fetchMemory s
  where
    s = 4 * div (fromEnum n) 4
    i = mod (fromEnum n) 4
    extractQuarter :: W.Word32 -> W.Word8
    extractQuarter v = toEnum $ fromEnum $ quarterWords !! i
      where
        bv = BV.bitVec 32 v
        quarterWords = [bv BV.@: ix | ix <- map reverse $ chunksOf 8 [0 .. 31]]
