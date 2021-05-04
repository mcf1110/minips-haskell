module Lib.Memory.Cache where

import qualified Data.IntMap.Lazy                 as IM
import qualified Data.Vector                      as V

import           Control.Monad                    (unless, when)
import qualified Control.Monad.State.Lazy         as S
import           Data.Maybe                       (fromJust)
import           Optics                           (Field2 (_2), Field3 (_3),
                                                   assign, modifying, over, (%),
                                                   (^.))
import           Optics.Operators.Unsafe          ((^?!))

import           Lib.Computer.Types
import           Lib.Memory.Cache.LensHelpers     (getUnit)
import           Lib.Memory.Cache.PureHelpers     (getWaysAtLine, lineNumber,
                                                   selectIndex)
import           Lib.Memory.Cache.StatefulHelpers
import           Lib.Operation.Types              (Operation)

triggerInCache :: Enum i => MemoryAccessType -> i -> Operation ()
triggerInCache mat ix = do
  comp <- S.get
  -- state within a state, yay!
  let (val, (lat, newMem, newRng)) =
        S.runState (exec mat ix) (0, comp ^. mem, comp ^. rng)
  assign mem newMem
  assign rng newRng
  modifying (stats % nCycles) (+ lat)
  addTrace
  where
    w32 = toEnum $ fromEnum ix
    addTrace = modifying (stats % memTrace) (fmap ((mat, w32, w32 `div` 32) :))

exec :: Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) ()
exec mat ix = do
  updateLatency
  isHit <- checkHit mat ix
  if isHit
    then do
      countHit mat
      updateOnExistingWord mat ix lastUsed 0
      setDirtyIfNeeded mat ix
    else do
      countMiss mat
      snoopingWasSuccessful <- trySnooping mat ix
      unless snoopingWasSuccessful $ propagateToNext $ recursive ix
      addToCurrentCache mat ix
  removeFromInstructionCache mat ix
  where
    recursive =
      case mat of
        InstrFetch -> exec InstrFetch
        _          -> exec Read

addToCurrentCache ::
     Enum i => MemoryAccessType -> i -> S.State (Latency, Memory, [Int]) ()
addToCurrentCache mat ix = do
  (lat, memory, rng) <- S.get
  let cm = memory ^?! (getUnit mat % cacheMap)
      line = lineNumber ix cm
      ways = getWaysAtLine line cm
      (rng', selectedIndex) = selectIndex ways (memory ^?! strategy) rng
      newCacheLine =
        CacheLine
          { _isDirty = mat == Write
          , _lastUsed = 0
          , _address = fromEnum ix `div` (cm ^. wordsPerLine * 4)
          }
      newWays =
        fmap (over lastUsed (+ 1)) <$>
        ways V.// [(selectedIndex, Just newCacheLine)]
      justDeleted = ways V.! selectedIndex
      shouldWriteBack = maybe False (^. isDirty) justDeleted
      writeBack =
        propagateToNext $
        exec Write (fromJust justDeleted ^. address * (cm ^. wordsPerLine * 4))
      updateAddresses = IM.insert line newWays
  modifying _2 (over (getUnit mat % cacheMap % addresses) updateAddresses)
  assign _3 rng'
  when shouldWriteBack writeBack
