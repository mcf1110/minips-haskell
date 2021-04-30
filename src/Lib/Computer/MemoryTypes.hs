{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.MemoryTypes where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Word   as W
import           Optics      (Lens', lens, makeLenses, (^.))

type RAMMap = IM.IntMap W.Word32

type NLines = Int

type NWays = Int

type WordsPerLine = Int

data CacheMap =
  CacheMap
    { _addresses    :: IM.IntMap (V.Vector (Maybe CacheLine))
    , _nLines       :: NLines
    , _nWays        :: NWays
    , _wordsPerLine :: WordsPerLine
    }

data CacheLine =
  CacheLine
    { _isDirty  :: Bool
    , _lastUsed :: Int
    , _address  :: Int
    }
  deriving (Show)

data MemInfo =
  MemInfo
    { _hits  :: Int
    , _total :: Int
    }

data CacheType
  = Unified
  | DataCache
  | InstructionCache

data Memory
  = RAM
      { _info :: MemInfo
      , _im   :: RAMMap
      }
  | Cache
      { _info      :: MemInfo
      , _level     :: CacheLevel
      , _strategy  :: CacheStrategy
      , _cacheMap  :: CacheMap
      , _cacheType :: CacheType
      , _nextMem   :: Memory
      }

data CacheLevel
  = L1
  | L2
  deriving (Show)

data CacheStrategy
  = Random
  | LRU

makeLenses ''MemInfo

makeLenses ''CacheMap

makeLenses ''CacheLine

makeLenses ''Memory

ram :: Lens' Memory RAMMap
ram = lens get set
  where
    get (RAM info im) = im
    get c@Cache {}    = get $ _nextMem c
    set (RAM info im) im' = RAM info im'
    set c@Cache {} im'    = c {_nextMem = set (_nextMem c) im'}

emptyInfo :: MemInfo
emptyInfo = MemInfo 0 0

mkRam :: Memory
mkRam = RAM emptyInfo mempty

mkUnifiedCache ::
     CacheLevel -> CacheStrategy -> Int -> NWays -> Int -> Memory -> Memory
mkUnifiedCache lvl strat nbytes nways bytesPerLine =
  Cache emptyInfo lvl strat cm Unified
  where
    bytesPerWord = 4
    wpl = bytesPerLine `div` bytesPerWord
    nLines = nbytes `div` (wpl * 32)
    cm = mkCacheMap nLines nways wpl

mkCacheMap :: NLines -> NWays -> WordsPerLine -> CacheMap
mkCacheMap = CacheMap mempty

getLatency :: Memory -> Int
getLatency RAM {} = 100
getLatency c@Cache {} =
  case _cacheType c of
    InstructionCache -> 0
    _ ->
      case _level c of
        L1 -> 1
        L2 -> 10
