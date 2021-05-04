{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.MemoryTypes where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Word   as W
import           Optics      (Lens', lens, makeLenses, (^.))

type ActualMemory = IM.IntMap W.Word32

type NLines = Int

type NWays = Int

type WordsPerLine = Int

data CacheLevel
  = L1
  | L2
  deriving (Show)

data CacheStrategy
  = Random
  | LRU
  deriving (Show)

data CacheUnit =
  CacheUnit
    { _info     :: !MemInfo
    , _cacheMap :: !CacheMap
    }
  deriving (Show)

data MemInfo =
  MemInfo
    { _hits  :: !Int
    , _total :: !Int
    }
  deriving (Show)

data CacheMap =
  CacheMap
    { _addresses    :: IM.IntMap (V.Vector (Maybe CacheLine))
    , _nLines       :: !NLines
    , _nWays        :: !NWays
    , _wordsPerLine :: !WordsPerLine
    }
  deriving (Show)

data CacheLine =
  CacheLine
    { _isDirty  :: !Bool
    , _lastUsed :: !Int
    , _address  :: !Int
    }
  deriving (Show)

data Memory
  = RAM
      { _ramInfo :: !MemInfo
      }
  | Cache
      { _unit     :: !CacheUnit
      , _level    :: CacheLevel
      , _strategy :: CacheStrategy
      , _nextMem  :: Memory
      }
  | SplitCache
      { _dataUnit :: !CacheUnit
      , _instUnit :: !CacheUnit
      , _level    :: CacheLevel
      , _strategy :: CacheStrategy
      , _nextMem  :: Memory
      }
  deriving (Show)

makeLenses ''MemInfo

makeLenses ''CacheLine

makeLenses ''CacheMap

makeLenses ''CacheUnit

makeLenses ''Memory

emptyInfo :: MemInfo
emptyInfo = MemInfo 0 0

mkRam :: Memory
mkRam = RAM emptyInfo

mkUnifiedCache ::
     CacheLevel -> CacheStrategy -> Int -> NWays -> Int -> Memory -> Memory
mkUnifiedCache lvl strat nbytes nways bytesPerLine = Cache unit lvl strat
  where
    bytesPerWord = 4
    wpl = bytesPerLine `div` bytesPerWord
    nLines = nbytes `div` bytesPerLine
    unit = CacheUnit emptyInfo $ mkCacheMap nLines nways wpl

mkSplitCache ::
     CacheLevel -> CacheStrategy -> Int -> NWays -> Int -> Memory -> Memory
mkSplitCache lvl strat nbytes nways bytesPerLine =
  SplitCache unit unit lvl strat
  where
    bytesPerWord = 4
    wpl = bytesPerLine `div` bytesPerWord
    nLines = nbytes `div` bytesPerLine
    unit = CacheUnit emptyInfo $ mkCacheMap nLines nways wpl

mkCacheMap :: NLines -> NWays -> WordsPerLine -> CacheMap
mkCacheMap = CacheMap mempty

getLatency :: Memory -> Int
getLatency RAM {} = 100
getLatency c =
  case _level c of
    L1 -> 1
    L2 -> 10
