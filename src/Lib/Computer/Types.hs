{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.Types
  ( module Lib.Computer.MemoryTraceTypes
  , module Lib.Computer.MemoryTypes
  , module Lib.Computer.RegisterTypes
  , module Lib.Computer.Types
  ) where

import           Lib.Computer.MemoryTraceTypes
import           Lib.Computer.MemoryTypes
import           Lib.Computer.RegisterTypes
import           Optics                        (makeLenses, (^.))

data InstructionCounter =
  InstructionCounter
    { _rCounter  :: !Int
    , _iCounter  :: !Int
    , _jCounter  :: !Int
    , _frCounter :: !Int
    , _fiCounter :: !Int
    }

data Stats =
  Stats
    { _insCounter :: !InstructionCounter
    , _memTrace   :: !(Maybe [MemoryTrace])
    , _nCycles    :: !Int
    }

data Computer =
  Computer
    { _reg   :: Registers
    , _mem   :: Memory
    , _ram   :: ActualMemory
    , _stats :: !Stats
    , _rng   :: [Int]
    }

makeLenses ''InstructionCounter

makeLenses ''Stats

makeLenses ''Computer

sumInstructionCounters :: InstructionCounter -> Int
sumInstructionCounters counters =
  sum $ map (counters ^.) [rCounter, iCounter, jCounter, frCounter, fiCounter]

instance Show InstructionCounter where
  show counters =
    unwords
      [ "Instruction count:"
      , show $ sumInstructionCounters counters
      , "(R:"
      , show (counters ^. rCounter)
      , "I:"
      , show (counters ^. iCounter)
      , "J:"
      , show (counters ^. jCounter)
      , "FR:"
      , show (counters ^. frCounter)
      , "FI:"
      , show (counters ^. fiCounter) <> ")"
      ]
