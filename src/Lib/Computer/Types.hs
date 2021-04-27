{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.Types where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Word   as W
import           Optics      ((%), (^.))
import           Optics.TH   (makeLenses)

type Memory = IM.IntMap W.Word32

type GPR = V.Vector W.Word32

type FPR = V.Vector W.Word32

type CCFlags = V.Vector Bool

type Address = W.Word32

type Line = W.Word32

type MemoryTrace = (MemoryTraceType, Address, W.Word32)

data MemoryTraceType
  = Read
  | InstrFetch
  | Write
  deriving (Show)

data Registers =
  Registers
    { _gpr     :: GPR
    , _fpr     :: FPR
    , _ccFlags :: CCFlags
    }

data InstructionCounter =
  InstructionCounter
    { _rCounter  :: Int
    , _iCounter  :: Int
    , _jCounter  :: Int
    , _frCounter :: Int
    , _fiCounter :: Int
    }

data Stats =
  Stats
    { _insCounter :: InstructionCounter
    , _memTrace   :: [MemoryTrace]
    , _nCycles    :: Int
    }

data Computer =
  Computer
    { _reg   :: Registers
    , _mem   :: Memory
    , _stats :: Stats
    }

makeLenses ''Registers

makeLenses ''InstructionCounter

makeLenses ''Stats

makeLenses ''Computer

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

sumInstructionCounters :: InstructionCounter -> Int
sumInstructionCounters counters =
  sum $ map (counters ^.) [rCounter, iCounter, jCounter, frCounter, fiCounter]
