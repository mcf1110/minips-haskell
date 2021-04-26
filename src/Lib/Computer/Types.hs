{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.Types where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Word   as W
import           Optics      ((^.))
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

data Stats =
  Stats
    { _rCounter  :: Int
    , _iCounter  :: Int
    , _jCounter  :: Int
    , _frCounter :: Int
    , _fiCounter :: Int
    , _memTrace  :: [MemoryTrace]
    }

data Computer =
  Computer
    { _reg   :: Registers
    , _mem   :: Memory
    , _stats :: Stats
    }

makeLenses ''Registers

makeLenses ''Stats

makeLenses ''Computer

instance Show Stats where
  show stats =
    unwords
      [ "Instruction count:"
      , show $ sumStats stats
      , "(R:"
      , show (stats ^. rCounter)
      , "I:"
      , show (stats ^. iCounter)
      , "J:"
      , show (stats ^. jCounter)
      , "FR:"
      , show (stats ^. frCounter)
      , "FI:"
      , show (stats ^. fiCounter) <> ")"
      ]

sumStats :: Stats -> Int
sumStats stats =
  sum $ map (stats ^.) [rCounter, iCounter, jCounter, frCounter, fiCounter]
