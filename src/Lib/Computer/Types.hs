{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.Types where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Word   as W
import           Optics.TH   (makeLenses)

type Memory = IM.IntMap W.Word32

type GPR = V.Vector W.Word32

type FPR = V.Vector W.Word32

type CCFlags = V.Vector Bool

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
  show stats@(Stats r i j fr fi) =
    unwords
      [ "Instruction count:"
      , show $ sumStats stats
      , "(R:"
      , show r
      , "I:"
      , show i
      , "J:"
      , show j
      , "FR:"
      , show fr
      , "FI:"
      , show fi <> ")"
      ]

sumStats :: Stats -> Int
sumStats (Stats r i j fr fi) = sum [r, i, j, fr, fi]