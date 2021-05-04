{-# LANGUAGE TemplateHaskell #-}

module Lib.Computer.RegisterTypes where

import qualified Data.Vector.Unboxed as V
import qualified Data.Word           as W
import           Optics              (makeLenses)

type GPR = V.Vector W.Word32

type FPR = V.Vector W.Word32

type CCFlags = V.Vector Bool

data Registers =
  Registers
    { _gpr     :: GPR
    , _fpr     :: FPR
    , _ccFlags :: CCFlags
    }

makeLenses ''Registers
