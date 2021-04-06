module Lib.Operation.Types where

import           Control.Monad.State.Lazy
import qualified Data.BitVector           as BV
import           Lib.Computer             (Computer)

data SC
  = PutInt Int
  | PutStr String
  | PutChar Char
  | GetInt
  | Die
  | NoSC
  deriving (Show, Eq)

type Operation a = State Computer a

type RegNum = BV.BitVector

type Immediate = BV.BitVector
