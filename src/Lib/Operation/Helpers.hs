module Lib.Operation.Helpers where

import           Control.Monad.State.Lazy (modify)
import qualified Data.Bifunctor           as B
import qualified Data.BitVector           as BV
import qualified Data.Word                as W

import qualified Lib.Memory               as M
import           Lib.Operation.Types      (Immediate, Operation)
import qualified Lib.Registers            as R

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum . fromEnum $ BV.bitVec 32 $ fromEnum x + fromEnum y

signExt :: BV.BitVector -> Integer
signExt = BV.int . BV.bitVec 32

addToPC :: (Integral a, Show a) => a -> Operation ()
addToPC v = modifyReg $ (\r -> R.set 32 (addEnum (R.get 32 r) (4 * v)) r)

incPC :: Operation ()
incPC = addToPC 1

calcJumpAddr :: Immediate -> R.Registers -> W.Word32
calcJumpAddr tgt r =
  toEnum $ fromEnum $ BV.append upperPC $ BV.zeroExtend (28 - BV.size tgt) tgt
  where
    upperPC = BV.most 4 $ BV.bitVec 32 $ fromEnum $ R.get 32 r

modifyReg :: (R.Registers -> R.Registers) -> Operation ()
modifyReg = modify . B.first
