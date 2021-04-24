module Lib.Operation.Helpers where

import           Control.Monad.State.Lazy (modify)
import qualified Data.Bifunctor           as B
import qualified Data.BitVector           as BV
import qualified Data.Word                as W

import           Lib.Computer.Types       (Computer, reg)
import qualified Lib.Memory               as M
import           Lib.Operation.Types      (Immediate, Operation)
import qualified Lib.Registers            as R
import           Optics                   (over, view)

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum . fromEnum $ BV.bitVec 32 $ fromEnum x + fromEnum y

signExt :: BV.BitVector -> Integer
signExt = BV.int . BV.bitVec 32

addToPC :: (Integral a, Show a) => a -> Operation ()
addToPC v = modify (\comp -> R.set 32 (addEnum (R.get 32 comp) (4 * v)) comp)

incPC :: Operation ()
incPC = addToPC 1

calcJumpAddr :: Immediate -> Computer -> W.Word32
calcJumpAddr tgt comp =
  toEnum $ fromEnum $ BV.append upperPC $ BV.zeroExtend (28 - BV.size tgt) tgt
  where
    upperPC = BV.most 4 $ BV.bitVec 32 $ fromEnum $ R.get 32 comp

modifyReg :: (R.Registers -> R.Registers) -> Operation ()
modifyReg = modify . over reg

w32ToSigned :: W.Word32 -> Int
w32ToSigned w =
  if w > 0x7fffffff
    then -(fromEnum (0xffffffff - w + 1))
    else fromEnum w

bvToSigned :: BV.BitVector -> Integer
bvToSigned bv = BV.int $ BV.signExtend (32 - BV.size bv) bv
