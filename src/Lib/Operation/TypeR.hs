module Lib.Operation.TypeR where

import           Control.Monad.State.Lazy (modify)
import qualified Data.Bifunctor           as B
import qualified Data.Word                as W

import           Lib.Operation.Helpers    (modifyReg)
import           Lib.Operation.Infixes
import           Lib.Operation.Types      (Immediate, Operation, RegNum)
import qualified Lib.Registers            as R

add :: RegNum -> RegNum -> RegNum -> Operation ()
add rs rt rd = rd $<- rs $+$ rt

slt :: RegNum -> RegNum -> RegNum -> Operation ()
slt rs rt rd = rd $<- rs $<$ rt

or :: RegNum -> RegNum -> RegNum -> Operation ()
or rs rt rd = rd $<- rs $|$ rt

xor :: RegNum -> RegNum -> RegNum -> Operation ()
xor rs rt rd = rd $<- rs $^$ rt

srl :: RegNum -> RegNum -> Immediate -> Operation ()
srl rt rd sh = rd $<- rt $>>: sh

sll :: RegNum -> RegNum -> Immediate -> Operation ()
sll rt rd sh = rd $<- rt $<<: sh

setHiLo ::
     (RegNum -> RegNum -> Operation (W.Word32, W.Word32))
  -> RegNum
  -> RegNum
  -> Operation ()
setHiLo f rs rt = do
  (hi, lo) <- rs `f` rt
  34 $= lo
  33 $= hi
  return ()

mult :: RegNum -> RegNum -> Operation ()
mult = setHiLo ($*$)

divide :: RegNum -> RegNum -> Operation ()
divide = setHiLo ($/$)

moveFromTo :: RegNum -> RegNum -> Operation ()
moveFromTo from to = modifyReg (\r -> R.set to (R.get from r) r)
