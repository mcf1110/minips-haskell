module Lib.Operation.TypeI where

import           Control.Monad.State.Lazy
import qualified Data.BitVector           as BV

import           Data.Bits                (Bits ((.&.)))
import qualified Lib.Memory               as M
import           Lib.Operation.Helpers    (addEnum, bvToSigned)
import           Lib.Operation.Infixes    (($&:), ($+:), ($.=), ($<-), ($<:),
                                           ($=), ($|:))
import           Lib.Operation.Types      (Immediate, Operation, RegNum)
import qualified Lib.Registers            as R

addi :: RegNum -> RegNum -> Immediate -> Operation ()
addi rs rt im = rt $<- rs $+: im

addiu :: RegNum -> RegNum -> Immediate -> Operation ()
addiu rs rt im = rt $<- rs $+: im

andi :: RegNum -> RegNum -> Immediate -> Operation ()
andi rs rt im = rt $<- rs $&: im

ori :: RegNum -> RegNum -> Immediate -> Operation ()
ori rs rt im = rt $<- rs $|: im

lui :: RegNum -> Immediate -> Operation ()
lui rt im = rt $= up
  where
    up = toEnum . fromEnum $ BV.zeroExtend 32 im BV.<<. 0x10

lw :: RegNum -> RegNum -> Immediate -> Operation ()
lw rs rt im = do
  comp <- get
  let rsv = R.get rs comp
      sign = bvToSigned im
      word = M.get (addEnum rsv sign) comp
  rt $= word

loadByteWith ::
     (Int -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> RegNum
  -> Immediate
  -> Operation ()
loadByteWith extFn rs rt off = do
  comp <- get
  let rsv = R.get rs comp
      sign = bvToSigned off
      byte = M.getQuarter (addEnum rsv sign) comp
      word = toEnum $ fromEnum $ extFn (32 - 8) $ BV.bitVec 8 byte
  rt $= word

lb :: RegNum -> RegNum -> Immediate -> Operation ()
lb = loadByteWith BV.signExtend

lbu :: RegNum -> RegNum -> Immediate -> Operation ()
lbu = loadByteWith BV.zeroExtend

sw :: RegNum -> RegNum -> Immediate -> Operation ()
sw rs rt im = do
  comp <- get
  let rsv = R.get rs comp
      rtv = R.get rt comp
      sign = bvToSigned im
      comp' = M.set (addEnum rsv sign) rtv comp
  put comp'

sb :: RegNum -> RegNum -> Immediate -> Operation ()
sb base rt im = do
  comp <- get
  let rsv = R.get base comp
      rtv = 0x000000FF .&. R.get rt comp
      sign = bvToSigned im
      comp' = M.set (addEnum rsv sign) rtv comp
  put comp'

lwc1 :: RegNum -> RegNum -> Immediate -> Operation ()
lwc1 base ft offset = do
  comp <- get
  let baseValue = R.get base comp
      sign = bvToSigned offset
      word = M.get (addEnum baseValue sign) comp
  ft $.= word

ldc1 :: RegNum -> RegNum -> Immediate -> Operation ()
ldc1 base ft offset = do
  comp <- get
  let baseValue = R.get base comp
      sign = bvToSigned offset
      pos = addEnum baseValue sign
      word1 = M.get pos comp
      word2 = M.get (pos + 4) comp
  ft $.= word1
  (ft + 1) $.= word2

swc1 :: RegNum -> RegNum -> Immediate -> Operation ()
swc1 base ft offset = do
  comp <- get
  let baseValue = R.get base comp
      sign = bvToSigned offset
      pos = addEnum baseValue sign
      word = R.getCop ft comp
      comp' = M.set pos word comp
  put comp'

slti :: RegNum -> RegNum -> Immediate -> Operation ()
slti rs rt im = rt $<- rs $<: im
