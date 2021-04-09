module Lib.Operation.TypeI where

import           Control.Monad.State.Lazy
import qualified Data.BitVector           as BV

import           Data.Bits                (Bits ((.&.)))
import qualified Lib.Memory               as M
import           Lib.Operation.Helpers    (addEnum)
import           Lib.Operation.Infixes
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
  (r, m) <- get
  let rsv = R.get rs r
      sign = BV.zeroExtend 32 im
      word = M.get (addEnum rsv sign) m
  rt $= word

lb :: RegNum -> RegNum -> Immediate -> Operation ()
lb rs rt off = do
  (r, m) <- get
  let rsv = R.get rs r
      sign = BV.zeroExtend 32 off
      byte = M.getQuarter (addEnum rsv sign) m
      word = toEnum $ fromEnum $ BV.signExtend (32 - 8) $ BV.bitVec 8 byte
  rt $= word

sw :: RegNum -> RegNum -> Immediate -> Operation ()
sw rs rt im = do
  (r, m) <- get
  let rsv = R.get rs r
      rtv = R.get rt r
      sign = BV.zeroExtend 32 im
      m' = M.set (addEnum rsv sign) rtv m
  put (r, m')

sb :: RegNum -> RegNum -> Immediate -> Operation ()
sb base rt im = do
  (r, m) <- get
  let rsv = R.get base r
      rtv = 0x000000FF .&. R.get rt r
      sign = BV.zeroExtend 32 im
      m' = M.set (addEnum rsv sign) rtv m
  put (r, m')

lwc1 :: RegNum -> RegNum -> Immediate -> Operation ()
lwc1 base ft offset = do
  (r, m) <- get
  let baseValue = R.get base r
      sign = BV.zeroExtend 32 offset
      word = M.get (addEnum baseValue sign) m
  ft $.= word

ldc1 :: RegNum -> RegNum -> Immediate -> Operation ()
ldc1 base ft offset = do
  (r, m) <- get
  let baseValue = R.get base r
      sign = BV.zeroExtend 32 offset
      pos = addEnum baseValue sign
      word1 = M.get pos m
      word2 = M.get (pos + 4) m
  ft $.= word1
  (ft + 1) $.= word2

swc1 :: RegNum -> RegNum -> Immediate -> Operation ()
swc1 base ft offset = do
  (r, m) <- get
  let baseValue = R.get base r
      sign = BV.zeroExtend 32 offset
      pos = addEnum baseValue sign
      word = R.getCop ft r
      m' = M.set pos word m
  put (r, m')

slti :: RegNum -> RegNum -> Immediate -> Operation ()
slti rs rt im = rt $<- rs $<: im
