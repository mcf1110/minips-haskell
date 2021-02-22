module Lib.Operation where

import           Lib.Computer
import           Lib.Decode
import qualified Lib.Memory               as M
import qualified Lib.Registers            as R

import           Control.Monad            (when)
import           Control.Monad.State.Lazy
import qualified Data.Bifunctor           as B
import qualified Data.BitVector           as BV
import qualified Data.Word                as W

data SC
  = PutInt Int
  | PutStr String
  | PutChar Char
  | GetInt
  | Die
  | NoOp
  deriving (Show, Eq)

type Operation a = State Computer a

type RegNum = BV.BitVector

type Immediate = BV.BitVector

evalInstruction :: Instr -> Operation SC
evalInstruction Syscall = do
  incPC
  (r, m) <- get
  let v0 = R.get 2 r
      a0 = R.get 4 r
  return $
    case v0 of
      1  -> PutInt $ fromEnum a0
      4  -> PutStr $ M.getString a0 m
      11 -> PutChar $ toEnum . fromEnum $ a0
      5  -> GetInt
      10 -> Die
evalInstruction ins = do
  eval ins
  incPC
  return NoOp
  where
    eval (IInstr Addi rs rt im)   = addi rs rt im
    eval (IInstr Addiu rs rt im)  = addiu rs rt im
    eval (IInstr Lui _ rt im)     = lui rt im
    eval (IInstr Ori rs rt im)    = ori rs rt im
    eval (IInstr Beq rs rt im)    = beq rs rt im
    eval (RInstr Add rs rt rd _)  = add rs rt rd
    eval (RInstr Addu rs rt rd _) = add rs rt rd
    eval a                        = error $ "Falta implementar: " <> show a

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum $ fromEnum x + fromEnum y

infixr 1 $=

($=) :: RegNum -> W.Word32 -> Operation ()
($=) ad v = modify . B.first $ R.set ad v

($+$) :: RegNum -> RegNum -> Operation W.Word32
($+$) ra rb = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (R.get rb r)

($+:) :: RegNum -> Immediate -> Operation W.Word32
($+:) ra im = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (BV.int im)

($+:.) :: RegNum -> Immediate -> Operation W.Word32
($+:.) ra im = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (BV.nat im)

($|:) :: RegNum -> Immediate -> Operation W.Word32
($|:) ra im = do
  (r, m) <- get
  return $
    toEnum $ fromEnum $ BV.bitVec 32 (R.get ra r) BV..|. BV.zeroExtend 32 im

addToPC :: (Integral a, Show a) => a -> Operation ()
addToPC v = modify . B.first $ (\r -> R.set 32 (addEnum (R.get 32 r) (4 * v)) r)

incPC :: Operation ()
incPC = addToPC 1

-- Type R
add :: RegNum -> RegNum -> RegNum -> Operation ()
add rs rt rd = do
  res <- rs $+$ rt
  rd $= res

-- Type I
addi :: RegNum -> RegNum -> Immediate -> Operation ()
addi rs rt im = do
  res <- rs $+: im
  rt $= res

addiu :: RegNum -> RegNum -> Immediate -> Operation ()
addiu rs rt im = do
  res <- rs $+:. im
  rt $= res

lui :: RegNum -> Immediate -> Operation ()
lui rt im = do
  rt $= toEnum (fromEnum $ BV.zeroExtend 32 im BV.<<. 0x10)

ori :: RegNum -> RegNum -> Immediate -> Operation ()
ori rs rt im = do
  res <- rs $|: im
  rt $= res

beq :: RegNum -> RegNum -> Immediate -> Operation ()
beq rs rt im = do
  (r, m) <- get
  when (R.get rs r == R.get rt r) $ addToPC $ BV.int im
