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
  | NoSC
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
  incPC
  eval ins
  return NoSC
  where
    eval (IInstr Beq rs rt im)    = beq rs rt im
    eval (IInstr Bne rs rt im)    = bne rs rt im
    eval (IInstr Addi rs rt im)   = addi rs rt im
    eval (IInstr Addiu rs rt im)  = addiu rs rt im
    eval (IInstr Andi rs rt im)   = andi rs rt im
    eval (IInstr Lui _ rt im)     = lui rt im
    eval (IInstr Lw rs rt im)     = lw rs rt im
    eval (IInstr Ori rs rt im)    = ori rs rt im
    eval (IInstr Sw rs rt im)     = sw rs rt im
    eval (IInstr Slti rs rt im)   = slti rs rt im
    eval (RInstr Add rs rt rd _)  = add rs rt rd
    eval (RInstr Addu rs rt rd _) = add rs rt rd
    eval (RInstr Slt rs rt rd _)  = slt rs rt rd
    eval (RInstr Jr rs _ _ _)     = jr rs
    eval (RInstr Srl _ rt rd sh)  = srl rt rd sh
    eval (RInstr Sll _ rt rd sh)  = sll rt rd sh
    eval (RInstr Jalr rs _ rd _)  = jalr rd rs
    eval (JInstr J tgt)           = jump tgt
    eval (JInstr Jal tgt)         = jal tgt
    eval Nop                      = return ()
    eval Break                    = return ()
    eval a                        = error $ "Falta implementar: " <> show a

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum . fromEnum $ BV.bitVec 32 $ fromEnum x + fromEnum y

signExt :: BV.BitVector -> Integer
signExt = BV.int . BV.bitVec 32

infixr 1 $=

($=) :: RegNum -> W.Word32 -> Operation ()
($=) ad v = modify . B.first $ R.set ad v

infixr 1 $<-

($<-) :: RegNum -> Operation W.Word32 -> Operation () -- monadic version
($<-) tgt op = op >>= (tgt $=)

($+$) :: RegNum -> RegNum -> Operation W.Word32
($+$) ra rb = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (R.get rb r)

($<$) :: RegNum -> RegNum -> Operation W.Word32
($<$) ra rb = do
  (r, m) <- get
  let signed rx = BV.int $ BV.bitVec 32 $ R.get rx r
  return $
    if signed ra < signed rb
      then 1
      else 0

($<:) :: RegNum -> Immediate -> Operation W.Word32
($<:) ra im = do
  (r, m) <- get
  let signed rx = BV.int $ BV.bitVec 32 $ R.get rx r
      signedIm = BV.int $ BV.bitVec 32 im
  return $
    if signed ra < signedIm
      then 1
      else 0

($+:) :: RegNum -> Immediate -> Operation W.Word32
($+:) ra im = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (BV.int im)

bitwiseWith ::
     (BV.BitVector -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> Immediate
  -> Operation W.Word32
bitwiseWith op ra im = do
  (r, m) <- get
  return $
    toEnum $ fromEnum $ BV.bitVec 32 (R.get ra r) `op` BV.zeroExtend 32 im

($|:) :: RegNum -> Immediate -> Operation W.Word32
($|:) = bitwiseWith (BV..|.)

($&:) :: RegNum -> Immediate -> Operation W.Word32
($&:) = bitwiseWith (BV..&.)

shiftWith ::
     Enum a
  => (RegNum -> Immediate -> a)
  -> RegNum
  -> Immediate
  -> Operation W.Word32
shiftWith op ra im = do
  (r, m) <- get
  return $
    (toEnum . fromEnum) $ op (BV.bitVec 32 $ R.get ra r) $ BV.zeroExtend 32 im

($>>:) :: RegNum -> Immediate -> Operation W.Word32
($>>:) = shiftWith BV.shr

($<<:) :: RegNum -> Immediate -> Operation W.Word32
($<<:) = shiftWith BV.shl

addToPC :: (Integral a, Show a) => a -> Operation ()
addToPC v = modify . B.first $ (\r -> R.set 32 (addEnum (R.get 32 r) (4 * v)) r)

incPC :: Operation ()
incPC = addToPC 1

-- Type R
add :: RegNum -> RegNum -> RegNum -> Operation ()
add rs rt rd = rd $<- rs $+$ rt

slt :: RegNum -> RegNum -> RegNum -> Operation ()
slt rs rt rd = rd $<- rs $<$ rt

jr :: RegNum -> Operation ()
jr rnum = do
  runBranchDelaySlot
  modify . B.first $ (\r -> R.set 32 (R.get rnum r) r)

srl :: RegNum -> RegNum -> Immediate -> Operation ()
srl rt rd sh = rd $<- rt $>>: sh

sll :: RegNum -> RegNum -> Immediate -> Operation ()
sll rt rd sh = rd $<- rt $<<: sh

-- tr r = trace (unlines $ showRegisters r) r
jalr :: RegNum -> RegNum -> Operation ()
jalr rd rs = do
  modify . B.first $ (\r -> R.set rd (4 + R.get 32 r) r)
  jr rs

-- Type I
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

sw :: RegNum -> RegNum -> Immediate -> Operation ()
sw rs rt im = do
  (r, m) <- get
  let rsv = R.get rs r
      rtv = R.get rt r
      sign = BV.zeroExtend 32 im
      m' = M.set (addEnum rsv sign) rtv m
  put (r, m')

slti :: RegNum -> RegNum -> Immediate -> Operation ()
slti rs rt im = rt $<- rs $<: im

-- Type J
jump :: Immediate -> Operation ()
jump tgt = modify . B.first $ (\r -> R.set 32 (calcJumpAddr tgt r) r)

jal :: Immediate -> Operation ()
jal tgt =
  modify . B.first $
  (\r -> R.set 32 (calcJumpAddr tgt r) $ R.set 31 (R.get 32 r) r)

calcJumpAddr :: Immediate -> R.Registers -> W.Word32
calcJumpAddr tgt r =
  toEnum $ fromEnum $ BV.append upperPC $ BV.zeroExtend (28 - BV.size tgt) tgt
  where
    upperPC = BV.most 4 $ BV.bitVec 32 $ fromEnum $ R.get 32 r

-- Branching
branchOn ::
     (W.Word32 -> W.Word32 -> Bool)
  -> RegNum
  -> RegNum
  -> Immediate
  -> Operation ()
branchOn op rs rt im = do
  (r, m) <- get
  when (R.get rs r `op` R.get rt r) $ do
    runBranchDelaySlot
    addToPC (-1)
    addToPC $ BV.int im

beq :: RegNum -> RegNum -> Immediate -> Operation ()
beq = branchOn (==)

bne :: RegNum -> RegNum -> Immediate -> Operation ()
bne = branchOn (/=)

runBranchDelaySlot :: Operation ()
runBranchDelaySlot = do
  (r, m) <- get
  let pc = R.get 32 r
      ins = decodeInstruction $ M.get pc m
  evalInstruction ins
  return ()
