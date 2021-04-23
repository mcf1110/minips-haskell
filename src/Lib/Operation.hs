module Lib.Operation
  ( evalInstruction
  , SC(..)
  ) where

import           Control.Monad            (when)
import           Control.Monad.State.Lazy
import qualified Data.Bifunctor           as B
import qualified Data.BitVector           as BV
import qualified Data.Word                as W
import           Prelude                  hiding (and, or)

import           Lib.Decode
import qualified Lib.Memory               as M
import qualified Lib.Registers            as R

import           Lib.Operation.Helpers    (addToPC, calcJumpAddr, incPC,
                                           modifyReg, w32ToSigned)
import           Lib.Operation.TypeFR
import           Lib.Operation.TypeI
import           Lib.Operation.TypeR
import           Lib.Operation.Types

evalInstruction :: Instr -> Operation SC
evalInstruction Syscall = do
  incPC
  (r, m) <- get
  let v0 = R.get 2 r
      a0 = R.get 4 r
  return $
    case v0 of
      1  -> PutInt $ w32ToSigned a0
      4  -> PutStr $ M.getString a0 m
      11 -> PutChar $ toEnum . fromEnum $ a0
      5  -> GetInt
      10 -> Die
      2  -> PutFloat $ R.getF 12 r
      3  -> PutDouble $ R.getD 12 r
      6  -> GetFloat
      7  -> GetDouble
      x  -> error $ "Syscall desconhecida: " <> show x
evalInstruction ins = do
  incPC
  runOperation ins
  return NoSC

runOperation :: Instr -> Operation ()
runOperation (IInstr Beq rs rt im) = beq rs rt im
runOperation (IInstr Bne rs rt im) = bne rs rt im
runOperation (IInstr Blez rs _ im) = blez rs im
runOperation (IInstr Bgez rs _ im) = bgez rs im
runOperation (IInstr Addi rs rt im) = addi rs rt im
runOperation (IInstr Addiu rs rt im) = addiu rs rt im
runOperation (IInstr Andi rs rt im) = andi rs rt im
runOperation (IInstr Ori rs rt im) = ori rs rt im
runOperation (IInstr Lui _ rt im) = lui rt im
runOperation (IInstr Lw rs rt im) = lw rs rt im
runOperation (IInstr Lb rs rt im) = lb rs rt im
runOperation (IInstr Lbu rs rt im) = lbu rs rt im
runOperation (IInstr Sw rs rt im) = sw rs rt im
runOperation (IInstr Sb rs rt im) = sb rs rt im
runOperation (IInstr Slti rs rt im) = slti rs rt im
runOperation (IInstr Lwc1 rs rt im) = lwc1 rs rt im
runOperation (IInstr Ldc1 rs rt im) = ldc1 rs rt im
runOperation (IInstr Swc1 rs rt im) = swc1 rs rt im
runOperation (RInstr Add rs rt rd _) = add rs rt rd
runOperation (RInstr Addu rs rt rd _) = add rs rt rd
runOperation (RInstr Subu rs rt rd _) = subu rs rt rd
runOperation (RInstr Slt rs rt rd _) = slt rs rt rd
runOperation (RInstr Sltu rs rt rd _) = sltu rs rt rd
runOperation (RInstr And rs rt rd _) = and rs rt rd
runOperation (RInstr Or rs rt rd _) = or rs rt rd
runOperation (RInstr Xor rs rt rd _) = xor rs rt rd
runOperation (RInstr Mult rs rt _ _) = mult rs rt
runOperation (RInstr Div rs rt _ _) = divide rs rt
runOperation (RInstr Mflo _ _ rd _) = moveFromTo 34 rd
runOperation (RInstr Mfhi _ _ rd _) = moveFromTo 33 rd
runOperation (RInstr Jr rs _ _ _) = jr rs
runOperation (RInstr Srl _ rt rd sh) = srl rt rd sh
runOperation (RInstr Sra _ rt rd sh) = sra rt rd sh
runOperation (RInstr Sll _ rt rd sh) = sll rt rd sh
runOperation (RInstr Jalr rs _ rd _) = jalr rd rs
runOperation (JInstr J tgt) = jump tgt
runOperation (JInstr Jal tgt) = jal tgt
runOperation (FRInstr FAdd fmt ft fs fd) = fadd fmt ft fs fd
runOperation (FRInstr FSub fmt ft fs fd) = fsub fmt ft fs fd
runOperation (FRInstr FMul fmt ft fs fd) = fmul fmt ft fs fd
runOperation (FRInstr FDiv fmt ft fs fd) = fdiv fmt ft fs fd
runOperation (FRInstr Mfc1 _ rt fs _) = mfc1 rt fs
runOperation (FRInstr Mtc1 _ rt fs _) = mtc1 rt fs
runOperation (FRInstr Mov fmt _ fs fd) = mov fmt fs fd
runOperation (FRInstr CvtD fmt _ fs fd) = cvtd fmt fs fd
runOperation (FRInstr CvtS fmt _ fs fd) = cvts fmt fs fd
runOperation (FRInstr CvtW fmt _ fs fd) = cvtw fmt fs fd
runOperation (FRInstr CLt fmt ft fs fd) = clt fmt ft fs
runOperation Nop = return ()
runOperation Break = return ()
runOperation a = error $ "Falta implementar: " <> show a

runBranchDelaySlot :: Operation ()
runBranchDelaySlot = do
  (r, m) <- get
  let pc = R.get 32 r
      ins = decodeInstruction $ M.get pc m
  evalInstruction ins
  return ()

-- jumps must be in this module to avoid circular dependencies, because of BranchDelaySlot
branchOn ::
     (Int -> Int -> Bool) -> RegNum -> RegNum -> Immediate -> Operation ()
branchOn op rs rt im = do
  (r, m) <- get
  when (w32ToSigned (R.get rs r) `op` w32ToSigned (R.get rt r)) $ do
    runBranchDelaySlot
    addToPC (-1)
    addToPC $ BV.int im

beq :: RegNum -> RegNum -> Immediate -> Operation ()
beq = branchOn (==)

bne :: RegNum -> RegNum -> Immediate -> Operation ()
bne = branchOn (/=)

blez :: RegNum -> Immediate -> Operation ()
blez = branchOn (>=) 0 -- branchOn $zero >= $x

bgez :: RegNum -> Immediate -> Operation ()
bgez = branchOn (<=) 0 -- branchOn $zero <= $x

jr :: RegNum -> Operation ()
jr rnum = do
  runBranchDelaySlot
  modifyReg (\r -> R.set 32 (R.get rnum r) r)

jalr :: RegNum -> RegNum -> Operation ()
jalr rd rs = do
  modifyReg (\r -> R.set rd (4 + R.get 32 r) r)
  jr rs

jump :: Immediate -> Operation ()
jump tgt = do
  runBranchDelaySlot
  modifyReg (\r -> R.set 32 (calcJumpAddr tgt r) r)

jal :: Immediate -> Operation ()
jal tgt = do
  runBranchDelaySlot
  modifyReg (\r -> R.set 32 (calcJumpAddr tgt r) $ R.set 31 (R.get 32 r) r)
