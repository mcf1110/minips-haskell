module Lib.Decode.Types where

import qualified Data.BitVector as BV

data Instr
  = RInstr
      { funct :: Funct
      , rs    :: BV.BitVector
      , rt    :: BV.BitVector
      , rd    :: BV.BitVector
      , shamt :: BV.BitVector
      }
  | IInstr
      { iop       :: IOp
      , rs        :: BV.BitVector
      , rt        :: BV.BitVector
      , immediate :: BV.BitVector
      }
  | JInstr
      { jop :: JOp
      , tgt :: BV.BitVector
      }
  | FRInstr
      { ffunct :: FFunct
      , fmt    :: FFmt
      , ft     :: BV.BitVector
      , fs     :: BV.BitVector
      , fd     :: BV.BitVector
      }
  | FIInstr
      { fiop :: FIOp
      , ft   :: BV.BitVector
      , imm  :: BV.BitVector
      }
  | Syscall
  | Nop
  | Break
  deriving (Show, Eq)

data FFmt
  = Single
  | Double
  | Word
  deriving (Show, Eq)

data Funct
  = Add
  | Addu
  | And
  | Jr
  | Nor
  | Or
  | Xor
  | Slt
  | Sltu
  | Sll
  | Srl
  | Sub
  | Subu
  | Jalr
  | Mult
  | Mflo
  | Mfhi
  | Div
  deriving (Show, Eq)

data IOp
  = Addi
  | Addiu
  | Andi
  | Beq
  | Bne
  | Lui
  | Ori
  | Lw
  | Lb
  | Sw
  | Slti
  | Lwc1
  | Swc1
  | Ldc1
  deriving (Show, Eq)

data JOp
  = J
  | Jal
  deriving (Show, Eq)

data FFunct
  = Mfc1
  | Mtc1
  | Mov
  | CvtD
  | CvtS
  | CvtW
  | FAdd
  | FMul
  | FDiv
  deriving (Show, Eq)

data FIOp
  = Bc1t
  | Bc1f
  deriving (Show, Eq)

type Program = [Instr]
