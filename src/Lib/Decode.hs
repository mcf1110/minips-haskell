module Lib.Decode where

import qualified Data.BitVector as BV
import qualified Data.Word      as W

import           Lib.Segment

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
      , fmt    :: BV.BitVector
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

data Funct
  = Add
  | Addu
  | And
  | Jr
  | Nor
  | Or
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
  | Ldc1
  deriving (Show, Eq)

data JOp
  = J
  | Jal
  deriving (Show, Eq)

data FFunct
  = Mfc1
  | Mtc1
  deriving (Show, Eq)

data FIOp
  = Bc1t
  | Bc1f
  deriving (Show, Eq)

type Program = [Instr]

decodeProgram :: Segment -> Program
decodeProgram = map decodeInstruction

decodeInstruction :: W.Word32 -> Instr
decodeInstruction = decode . wordToBV

wordToBV :: W.Word32 -> BV.BitVector
wordToBV = BV.bitVec 32

getFields :: [Int] -> BV.BitVector -> [BV.BitVector]
getFields sizes bv = map get offsets
  where
    starts = scanr (+) 0 sizes
    offsets = zip (map (subtract 1) starts) (tail starts)
    get (a, b) = BV.extract a b bv

decode :: BV.BitVector -> Instr
decode 0 = Nop
decode bv =
  case BV.extract 31 26 bv of
    0 -> decodeRFormat bv
    2 -> decodeJFormat bv
    3 -> decodeJFormat bv
    x ->
      if x < 16 || x > 19
        then decodeIFormat bv
        else decodeCoprocessor bv

decodeRFormat :: BV.BitVector -> Instr
decodeRFormat = fromList . getFields [6, 5, 5, 5, 5, 6]
  where
    fromList [_, s, t, d, shamt, f] =
      case f of
        0x0c -> Syscall
        0x0d -> Break
        _    -> RInstr (decodeFunct f) s t d shamt
    decodeFunct :: BV.BitVector -> Funct
    decodeFunct 0x20 = Add
    decodeFunct 0x21 = Addu
    -- decodeFunct 0x24 = And
    decodeFunct 0x08 = Jr
    -- decodeFunct 0x27 = Nor
    decodeFunct 0x25 = Or
    decodeFunct 0x2a = Slt
    -- decodeFunct 0x2b = Sltu
    decodeFunct 0x00 = Sll
    decodeFunct 0x02 = Srl
    decodeFunct 0x9  = Jalr
    -- decodeFunct 0x22 = Sub
    -- decodeFunct 0x23 = Subu
    decodeFunct 0x18 = Mult
    decodeFunct 0x12 = Mflo
    decodeFunct 0x10 = Mfhi
    decodeFunct 0x1a = Div
    decodeFunct x    = error $ "Falta decodificar o funct " <> BV.showBin x

decodeIFormat :: BV.BitVector -> Instr
decodeIFormat = fromList . getFields [6, 5, 5, 16]
  where
    fromList [op, s, t, i] = IInstr (decodeOp op) s t i
    decodeOp 0x8  = Addi
    decodeOp 0x9  = Addiu
    decodeOp 0xc  = Andi
    decodeOp 0x4  = Beq
    decodeOp 0x5  = Bne
    decodeOp 0xf  = Lui
    decodeOp 0xd  = Ori
    decodeOp 0x23 = Lw
    decodeOp 0x20 = Lb
    decodeOp 0x2b = Sw
    decodeOp 0xa  = Slti
    decodeOp 0x31 = Lwc1
    decodeOp 0x35 = Ldc1

decodeJFormat :: BV.BitVector -> Instr
decodeJFormat = fromList . getFields [6, 26]
  where
    fromList [op, tgt] = JInstr (decodeOp op) (4 * tgt)
    decodeOp 0x2 = J
    decodeOp 0x3 = Jal

decodeCoprocessor :: BV.BitVector -> Instr
decodeCoprocessor = fromList . getFields [6, 5, 5, 5, 8, 3]
  where
    fromList [op, fmt, ft, fs, fd, funct] =
      FRInstr (decodeOp fmt funct) fmt ft fs fd
    decodeOp 0 _ = Mfc1
    decodeOp 4 _ = Mtc1
