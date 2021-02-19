module Lib.Decode where

import qualified Data.Word as W
import qualified Data.BitVector as BV

import Lib.Segment


data Instr = 
    RInstr { funct :: Funct, rs:: BV.BitVector, rt :: BV.BitVector, rd :: BV.BitVector, shamt :: BV.BitVector} |
    IInstr { iop :: IOp, rs:: BV.BitVector, rt :: BV.BitVector, immediate :: BV.BitVector } | 
    JInstr { jop :: JOp, tgt:: BV.BitVector} |
    Syscall deriving (Show, Eq)


data Funct = Add | Addu | And | Jr | Nor | Or | Slt | Sltu | Sll | Srl | Sub | Subu deriving (Show, Eq)
data IOp = Addi | Addiu | Beq | Bne  | Lui | Ori deriving (Show, Eq)
data JOp = J | Jal deriving (Show, Eq)

type Program = [Instr]

decodeProgram :: Segment -> Program
decodeProgram = map (decode . wordToBV)

wordToBV :: W.Word32 -> BV.BitVector
wordToBV = BV.bitVec 32

getFields :: [Int] -> BV.BitVector -> [BV.BitVector]
getFields sizes bv = map get offsets
    where
        starts = scanr (+) 0 sizes
        offsets = zip (map (subtract 1) starts) (tail starts)
        get (a, b) = BV.extract a b bv

decode :: BV.BitVector -> Instr
decode bv = case (BV.extract 31 26 bv) of
    0 -> decodeRFormat bv
    2 -> decodeJFormat bv
    3 -> decodeJFormat bv
    _ -> decodeIFormat bv

decodeRFormat :: BV.BitVector -> Instr
decodeRFormat = fromList . getFields [6,5,5,5,5,6]
    where 
        fromList [_ ,s,t,d,shamt, f] = if f == 0x0c then Syscall else RInstr (decodeFunct f) s t d shamt
        decodeFunct :: BV.BitVector -> Funct
        decodeFunct 0x20 = Add
        decodeFunct 0x21 = Addu
        decodeFunct 0x24 = And
        decodeFunct 0x08 = Jr
        decodeFunct 0x27 = Nor
        decodeFunct 0x25 = Or
        decodeFunct 0x2a = Slt
        decodeFunct 0x2b = Sltu
        decodeFunct 0x00 = Sll
        decodeFunct 0x02 = Srl
        decodeFunct 0x22 = Sub
        decodeFunct 0x23 = Subu

decodeIFormat :: BV.BitVector -> Instr
decodeIFormat =  fromList . getFields [6,5,5,16]
    where 
        fromList [op, s, t, i] = IInstr (decodeOp op) s t i
        decodeOp 0x8 = Addi
        decodeOp 0x9 = Addiu
        decodeOp 0x4 = Beq
        decodeOp 0x5 = Bne
        decodeOp 0xf = Lui
        decodeOp 0xd = Ori

decodeJFormat :: BV.BitVector -> Instr
decodeJFormat = fromList . getFields [6, 26]
    where 
        fromList [op, tgt] = JInstr (decodeOp op) (4*tgt)
        decodeOp 0x2 = J