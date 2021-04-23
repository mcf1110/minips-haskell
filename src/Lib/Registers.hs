module Lib.Registers where

import qualified Data.Vector         as V
import qualified Data.Word           as W

import qualified Data.Bifunctor      as B
import qualified Data.Binary.IEEE754 as F
import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.Either         (fromRight)

type GPR = V.Vector W.Word32

type FPR = V.Vector W.Word32

type CCFlags = V.Vector Bool

type Registers = (GPR, (FPR, CCFlags)) -- oh god why

startingRegisters :: Registers
startingRegisters = (gpr, coprocessor)
  where
    gpr =
      V.replicate 35 0 V.//
      [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]
    coprocessor = (V.replicate 32 0, V.replicate 8 False)

get :: Enum a => a -> Registers -> W.Word32
get ix r = fst r V.! fromEnum ix

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Registers -> Registers
set 0 _  = id
set ix v = B.first (\gpr -> gpr V.// [(fromEnum ix, v)])

getCop :: Enum a => a -> Registers -> W.Word32
getCop ix r = fst (snd r) V.! fromEnum ix

setCop :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Registers -> Registers
setCop ix v = B.second (\(fpr, cc) -> (fpr V.// [(fromEnum ix, v)], cc))

getF :: Enum a => a -> Registers -> Float
getF ix r = F.wordToFloat $ getCop ix r

getD :: (Enum a, Num a) => a -> Registers -> Double
getD ix r = F.wordToDouble $ shiftL word2 32 + word1
  where
    word1, word2 :: W.Word64
    word1 = toEnum . fromEnum $ getCop ix r
    word2 = toEnum . fromEnum $ getCop (ix + 1) r

setF :: (Eq a, Num a, Enum a) => a -> Float -> Registers -> Registers
setF ix f = setCop ix (F.floatToWord f)

setD :: (Eq a, Num a, Enum a) => a -> Double -> Registers -> Registers
setD ix d r = setCop (ix + 1) w2 (setCop ix w1 r)
  where
    word, bitMask :: W.Word64
    word = F.doubleToWord d
    bitMask = 0xffffffff
    w2, w1 :: W.Word32
    w1 = toEnum . fromEnum $ bitMask .&. word
    w2 = toEnum . fromEnum $ shiftR word 32

getFlag :: (Enum a, Num a) => a -> Registers -> Bool
getFlag ix r = snd (snd r) V.! fromEnum ix

setFlag :: (Eq a, Num a, Enum a) => a -> Bool -> Registers -> Registers
setFlag ix v = B.second (\(fpr, cc) -> (fpr, cc V.// [(fromEnum ix, v)]))
