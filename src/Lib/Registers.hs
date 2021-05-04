module Lib.Registers
  ( get
  , set
  , getCop
  , setCop
  , getFlag
  , setFlag
  , getF
  , setF
  , getD
  , setD
  , Registers(..)
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Word           as W

import qualified Data.Bifunctor      as B
import qualified Data.Binary.IEEE754 as F
import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.Either         (fromRight)
import           Lib.Computer.Types
import           Optics              (Lens', over, (%), (^.))

_getAt ::
     (Enum a, V.Unbox b) => Lens' Registers (V.Vector b) -> a -> Computer -> b
_getAt lens ix comp = (comp ^. (reg % lens)) V.! fromEnum ix

_setAt ::
     (Enum a, V.Unbox b)
  => Lens' Registers (V.Vector b)
  -> a
  -> b
  -> Computer
  -> Computer
_setAt lens ix v = over (reg % lens) (\regs -> regs V.// [(fromEnum ix, v)])

get :: Enum a => a -> Computer -> W.Word32
get = _getAt gpr

getCop :: Enum a => a -> Computer -> W.Word32
getCop = _getAt fpr

getFlag :: Enum a => a -> Computer -> Bool
getFlag = _getAt ccFlags

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Computer -> Computer
set 0 _  = id
set ix v = _setAt gpr ix v

setCop :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Computer -> Computer
setCop = _setAt fpr

setFlag :: (Eq a, Num a, Enum a) => a -> Bool -> Computer -> Computer
setFlag = _setAt ccFlags

getF :: Enum a => a -> Computer -> Float
getF ix comp = F.wordToFloat $ getCop ix comp

getD :: (Enum a, Num a) => a -> Computer -> Double
getD ix comp = F.wordToDouble $ shiftL word2 32 + word1
  where
    word1, word2 :: W.Word64
    word1 = toEnum . fromEnum $ getCop ix comp
    word2 = toEnum . fromEnum $ getCop (ix + 1) comp

setF :: (Eq a, Num a, Enum a) => a -> Float -> Computer -> Computer
setF ix f = setCop ix (F.floatToWord f)

setD :: (Eq a, Num a, Enum a) => a -> Double -> Computer -> Computer
setD ix d r = setCop (ix + 1) w2 (setCop ix w1 r)
  where
    word, bitMask :: W.Word64
    word = F.doubleToWord d
    bitMask = 0xffffffff
    w2, w1 :: W.Word32
    w1 = toEnum . fromEnum $ bitMask .&. word
    w2 = toEnum . fromEnum $ shiftR word 32
