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

import qualified Data.Vector         as V
import qualified Data.Word           as W

import qualified Data.Bifunctor      as B
import qualified Data.Binary.IEEE754 as F
import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.Either         (fromRight)
import           Lib.Computer.Types
import           Optics              (Lens', over, (%), (^.))

-- TODO refactor using these
-- _getAt :: Enum a => Lens' Computer (V.Vector b) -> a -> Computer -> b
-- _getAt lens ix comp = (comp ^. lens) V.! fromEnum ix
-- _setAt :: Enum a => Lens' Computer (V.Vector b) -> a -> b -> Computer -> Computer
-- _setAt lens ix v = over lens (\regs -> regs V.// [(fromEnum ix, v)])
get :: Enum a => a -> Computer -> W.Word32
get ix comp = (comp ^. reg % gpr) V.! fromEnum ix

set :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Computer -> Computer
set 0 _  = id
set ix v = over (reg % gpr) (\regs -> regs V.// [(fromEnum ix, v)])

getCop :: Enum a => a -> Computer -> W.Word32
getCop ix comp = (comp ^. reg % fpr) V.! fromEnum ix

setCop :: (Eq a, Num a, Enum a) => a -> W.Word32 -> Computer -> Computer
setCop ix v = over (reg % fpr) (\regs -> regs V.// [(fromEnum ix, v)])

getFlag :: (Enum a, Num a) => a -> Computer -> Bool
getFlag ix comp = (comp ^. reg % ccFlags) V.! fromEnum ix

setFlag :: (Eq a, Num a, Enum a) => a -> Bool -> Computer -> Computer
setFlag ix v = over (reg % ccFlags) (\cc -> cc V.// [(fromEnum ix, v)])

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
