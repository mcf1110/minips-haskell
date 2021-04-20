module Lib.Operation.Infixes where

import           Control.Monad.State.Lazy
import qualified Data.Bifunctor           as B
import qualified Data.BitVector           as BV
import qualified Data.Word                as W
import           Lib.Operation.Helpers    (addEnum, modifyReg)
import           Lib.Operation.Types      (Immediate, Operation, RegNum)
import qualified Lib.Registers            as R

infixr 1 $=

($=) :: RegNum -> W.Word32 -> Operation ()
($=) ad v = modifyReg $ R.set ad v

infixr 1 $<-

($<-) :: RegNum -> Operation W.Word32 -> Operation () -- monadic version
($<-) tgt op = op >>= (tgt $=)

infixr 1 $.=

($.=) :: RegNum -> W.Word32 -> Operation ()
($.=) ad v = modifyReg $ R.setCop ad v

($+$) :: RegNum -> RegNum -> Operation W.Word32
($+$) ra rb = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (R.get rb r)

($-$) :: RegNum -> RegNum -> Operation W.Word32
($-$) ra rb = do
  (r, m) <- get
  return $ addEnum (R.get ra r) (1 + 0xffffffff - R.get rb r)

bitwiseWithRegNum ::
     (BV.BitVector -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> RegNum
  -> Operation W.Word32
bitwiseWithRegNum op ra rb = do
  (r, m) <- get
  return $
    toEnum $ fromEnum $ BV.bitVec 32 (R.get ra r) `op` BV.bitVec 32 (R.get rb r)

($|$) :: RegNum -> RegNum -> Operation W.Word32
($|$) = bitwiseWithRegNum (BV..|.)

($&$) :: RegNum -> RegNum -> Operation W.Word32
($&$) = bitwiseWithRegNum (BV..&.)

($^$) :: RegNum -> RegNum -> Operation W.Word32
($^$) = bitwiseWithRegNum (\a b -> BV.not $ BV.xnor a b)

($*$) :: RegNum -> RegNum -> Operation (W.Word32, W.Word32)
($*$) ra rb = do
  (r, m) <- get
  let a = BV.signExtend 32 $ BV.bitVec 32 $ R.get ra r
      b = BV.signExtend 32 $ BV.bitVec 32 $ R.get rb r
      [hi, lo] = map (toEnum . fromEnum) $ BV.split 2 $ BV.least 64 $ a * b
  return (hi, lo)

($/$) :: RegNum -> RegNum -> Operation (W.Word32, W.Word32)
($/$) ra rb = do
  (r, m) <- get
  let toSigned = fromInteger . BV.int . BV.bitVec 32
      a = toSigned $ R.get ra r
      b = toSigned $ R.get rb r
      cvt = toEnum . fromEnum . BV.bitVec 32
      (lo, hi) = B.bimap cvt cvt $ a `quotRem` b
  return (hi, lo)

($<$) :: RegNum -> RegNum -> Operation W.Word32
($<$) ra rb = do
  (r, m) <- get
  let signed rx = BV.int $ BV.bitVec 32 $ R.get rx r
  return $
    if signed ra < signed rb
      then 1
      else 0

($+<$) :: RegNum -> RegNum -> Operation W.Word32
($+<$) ra rb = do
  (r, m) <- get
  let unsigned rx = BV.nat $ BV.bitVec 32 $ R.get rx r
  return $
    if unsigned ra < unsigned rb
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

bitwiseWithImmediate ::
     (BV.BitVector -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> Immediate
  -> Operation W.Word32
bitwiseWithImmediate op ra im = do
  (r, m) <- get
  return $
    toEnum $ fromEnum $ BV.bitVec 32 (R.get ra r) `op` BV.zeroExtend 32 im

($|:) :: RegNum -> Immediate -> Operation W.Word32
($|:) = bitwiseWithImmediate (BV..|.)

($&:) :: RegNum -> Immediate -> Operation W.Word32
($&:) = bitwiseWithImmediate (BV..&.)

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

ashift :: RegNum -> Immediate -> Operation W.Word32
ashift = shiftWith BV.ashr
