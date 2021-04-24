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
($=) ad v = modify $ R.set ad v

infixr 1 $<-

($<-) :: RegNum -> Operation W.Word32 -> Operation () -- monadic version
($<-) tgt op = op >>= (tgt $=)

infixr 1 $.=

($.=) :: RegNum -> W.Word32 -> Operation ()
($.=) ad v = modify $ R.setCop ad v

($+$) :: RegNum -> RegNum -> Operation W.Word32
($+$) ra rb = do
  comp <- get
  return $ addEnum (R.get ra comp) (R.get rb comp)

($-$) :: RegNum -> RegNum -> Operation W.Word32
($-$) ra rb = do
  comp <- get
  return $ addEnum (R.get ra comp) (1 + 0xffffffff - R.get rb comp)

bitwiseWithRegNum ::
     (BV.BitVector -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> RegNum
  -> Operation W.Word32
bitwiseWithRegNum op ra rb = do
  comp <- get
  return $
    toEnum $
    fromEnum $ BV.bitVec 32 (R.get ra comp) `op` BV.bitVec 32 (R.get rb comp)

($|$) :: RegNum -> RegNum -> Operation W.Word32
($|$) = bitwiseWithRegNum (BV..|.)

($&$) :: RegNum -> RegNum -> Operation W.Word32
($&$) = bitwiseWithRegNum (BV..&.)

($^$) :: RegNum -> RegNum -> Operation W.Word32
($^$) = bitwiseWithRegNum (\a b -> BV.not $ BV.xnor a b)

($*$) :: RegNum -> RegNum -> Operation (W.Word32, W.Word32)
($*$) ra rb = do
  comp <- get
  let a = BV.signExtend 32 $ BV.bitVec 32 $ R.get ra comp
      b = BV.signExtend 32 $ BV.bitVec 32 $ R.get rb comp
      [hi, lo] = map (toEnum . fromEnum) $ BV.split 2 $ BV.least 64 $ a * b
  return (hi, lo)

($/$) :: RegNum -> RegNum -> Operation (W.Word32, W.Word32)
($/$) ra rb = do
  comp <- get
  let toSigned = fromInteger . BV.int . BV.bitVec 32
      a = toSigned $ R.get ra comp
      b = toSigned $ R.get rb comp
      cvt = toEnum . fromEnum . BV.bitVec 32
      (lo, hi) = B.bimap cvt cvt $ a `quotRem` b
  return (hi, lo)

($<$) :: RegNum -> RegNum -> Operation W.Word32
($<$) ra rb = do
  comp <- get
  let signed rx = BV.int $ BV.bitVec 32 $ R.get rx comp
  return $
    if signed ra < signed rb
      then 1
      else 0

($+<$) :: RegNum -> RegNum -> Operation W.Word32
($+<$) ra rb = do
  comp <- get
  let unsigned rx = BV.nat $ BV.bitVec 32 $ R.get rx comp
  return $
    if unsigned ra < unsigned rb
      then 1
      else 0

($<:) :: RegNum -> Immediate -> Operation W.Word32
($<:) ra im = do
  comp <- get
  let signed rx = BV.int $ BV.bitVec 32 $ R.get rx comp
      signedIm = BV.int $ BV.bitVec 32 im
  return $
    if signed ra < signedIm
      then 1
      else 0

($+:) :: RegNum -> Immediate -> Operation W.Word32
($+:) ra im = do
  comp <- get
  return $ addEnum (R.get ra comp) (BV.int im)

bitwiseWithImmediate ::
     (BV.BitVector -> BV.BitVector -> BV.BitVector)
  -> RegNum
  -> Immediate
  -> Operation W.Word32
bitwiseWithImmediate op ra im = do
  comp <- get
  return $
    toEnum $ fromEnum $ BV.bitVec 32 (R.get ra comp) `op` BV.zeroExtend 32 im

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
  comp <- get
  return $
    (toEnum . fromEnum) $
    op (BV.bitVec 32 $ R.get ra comp) $ BV.zeroExtend 32 im

($>>:) :: RegNum -> Immediate -> Operation W.Word32
($>>:) = shiftWith BV.shr

($<<:) :: RegNum -> Immediate -> Operation W.Word32
($<<:) = shiftWith BV.shl

ashift :: RegNum -> Immediate -> Operation W.Word32
ashift = shiftWith BV.ashr
