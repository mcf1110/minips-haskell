module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy (get, put)
import           GHC.Float
import           Lib.Decode               (FFmt (Double, Single, Word))
import           Lib.Operation.Helpers    (modifyReg, w32ToSigned)
import           Lib.Operation.Infixes    (($.=))
import           Lib.Operation.Types      (Operation, RegNum)
import qualified Lib.Registers            as R

type Getter a = RegNum -> R.Registers -> a

type Setter a = RegNum -> a -> R.Registers -> R.Registers

type Calc a = a -> a -> a

mfc1 :: RegNum -> RegNum -> Operation ()
mfc1 rt fs = modifyReg (\r -> R.set rt (R.getCop fs r) r)

mtc1 :: RegNum -> RegNum -> Operation ()
mtc1 rt fs = modifyReg (\r -> R.setCop fs (R.get rt r) r)

mov :: FFmt -> RegNum -> RegNum -> Operation ()
mov Single fs fd = modifyReg (\r -> R.setCop fd (R.getCop fs r) r)
mov Double fs fd = do
  mov Single fs fd
  mov Single (fs + 1) (fd + 1)

cvtd :: FFmt -> RegNum -> RegNum -> Operation ()
cvtd Single = convertWith float2Double R.getF R.setD
cvtd Word   = convertWith (realToFrac . w32ToSigned) R.getCop R.setD

cvts :: FFmt -> RegNum -> RegNum -> Operation ()
cvts Double = convertWith double2Float R.getD R.setF
cvts Word   = convertWith (realToFrac . w32ToSigned) R.getCop R.setF

cvtw :: FFmt -> RegNum -> RegNum -> Operation ()
cvtw Double = convertWith truncate R.getD R.setCop
cvtw Single = convertWith truncate R.getF R.setCop

convertWith ::
     (a -> b) -> Getter a -> Setter b -> RegNum -> RegNum -> Operation ()
convertWith cvtFn getFn setFn fs fd =
  modifyReg (\r -> setFn fd (cvtFn $ getFn fs r) r)

fadd :: FFmt -> RegNum -> RegNum -> RegNum -> Operation ()
fadd Double = applyCalculation (+) R.getD R.setD
fadd Single = applyCalculation (+) R.getF R.setF

fsub :: FFmt -> RegNum -> RegNum -> RegNum -> Operation ()
fsub Double = applyCalculation (-) R.getD R.setD
fsub Single = applyCalculation (-) R.getF R.setF

fdiv :: FFmt -> RegNum -> RegNum -> RegNum -> Operation ()
fdiv Double = applyCalculation (/) R.getD R.setD
fdiv Single = applyCalculation (/) R.getF R.setF

fmul :: FFmt -> RegNum -> RegNum -> RegNum -> Operation ()
fmul Double = applyCalculation (*) R.getD R.setD
fmul Single = applyCalculation (*) R.getF R.setF

applyCalculation ::
     Calc a
  -> Getter a
  -> Setter a
  -> RegNum
  -> RegNum
  -> RegNum
  -> Operation ()
applyCalculation calc getter setter ft fs fd = do
  (r, m) <- get
  let a = getter fs r
      b = getter ft r
  put (setter fd (a `calc` b) r, m)
