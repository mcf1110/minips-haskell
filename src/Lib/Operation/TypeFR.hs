module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy (get, modify, put)
import           GHC.Float
import           Lib.Computer.Types       (Computer)
import           Lib.Decode               (FFmt (Double, Single, Word))
import           Lib.Operation.Helpers    (modifyReg, w32ToSigned)
import           Lib.Operation.Infixes    (($.=))
import           Lib.Operation.Types      (Operation, RegNum)
import qualified Lib.Registers            as R

type Getter a = RegNum -> Computer -> a

type Setter a = RegNum -> a -> Computer -> Computer

type Calc a = a -> a -> a

mfc1 :: RegNum -> RegNum -> Operation ()
mfc1 rt fs = modify (\comp -> R.set rt (R.getCop fs comp) comp)

mtc1 :: RegNum -> RegNum -> Operation ()
mtc1 rt fs = modify (\comp -> R.setCop fs (R.get rt comp) comp)

mov :: FFmt -> RegNum -> RegNum -> Operation ()
mov Single fs fd = modify (\comp -> R.setCop fd (R.getCop fs comp) comp)
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

clt :: FFmt -> RegNum -> RegNum -> Operation ()
clt Single = condition R.getF (<)
clt Double = condition R.getD (<)

condition :: Getter a -> (a -> a -> Bool) -> RegNum -> RegNum -> Operation ()
condition getter compare ft fs =
  modify
    (\comp ->
       let a = getter fs comp
           b = getter ft comp
        in R.setFlag 0 (compare a b) comp)

convertWith ::
     (a -> b) -> Getter a -> Setter b -> RegNum -> RegNum -> Operation ()
convertWith cvtFn getFn setFn fs fd =
  modify (\comp -> setFn fd (cvtFn $ getFn fs comp) comp)

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
  comp <- get
  let a = getter fs comp
      b = getter ft comp
  put $ setter fd (a `calc` b) comp
