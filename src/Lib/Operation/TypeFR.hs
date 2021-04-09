module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy (get)
import           Lib.Decode               (FFmt (Double, Single, Word))
import           Lib.Operation.Helpers    (modifyReg)
import           Lib.Operation.Infixes    (($.=))
import           Lib.Operation.Types      (Operation, RegNum)
import qualified Lib.Registers            as R

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
cvtd Single = convertWith R.getF R.setD
cvtd Word   = convertWith R.getCop R.setD

cvts :: FFmt -> RegNum -> RegNum -> Operation ()
cvts Double = convertWith R.getD R.setF
cvts Word   = convertWith R.getCop R.setF

convertWith ::
     (Real a, Fractional t1)
  => (t2 -> R.Registers -> a)
  -> (t3 -> t1 -> R.Registers -> R.Registers)
  -> t2
  -> t3
  -> Operation ()
convertWith get set fs fd = modifyReg (\r -> set fd (realToFrac $ get fs r) r)
