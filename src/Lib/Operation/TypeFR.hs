module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy (get)
import           GHC.Float
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
cvtd Single = convertWith float2Double R.getF R.setD
cvtd Word   = convertWith realToFrac R.getCop R.setD

cvts :: FFmt -> RegNum -> RegNum -> Operation ()
cvts Double = convertWith double2Float R.getD R.setF
cvts Word   = convertWith realToFrac R.getCop R.setF

cvtw :: FFmt -> RegNum -> RegNum -> Operation ()
cvtw Double = convertWith truncate R.getD R.setCop
cvtw Single = convertWith truncate R.getF R.setCop

convertWith ::
     (t1 -> t2)
  -> (t3 -> R.Registers -> t1)
  -> (t4 -> t2 -> R.Registers -> R.Registers)
  -> t3
  -> t4
  -> Operation ()
convertWith cvtFn get set fs fd = modifyReg (\r -> set fd (cvtFn $ get fs r) r)
