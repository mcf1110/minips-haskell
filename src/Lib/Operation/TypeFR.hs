module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy (get)
import           Lib.Decode               (FFmt (Double, Single))
import           Lib.Operation.Helpers    (modifyReg)
import           Lib.Operation.Infixes    (($.=))
import           Lib.Operation.Types
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
