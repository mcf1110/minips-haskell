module Lib.Operation.TypeFR where

import           Control.Monad.State.Lazy
import           Lib.Operation.Helpers    (modifyReg)
import           Lib.Operation.Infixes
import           Lib.Operation.Types
import qualified Lib.Registers            as R

mfc1 :: RegNum -> RegNum -> Operation ()
mfc1 rt fs = modifyReg (\r -> R.set rt (R.getCop fs r) r)
