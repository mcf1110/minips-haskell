module Lib.Run where

import           Lib.Decode
import qualified Lib.Memory     as M
import           Lib.Print
import qualified Lib.Registers  as R
import           Lib.State

import qualified Data.BitVector as BV
import qualified Data.Word      as W

runState :: State -> IO ()
runState st = do
  printState st
  let (r, m) = st
      pc = R.get 32 r
      ins = decodeInstruction $ M.get pc m
      st' = runInstruction ins st
  print ins
  printState st'

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum $ (fromEnum x) + (fromEnum y)

runInstruction :: Instr -> State -> State
runInstruction (IInstr Addi rs rt im) (r, m) =
  (R.set rt (addEnum (R.get rs r) (BV.nat im)) r, m)
