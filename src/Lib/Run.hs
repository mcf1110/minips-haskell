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
      (st', sc) = runInstruction ins st
  print ins
  printState st'

addEnum :: (Enum a, Enum b) => a -> b -> W.Word32
addEnum x y = toEnum $ (fromEnum x) + (fromEnum y)

data SC
  = PutInt Int
  | PutStr String
  | PutChar Char
  | GetInt
  | Die
  | NoOp
  deriving (Show, Eq)

runInstruction :: Instr -> State -> (State, SC)
runInstruction Syscall (r, m) = ((r, m), sc)
  where
    sc =
      case R.get 2 r of
        1  -> PutInt $ fromEnum $ R.get 4 r
        4  -> PutStr $ ""
        11 -> PutChar $ toEnum . fromEnum $ R.get 4 r
        5  -> GetInt
        10 -> Die
runInstruction ins (r, m) = (eval ins, NoOp)
  where
    infixr 1 $=
    ($=) ad v = R.set ad v r
    ($+$) ra rb = addEnum (R.get ra r) (R.get rb r)
    ($+:) ra im = addEnum (R.get ra r) (BV.nat im)
    eval (IInstr Addi rs rt im)  = (rt $= rs $+: im, m)
    eval (RInstr Add rs rt rd _) = (rd $= rs $+$ rt, m)
    eval a                       = error $ "Falta implementar: " <> show a
