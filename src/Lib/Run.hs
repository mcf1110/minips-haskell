module Lib.Run where

import           Lib.Decode
import qualified Lib.Memory      as M
import           Lib.Print
import qualified Lib.Registers   as R
import           Lib.State

import qualified Data.BitVector  as BV
import qualified Data.Word       as W

import           Data.Maybe      (isJust)

import           Data.List.Split (chunksOf)
import           Debug.Trace

runState :: State -> IO ()
runState st = do
  let (r, m) = st
      pc = R.get 32 r
      ins = decodeInstruction $ M.get pc m
      (st', sc) = runInstruction ins st
  mi <- runSc sc
  let st'' =
        if isJust mi
          then st'
          else st'
  if sc == Die
    then (putStrLn "--- Program Finished ---")
    else (runState st'') -- (printState st'')

runSc :: SC -> IO (Maybe Int)
runSc GetInt = do
  i <- getLine
  return $ Just $ read i
runSc sc = do
  runIO sc
  return Nothing
  where
    runIO (PutInt x)  = print x
    runIO (PutStr x)  = putStrLn x
    runIO (PutChar x) = putChar x
    runIO _           = return ()

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
runInstruction Syscall (r, m) = ((incPC r, m), sc)
  where
    sc =
      case R.get 2 r of
        1 -> PutInt $ fromEnum $ R.get 4 r
        4 ->
          PutStr $
          map (toEnum . fromEnum) $
          takeWhile (/= 0) $
          concat $ [toQuarterWord $ M.get a m | a <- [st,(st + 4) ..]]
          where st = R.get 4 r
                toQuarterWord w =
                  [bv BV.@: ix | ix <- (map reverse) $ chunksOf 8 [0 .. 31]]
                  where
                    bv = BV.bitVec 32 w
        11 -> PutChar $ toEnum . fromEnum $ R.get 4 r
        5 -> GetInt
        10 -> Die
runInstruction ins (r, m) = (eval ins, NoOp)
  where
    infixr 1 $=
    ($=) ad v = R.set ad v r
    ($+$) ra rb = addEnum (R.get ra r) (R.get rb r)
    ($+::) ra im = addEnum (R.get ra r) (BV.nat im)
    ($+:) ra im = addEnum (R.get ra r) (BV.int im)
    ($|:) ra im =
      toEnum $
      fromEnum $ (BV.bitVec 32 $ R.get ra r) BV..|. (BV.zeroExtend 32 im)
    ------
    eval (IInstr Addi rs rt im) = (incPC $ rt $= rs $+: im, m)
    eval (IInstr Addiu rs rt im) = (incPC $ rt $= rs $+:: im, m)
    eval (IInstr Lui rs rt im) =
      (incPC $ rt $= (toEnum $ fromEnum $ (BV.zeroExtend 32 im) BV.<<. 0x10), m)
    eval (IInstr Ori rs rt im) = (incPC $ rt $= rs $|: im, m)
    eval (RInstr Add rs rt rd _) = (incPC $ rd $= rs $+$ rt, m)
    eval a = error $ "Falta implementar: " <> show a

incPC :: R.Registers -> R.Registers
incPC r = R.set 32 (addEnum (R.get 32 r) (4)) r
