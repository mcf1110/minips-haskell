module Lib.Memory where

import qualified Data.BitVector           as BV
import qualified Data.IntMap.Lazy         as IM
import qualified Data.Vector              as V
import qualified Data.Word                as W

import           Control.Monad            (join, when)
import qualified Control.Monad.State.Lazy as S
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromJust, fromMaybe, isJust,
                                           isNothing)
import           Debug.Trace
import           Lib.Computer.Types
import           Lib.Memory.Cache         (triggerInCache)
import qualified Lib.Memory.Pure          as P
import           Lib.Operation.Types      (Operation)
import           Optics                   (Field2 (_2), Field3 (_3), Lens',
                                           assign, lens, modifying, over, use,
                                           (%), (.~), (^.))
import           Optics.Operators.Unsafe  ((^?!))

get :: Enum i => i -> Operation W.Word32
get ix = do
  triggerInCache Read ix
  S.gets $ P.pureGet ix

getQuarter :: Enum i => i -> Operation W.Word8
getQuarter ix = do
  triggerInCache Read ix
  S.gets $ P.pureGetQuarter ix

getInstruction :: Enum i => i -> Operation W.Word32
getInstruction ix = do
  triggerInCache InstrFetch ix
  S.gets $ P.pureGet ix

set :: (Eq i, Num i, Enum i) => i -> W.Word32 -> Operation ()
set ix v = do
  triggerInCache Write ix
  S.modify $ P.pureSet ix v

-- Helper function
getString :: Enum i => i -> Operation String
getString n = do
  chars <- toChars <$> get address
  let untilNull = takeWhile (/= '\NUL') chars
  if untilNull /= chars
    then return untilNull -- we've reached the end
    else (chars <>) <$> getString (address + 4) -- do another reading
  where
    address = 4 * div (fromEnum n) 4
    offset = mod (fromEnum n) 4
    toChars :: W.Word32 -> String
    toChars w32 =
      let bv = BV.bitVec 32 w32
       in drop
            offset
            [ toEnum $ fromEnum $ bv BV.@: ix
            | ix <- map reverse $ chunksOf 8 [0 .. 31]
            ]
