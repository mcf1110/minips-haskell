module Lib.Computer.MemoryTraceTypes where

import qualified Data.Word as W

type Address = W.Word32

type Latency = Int

type Line = W.Word32

type MemoryTrace = (MemoryAccessType, Address, Line)

data MemoryAccessType
  = Read
  | InstrFetch
  | Write
  deriving (Show, Eq)
