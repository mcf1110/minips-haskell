module Lib.Computer.MemoryTraceTypes where

import qualified Data.Word as W

type Address = W.Word32

type Latency = Int

type Line = W.Word32

type MemoryTrace = (MemoryTraceType, Address, Line)

data MemoryTraceType
  = Read
  | InstrFetch
  | Write
  deriving (Show)
