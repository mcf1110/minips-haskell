module Lib.State where

import Lib.Memory
import Lib.Registers

type State = (Registers, Memory)
