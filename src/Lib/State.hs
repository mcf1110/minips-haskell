module Lib.State where

import Lib.Registers
import Lib.Memory

type State = (Registers, Memory)