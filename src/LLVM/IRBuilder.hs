module LLVM.IRBuilder (IRBuilder) where

import Control.Monad.Free (Free)
import LLVM.IRInstruction (IRInstrOpF)
import LLVM.IROperand (IROperand)
import LLVM.IRType (IRType)

type IRBuilder = Free (IRInstrOpF IROperand IRType)
