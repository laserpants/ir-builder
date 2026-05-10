module LLVM.IRBuilder.Supply (fresh, freshRegister) where

import Common (Name)
import Control.Monad.State (gets, modify)
import LLVM.IRBuilder (IRBuilder)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), overBuilderEnvFresh)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

fresh :: IRBuilder Name
fresh = do
  modify (overBuilderEnvFresh (+ 1))
  fresh <- gets builderEnvFresh
  undefined

freshRegister :: IRType -> IRBuilder IROperand
freshRegister t = OLocal t <$> fresh
