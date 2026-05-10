module LLVM.IRBuilder.Supply (fresh, freshOperand) where

import Common (Name)
import Control.Monad.State (gets, modify)
import LLVM.IRBuilder (IRBuilder)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), overBuilderEnvFresh)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

fresh :: IRBuilder Name
fresh = do
  modify (overBuilderEnvFresh (+ 1))
  reg <- gets builderEnvFresh
  undefined

freshOperand :: IRType -> IRBuilder IROperand
freshOperand t = OLocal t <$> fresh
