module LLVM.IRBuilder.Supply (fresh, freshLabel, freshOperand) where

import Common (Name)
import Control.Monad.State (gets, modify)
import qualified Data.Text as Text
import LLVM.IRBuilder (IRBuilder)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), overBuilderEnvFreshLabel, overBuilderEnvFreshReg)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

fresh :: IRBuilder Name
fresh = do
  modify (overBuilderEnvFreshReg (+ 1))
  reg <- gets builderEnvFreshReg
  pure (Text.pack (show reg))

freshLabel :: Name -> IRBuilder Name
freshLabel hint = do
  modify (overBuilderEnvFreshLabel (+ 1))
  n <- gets builderEnvFreshLabel
  pure (hint <> Text.pack ("." <> show n))

freshOperand :: IRType -> IRBuilder IROperand
freshOperand t = OLocal t <$> fresh
