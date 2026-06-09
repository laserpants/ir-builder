module LLVM.IRBuilder.Supply (fresh, freshLabel, freshOperand) where

import Common (Name)
import qualified Data.Text as Text
import LLVM.IRBuilder.Class (MonadIRBuilder (..))
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), overBuilderEnvFreshLabel, overBuilderEnvFreshReg)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

fresh :: (MonadIRBuilder m) => m Name
fresh = do
  modifyIRBuilderEnv (overBuilderEnvFreshReg (+ 1))
  reg <- getsIRBuilderEnv builderEnvFreshReg
  pure (Text.pack (show reg))

freshLabel :: (MonadIRBuilder m) => Name -> m Name
freshLabel hint = do
  modifyIRBuilderEnv (overBuilderEnvFreshLabel (+ 1))
  n <- getsIRBuilderEnv builderEnvFreshLabel
  pure (hint <> Text.pack ("." <> show n))

freshOperand :: (MonadIRBuilder m) => IRType -> m IROperand
freshOperand t = OLocal t <$> fresh
