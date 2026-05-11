module LLVM.IRBuilder.Supply (fresh, freshOperand) where

import Common (Name)
import Control.Monad.State (gets, modify)
import qualified Data.Text as Text
import LLVM.IRBuilder (IRBuilder)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), overBuilderEnvFresh)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

fresh :: IRBuilder Name
fresh = do
  modify (overBuilderEnvFresh (+ 1))
  reg <- gets builderEnvFresh
  pure (Text.pack (show reg))

freshOperand :: IRType -> IRBuilder IROperand
freshOperand t = do
  hint <- gets builderEnvNameHint
  case hint of
    Just name -> do
      modify $ \env -> env{builderEnvNameHint = Nothing}
      pure (OLocal t name)
    Nothing ->
      OLocal t <$> fresh
