{-# LANGUAGE LambdaCase #-}

module LLVM.IRInstruction.DSL (add) where

import Common (Name)
import Control.Monad.Free (liftF)
import LLVM.IRBuilder (IRBuilder, IRBuilderF (..))
import LLVM.IRBuilder.Supply (freshRegister)
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType)

localOpComponents :: IROperand -> Maybe (Name, IRType)
localOpComponents =
  \case
    OLocal t name ->
      Just (name, t)
    _ ->
      Nothing

add :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
add t a b = do
  reg <- freshRegister t
  let instr =
        IRInstruction
          { instrResult = localOpComponents reg
          , instrOp = IAdd t a b
          }

  liftF $ EmitInstr instr reg
