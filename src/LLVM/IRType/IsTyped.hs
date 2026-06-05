module LLVM.IRType.IsTyped (IsTypedIR (..)) where

import LLVM.IRType (IRType (..))

class IsTypedIR t where
  irTypeOf :: t -> IRType

instance IsTypedIR IRType where
  irTypeOf = id
