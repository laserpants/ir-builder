module LLVM.IRType.HasType (HasTypeIR (..)) where

import LLVM.IRType (IRType (..))

class HasTypeIR t where
  irTypeOf :: t -> IRType

instance HasTypeIR IRType where
  irTypeOf = id
