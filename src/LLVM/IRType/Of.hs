module LLVM.IRType.Of (IRTypeOf (..)) where

import LLVM.IRType (IRType (..))

class IRTypeOf t where
  irTypeOf :: t -> IRType

instance IRTypeOf IRType where
  irTypeOf = id
