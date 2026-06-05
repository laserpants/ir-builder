module LLVM.IRType.IsTyped (IRIsTyped (..)) where

import LLVM.IRType (IRType (..))

class IRIsTyped t where
  irTypeOf :: t -> IRType

instance IRIsTyped IRType where
  irTypeOf = id
