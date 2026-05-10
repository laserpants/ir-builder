module LLVM.IRType.Constructors (i1, i8) where

import LLVM.IRType (IRType (..))

i1 :: IRType
i1 = TInt 1

i8 :: IRType
i8 = TInt 8
