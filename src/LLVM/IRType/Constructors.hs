{- |
This module provides smart constructors for creating LLVM IR types.

These helpers simplify the construction of common type patterns, including
integer types, pointers, and aggregate types.

==== __Usage__

This module is designed to be imported qualified when name collisions occur
with "LLVM.IROperand.Constructors":

@
import qualified LLVM.IRType.Constructors as T
import qualified LLVM.IROperand.Constructors as O

myFunction :: IRBuilder ()
myFunction = do
  x <- add T.i32 (O.i32 10) (O.i32 20)
  ...
@

Alternatively, import only non-colliding names unqualified:

@
import LLVM.IRType.Constructors (ptr, void)
import LLVM.IROperand.Constructors (local, global, float, double)
@
-}
module LLVM.IRType.Constructors (
  i1,
  i8,
  i32,
  i64,
  ptr,
  void,
  struct,
)
where

import LLVM.IRType (IRType (..))

{-# INLINE i1 #-}
i1 :: IRType
i1 = TInt 1

{-# INLINE i8 #-}
i8 :: IRType
i8 = TInt 8

{-# INLINE i32 #-}
i32 :: IRType
i32 = TInt 32

{-# INLINE i64 #-}
i64 :: IRType
i64 = TInt 64

{-# INLINE ptr #-}
ptr :: IRType
ptr = TPtr

{-# INLINE void #-}
void :: IRType
void = TVoid

{-# INLINE struct #-}
struct :: [IRType] -> IRType
struct = TStruct
