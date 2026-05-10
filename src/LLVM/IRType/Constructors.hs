module LLVM.IRType.Constructors (
  i1,
  i8,
  i32,
  i64,
  ptr,
  i8Ptr,
) where

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
ptr :: IRType -> IRType
ptr = TPtr

{-# INLINE i8Ptr #-}
i8Ptr :: IRType
i8Ptr = ptr i8
