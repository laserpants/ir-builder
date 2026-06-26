-- |
-- This module provides smart constructors for creating LLVM IR types.
--
-- These helpers simplify the construction of common type patterns, including
-- integer types, pointers, floating-point types, and aggregate types.
--
-- ==== __Usage__
--
-- This module is designed to be imported qualified when name collisions occur
-- with "LLVM.IROperand.Constructors":
--
-- @
-- import qualified LLVM.IRType.Constructors as T
-- import qualified LLVM.IROperand.Constructors as O
--
-- myFunction :: IRBuilder ()
-- myFunction = do
--   x <- add T.i32 (O.i32 10) (O.i32 20)
--   ...
-- @
--
-- Alternatively, import only non-colliding names unqualified:
--
-- @
-- import LLVM.IRType.Constructors (ptr, void, double)
-- import LLVM.IROperand.Constructors (local, global, float)
-- @
module LLVM.IRType.Constructors
  ( -- * Integer types
    i1,
    i8,
    i16,
    i32,
    i64,
    i128,

    -- * Floating-point types
    float,
    double,

    -- * Pointer and void
    ptr,
    void,

    -- * Aggregate types
    struct,
    array,
    vector,

    -- * Function type
    fun,

    -- * Named type reference
    named,
  )
where

import LLVM.IRType (IRName, IRType (..))

-- * Integer types

{-# INLINE i1 #-}
i1 :: IRType
i1 = TInt 1

{-# INLINE i8 #-}
i8 :: IRType
i8 = TInt 8

{-# INLINE i16 #-}
i16 :: IRType
i16 = TInt 16

{-# INLINE i32 #-}
i32 :: IRType
i32 = TInt 32

{-# INLINE i64 #-}
i64 :: IRType
i64 = TInt 64

{-# INLINE i128 #-}
i128 :: IRType
i128 = TInt 128

-- * Floating-point types

{-# INLINE float #-}
float :: IRType
float = TFloat

{-# INLINE double #-}
double :: IRType
double = TDouble

-- * Pointer and void

{-# INLINE ptr #-}
ptr :: IRType
ptr = TPtr

{-# INLINE void #-}
void :: IRType
void = TVoid

-- * Aggregate types

{-# INLINE struct #-}
struct :: [IRType] -> IRType
struct = TStruct

{-# INLINE array #-}
array :: Int -> IRType -> IRType
array = TArray

{-# INLINE vector #-}
vector :: Int -> IRType -> IRType
vector = TVector

-- * Function type

{-# INLINE fun #-}

-- | Construct a function type: @fun retType [paramTypes]@.
fun :: IRType -> [IRType] -> IRType
fun = TFun

-- * Named type reference

{-# INLINE named #-}

-- | Reference a named type defined elsewhere in the module (e.g. @%MyStruct@).
named :: IRName -> IRType
named = TNamed
