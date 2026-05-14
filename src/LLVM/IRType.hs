{-# LANGUAGE StrictData #-}

{- |
This module defines the type system for LLVM IR.
-}
module LLVM.IRType (IRType (..)) where

import Common (Name)

{- |
Represents LLVM IR types.

This data type models the LLVM type system, supporting both primitive
types and complex aggregate types. Types can be anonymous or named,
and the type system includes support for SIMD vectors.

==== __Constructors__

[@TInt Int@] Integer type with specified bit width (e.g., i32, i64)
[@TFloat@] Single-precision floating-point type (float)
[@TDouble@] Double-precision floating-point type (double)
[@TVoid@] Void type, used for functions that don't return a value
[@TFun IRType [IRType]@] Function type with return type and parameter types
[@TPtr@] Opaque pointer type (replaces typed pointers in modern LLVM)
[@TStruct [IRType]@] Structure type containing a list of field types
[@TArray Int IRType@] Array type with element count and element type
[@TNamed Name@] Reference to a named type defined elsewhere
[@TOpaque Name@] Opaque type declaration (incomplete type)
[@TVector Int IRType@] SIMD vector type with element count and element type
-}
data IRType
  = TInt Int
  | TFloat
  | TDouble
  | TVoid
  | TFun IRType [IRType]
  | TPtr
  | TStruct [IRType]
  | TArray Int IRType
  | TNamed Name
  | TOpaque Name
  | TVector Int IRType
  deriving (Show, Eq, Ord)
