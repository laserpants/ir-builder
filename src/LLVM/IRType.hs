{-# LANGUAGE StrictData #-}

-- | This module defines the type system for LLVM IR.
module LLVM.IRType (IRType (..), IRName) where

import Data.Text (Text)

{- | A type alias for names used in LLVM IR (block labels, register names,
function names, type names, etc.).
-}
type IRName = Text

{- | Represents LLVM IR types.

This data type models the LLVM type system, supporting both primitive
types and complex aggregate types. Types can be anonymous or named,
and the type system includes support for SIMD vectors.

__Constructors:__

* 'TInt': integer type with the given bit width (e.g., @i32@, @i64@)
* 'TFloat': single-precision floating-point (@float@)
* 'TDouble': double-precision floating-point (@double@)
* 'TVoid': void type, used for functions that don’t return a value
* 'TFun': function type with return type and parameter types
* 'TPtr': opaque pointer type (replaces typed pointers in modern LLVM)
* 'TStruct': structure type containing a list of field types
* 'TArray': array type with element count and element type
* 'TNamed': reference to a named type defined elsewhere
* 'TOpaque': opaque type declaration (incomplete type)
* 'TVector': SIMD vector type with element count and element type
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
  | TNamed IRName
  | TOpaque IRName
  | TVector Int IRType
  deriving (Show, Eq, Ord)
