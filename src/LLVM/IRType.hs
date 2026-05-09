{-# LANGUAGE StrictData #-}

module LLVM.IRType (IRType (..)) where

import Common (Name)

data IRType
  = TInt Int
  | TFloat
  | TDouble
  | TVoid
  | TFun IRType [IRType]
  | TPtr IRType
  | TStruct [IRType]
  | TArray Int IRType
  | TNamed Name
  | TOpaque Name
  | TVector Int IRType
  deriving (Show, Eq, Ord)
