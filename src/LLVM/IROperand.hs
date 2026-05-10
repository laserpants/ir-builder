{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module LLVM.IROperand (
  IRConstant (..),
  IROperand (..),
  IRTerminator (..),
  opComponents,
) where

import Common (Name)
import LLVM.IRType (IRType (..))

data IRConstant
  = CInt Int Integer
  | CFloat Float
  | CDouble Double
  | CNull IRType
  | CStruct [IRConstant]
  | CArray IRType [IRConstant]
  deriving (Show, Eq, Ord)

data IROperand
  = OLocal IRType Name
  | OGlobal IRType Name
  | OConstant IRConstant
  deriving (Show, Eq, Ord)

data IRTerminator
  = IRet IROperand
  | IBr Name
  | ICondBr IROperand Name Name
  | ISwitch IROperand Name [(IRConstant, Name)]
  | IUnreachable
  deriving (Show, Eq, Ord)

opComponents :: IROperand -> Maybe (Name, IRType)
opComponents =
  \case
    OLocal t name ->
      Just (name, t)
    OGlobal t name ->
      Just (name, t)
    _ ->
      Nothing
