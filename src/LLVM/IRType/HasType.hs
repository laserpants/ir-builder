{-# LANGUAGE LambdaCase #-}

module LLVM.IRType.HasType (HasTypeIR (..)) where

import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors

class HasTypeIR t where
  irTypeOf :: t -> IRType

instance HasTypeIR IRType where
  irTypeOf = id

instance HasTypeIR IRConstant where
  irTypeOf =
    \case
      CInt n _ ->
        TInt n
      CFloat{} ->
        TFloat
      CDouble{} ->
        TDouble
      CNull{} ->
        TPtr
      CStruct ts ->
        struct (irTypeOf <$> ts)
      CArray t cs ->
        array (length cs) t

instance HasTypeIR IROperand where
  irTypeOf =
    \case
      OLocal t _ ->
        t
      OGlobal t _ ->
        t
      OConstant c ->
        irTypeOf c
