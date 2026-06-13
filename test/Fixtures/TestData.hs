{-# LANGUAGE OverloadedStrings #-}

module Fixtures.TestData (
  -- * Types
  typeI1,
  typeI8,
  typeI32,
  typeI64,
  typeFloat,
  typeDouble,
  typeVoid,
  typePtr,
  typePtr32,
  typeArray,
  typeStruct,
  typeVector,

  -- * Operands
  operandLocal32,
  operandLocal64,
  operandGlobal32,
  operandConstInt,
  operandConstFloat,

  -- * Constants
  constInt32,
  constFloat,
  constDouble,
  constNull,
  constStruct,

  -- * Names
  nameA,
  nameB,
  nameC,
  nameBlock,
  nameFunc,

  -- * Instruction operations
  instrAdd,
  instrSub,
  instrMul,
  instrLoad,
  instrStore,

  -- * Terminators
  termRet,
  termBr,
  termCondBr,
)
where

import LLVM.IRInstruction (IRInstrOp (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRName, IRType (..))

-- * Common type values

typeI1 :: IRType
typeI1 = TInt 1

typeI8 :: IRType
typeI8 = TInt 8

typeI32 :: IRType
typeI32 = TInt 32

typeI64 :: IRType
typeI64 = TInt 64

typeFloat :: IRType
typeFloat = TFloat

typeDouble :: IRType
typeDouble = TDouble

typeVoid :: IRType
typeVoid = TVoid

typePtr :: IRType
typePtr = TPtr

typePtr32 :: IRType
typePtr32 = TPtr

typeArray :: IRType
typeArray = TArray 10 (TInt 32)

typeStruct :: IRType
typeStruct = TStruct [TInt 32, TFloat]

typeVector :: IRType
typeVector = TVector 4 (TInt 32)

-- * Common operand values

operandLocal32 :: IROperand
operandLocal32 = OLocal typeI32 "x"

operandLocal64 :: IROperand
operandLocal64 = OLocal typeI64 "y"

operandGlobal32 :: IROperand
operandGlobal32 = OGlobal typeI32 "global"

operandConstInt :: IROperand
operandConstInt = OConstant (CInt 32 42)

operandConstFloat :: IROperand
operandConstFloat = OConstant (CFloat 3.14)

-- * Common constant values

constInt32 :: IRConstant
constInt32 = CInt 32 42

constFloat :: IRConstant
constFloat = CFloat 2.71

constDouble :: IRConstant
constDouble = CDouble 3.14159

constNull :: IRConstant
constNull = CNull typePtr

constStruct :: IRConstant
constStruct = CStruct [CInt 32 1, CFloat 1.5]

-- * Common name values

nameA :: IRName
nameA = "a"

nameB :: IRName
nameB = "b"

nameC :: IRName
nameC = "c"

nameBlock :: IRName
nameBlock = "entry"

nameFunc :: IRName
nameFunc = "test_func"

-- * Common instruction values

instrAdd :: IRInstrOp
instrAdd = IAdd typeI32 operandLocal32 (OLocal typeI32 "y")

instrSub :: IRInstrOp
instrSub = ISub typeI32 operandLocal32 (OLocal typeI32 "y")

instrMul :: IRInstrOp
instrMul = IMul typeI32 operandLocal32 (OLocal typeI32 "y")

instrLoad :: IRInstrOp
instrLoad = ILoad typeI32 operandGlobal32

instrStore :: IRInstrOp
instrStore = IStore operandLocal32 operandGlobal32

-- * Common terminator values

termRet :: IRTerminator
termRet = IRet (Just operandLocal32)

termBr :: IRTerminator
termBr = IBr nameBlock

termCondBr :: IRTerminator
termCondBr = ICondBr operandLocal32 nameBlock "exit"
