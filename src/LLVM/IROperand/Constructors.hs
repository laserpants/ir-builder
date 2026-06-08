{- |
This module provides smart constructors for creating LLVM IR operands and constants.

These helpers simplify the construction of common operand patterns, including
integer and floating-point constants, null pointers, and references to local
and global values.
-}
module LLVM.IROperand.Constructors (
  -- * Boolean Constants
  falseConst,
  trueConst,

  -- * Integer Constants
  i1Const,
  i8Const,
  i32Const,
  i64Const,
  intConst,

  -- * Floating-Point Constants
  floatConst,
  doubleConst,

  -- * Null Pointer Constants
  nullPtr,

  -- * Operand Constructors
  local,
  global,
  constant,

  -- * Aggregate Constants
  structConst,
  arrayConst,
)
where

import Common (Name)
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRType (IRType (..))

-- * Boolean Constants

{- | Boolean constant representing 'false' (i1 0).

>>> falseConst
OConstant (CInt 1 0)
-}
{-# INLINE falseConst #-}
falseConst :: IROperand
falseConst = OConstant $ CInt 1 0

{- | Boolean constant representing 'true' (i1 1).

>>> trueConst
OConstant (CInt 1 1)
-}
{-# INLINE trueConst #-}
trueConst :: IROperand
trueConst = OConstant $ CInt 1 1

-- * Integer Constants

{- | Create an i1 (1-bit) integer constant.

==== __Parameters__

* 'Integer' - The integer value (typically 0 or 1)

>>> i1Const 0
OConstant (CInt 1 0)
-}
{-# INLINE i1Const #-}
i1Const :: Integer -> IROperand
i1Const = OConstant . CInt 1

{- | Create an i8 (8-bit) integer constant.

==== __Parameters__

* 'Integer' - The integer value

>>> i8Const 42
OConstant (CInt 8 42)
-}
{-# INLINE i8Const #-}
i8Const :: Integer -> IROperand
i8Const = OConstant . CInt 8

{- | Create an i32 (32-bit) integer constant.

==== __Parameters__

* 'Integer' - The integer value

>>> i32Const 1000
OConstant (CInt 32 1000)
-}
{-# INLINE i32Const #-}
i32Const :: Integer -> IROperand
i32Const = OConstant . CInt 32

{- | Create an i64 (64-bit) integer constant.

==== __Parameters__

* 'Integer' - The integer value

>>> i64Const 9999999
OConstant (CInt 64 9999999)
-}
{-# INLINE i64Const #-}
i64Const :: Integer -> IROperand
i64Const = OConstant . CInt 64

{- | Create an integer constant with a custom bit width.

==== __Parameters__

* 'Int' - The bit width
* 'Integer' - The integer value

>>> intConst 16 32768
OConstant (CInt 16 32768)
-}
{-# INLINE intConst #-}
intConst :: Int -> Integer -> IROperand
intConst w = OConstant . CInt w

-- * Floating-Point Constants

{- | Create a single-precision floating-point constant.

==== __Parameters__

* 'Float' - The floating-point value

>>> floatConst 3.14
OConstant (CFloat 3.14)
-}
{-# INLINE floatConst #-}
floatConst :: Float -> IROperand
floatConst = OConstant . CFloat

{- | Create a double-precision floating-point constant.

==== __Parameters__

* 'Double' - The floating-point value

>>> doubleConst 2.71828
OConstant (CDouble 2.71828)
-}
{-# INLINE doubleConst #-}
doubleConst :: Double -> IROperand
doubleConst = OConstant . CDouble

-- * Null Pointer Constants

{- | Create a null pointer constant of the specified type.

==== __Parameters__

* 'IRType' - The type of the null pointer

>>> nullPtr TPtr
OConstant (CNull TPtr)
-}
{-# INLINE nullPtr #-}
nullPtr :: IRType -> IROperand
nullPtr = OConstant . CNull

-- * Operand Constructors

{- | Create a reference to a local SSA value (register).

==== __Parameters__

* 'IRType' - The type of the local value
* 'Name' - The name of the local value

>>> local (TInt 32) "x"
OLocal (TInt 32) "x"
-}
{-# INLINE local #-}
local :: IRType -> Name -> IROperand
local = OLocal

{- | Create a reference to a global symbol.

==== __Parameters__

* 'IRType' - The type of the global symbol
* 'Name' - The name of the global symbol

>>> global TPtr "printf"
OGlobal TPtr "printf"
-}
{-# INLINE global #-}
global :: IRType -> Name -> IROperand
global = OGlobal

{- | Create a constant operand from an 'IRConstant'.

This is a convenience wrapper for the 'OConstant' constructor.

==== __Parameters__

* 'IRConstant' - The constant value

>>> constant (CInt 32 42)
OConstant (CInt 32 42)
-}
{-# INLINE constant #-}
constant :: IRConstant -> IROperand
constant = OConstant

-- * Aggregate Constants

{- | Create a structure constant from a list of field constants.

==== __Parameters__

* ['IRConstant'] - The list of field values

>>> structConst [CInt 32 1, CInt 32 2]
OConstant (CStruct [CInt 32 1, CInt 32 2])
-}
{-# INLINE structConst #-}
structConst :: [IRConstant] -> IROperand
structConst = OConstant . CStruct

{- | Create an array constant with the specified element type.

==== __Parameters__

* 'IRType' - The element type of the array
* ['IRConstant'] - The list of element values

>>> arrayConst (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3]
OConstant (CArray (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3])
-}
{-# INLINE arrayConst #-}
arrayConst :: IRType -> [IRConstant] -> IROperand
arrayConst t = OConstant . CArray t
