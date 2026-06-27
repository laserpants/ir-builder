{- | Smart constructors for 'IROperand' and 'IRConstant' values, including
integer and floating-point constants, null pointers, and references to
local and global symbols.

This module is designed to be imported qualified when name collisions occur
with "LLVM.IRType.Constructors":

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
import LLVM.IROperand.Constructors (local, global, nullPtr, float, double)
import LLVM.IRType.Constructors (i32, ptr, void)
@
-}
module LLVM.IROperand.Constructors (
  -- * Boolean constants
  false,
  true,
  unit,

  -- * Integer constants
  i1,
  i8,
  i32,
  i64,
  int,

  -- * Floating-point constants
  float,
  double,

  -- * Null pointer constants
  nullPtr,

  -- * Operand constructors
  local,
  global,
  constant,

  -- * Aggregate constants
  struct,
  array,
)
where

import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRType (IRName, IRType (..))

-- * Boolean constants

{- | Boolean constant representing 'false' (i1 0).

>>> false
OConstant (CInt 1 0)
-}
{-# INLINE false #-}
false :: IROperand
false = OConstant $ CInt 1 0

{- | Boolean constant representing 'true' (i1 1).

>>> true
OConstant (CInt 1 1)
-}
{-# INLINE true #-}
true :: IROperand
true = OConstant $ CInt 1 1

{- | Unit constant (i1 1).

Commonly used as a placeholder value or to represent a successful/present state.

>>> unit
OConstant (CInt 1 1)
-}
{-# INLINE unit #-}
unit :: IROperand
unit = OConstant $ CInt 1 1

-- * Integer constants

{- | Create an @i1@ (1-bit) integer constant from any 'Integral' value.

>>> i1 0
OConstant (CInt 1 0)
-}
{-# INLINE i1 #-}
i1 :: (Integral a) => a -> IROperand
i1 = OConstant . CInt 1 . toInteger

{- | Create an @i8@ (8-bit) integer constant from any 'Integral' value.

>>> i8 42
OConstant (CInt 8 42)
-}
{-# INLINE i8 #-}
i8 :: (Integral a) => a -> IROperand
i8 = OConstant . CInt 8 . toInteger

{- | Create an @i32@ (32-bit) integer constant from any 'Integral' value.

>>> i32 1000
OConstant (CInt 32 1000)
-}
{-# INLINE i32 #-}
i32 :: (Integral a) => a -> IROperand
i32 = OConstant . CInt 32 . toInteger

{- | Create an @i64@ (64-bit) integer constant from any 'Integral' value.

>>> i64 9999999
OConstant (CInt 64 9999999)
-}
{-# INLINE i64 #-}
i64 :: (Integral a) => a -> IROperand
i64 = OConstant . CInt 64 . toInteger

{- | Create an integer constant with a custom bit width.

Takes the bit width as an 'Int' and the value as any 'Integral'.

>>> int 16 32768
OConstant (CInt 16 32768)
-}
{-# INLINE int #-}
int :: (Integral a) => Int -> a -> IROperand
int w = OConstant . CInt w . toInteger

-- * Floating-point constants

{- | Create a single-precision floating-point constant.

>>> float 3.14
OConstant (CFloat 3.14)
-}
{-# INLINE float #-}
float :: Float -> IROperand
float = OConstant . CFloat

{- | Create a double-precision floating-point constant.

>>> double 2.71828
OConstant (CDouble 2.71828)
-}
{-# INLINE double #-}
double :: Double -> IROperand
double = OConstant . CDouble

-- * Null pointer constants

{- | Create a null pointer constant of the specified type.

>>> nullPtr TPtr
OConstant (CNull TPtr)
-}
{-# INLINE nullPtr #-}
nullPtr :: IRType -> IROperand
nullPtr = OConstant . CNull

-- * Operand constructors

{- | Create a reference to a local SSA value (register).

Takes the type and name of the SSA register.

>>> local (TInt 32) "x"
OLocal (TInt 32) "x"
-}
{-# INLINE local #-}
local :: IRType -> IRName -> IROperand
local = OLocal

{- | Create a reference to a global symbol.

Takes the type and name of the global.

>>> global TPtr "printf"
OGlobal TPtr "printf"
-}
{-# INLINE global #-}
global :: IRType -> IRName -> IROperand
global = OGlobal

{- | Wrap an 'IRConstant' as an 'IROperand'. Convenience alias for 'OConstant'.

>>> constant (CInt 32 42)
OConstant (CInt 32 42)
-}
{-# INLINE constant #-}
constant :: IRConstant -> IROperand
constant = OConstant

-- * Aggregate constants

{- | Create a structure constant from a list of field constants.

>>> struct [CInt 32 1, CInt 32 2]
OConstant (CStruct [CInt 32 1, CInt 32 2])
-}
{-# INLINE struct #-}
struct :: [IRConstant] -> IROperand
struct = OConstant . CStruct

{- | Create an array constant with the given element type and values.

>>> array (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3]
OConstant (CArray (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3])
-}
{-# INLINE array #-}
array :: IRType -> [IRConstant] -> IROperand
array t = OConstant . CArray t
