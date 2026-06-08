-- |
-- This module provides smart constructors for creating LLVM IR operands and constants.
--
-- These helpers simplify the construction of common operand patterns, including
-- integer and floating-point constants, null pointers, and references to local
-- and global values.
--
-- ==== __Usage__
--
-- This module is designed to be imported qualified when name collisions occur
-- with "LLVM.IRType.Constructors":
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
-- import LLVM.IROperand.Constructors (local, global, nullPtr, float, double)
-- import LLVM.IRType.Constructors (i32, ptr, void)
-- @
module LLVM.IROperand.Constructors
  ( -- * Boolean Constants
    false,
    true,
    unit,

    -- * Integer Constants
    i1,
    i8,
    i32,
    i64,
    int,

    -- * Floating-Point Constants
    float,
    double,

    -- * Null Pointer Constants
    nullPtr,

    -- * Operand Constructors
    local,
    global,
    constant,

    -- * Aggregate Constants
    struct,
    array,
  )
where

import Common (Name)
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRType (IRType (..))

-- * Boolean Constants

-- | Boolean constant representing 'false' (i1 0).
--
-- >>> false
-- OConstant (CInt 1 0)
{-# INLINE false #-}
false :: IROperand
false = OConstant $ CInt 1 0

-- | Boolean constant representing 'true' (i1 1).
--
-- >>> true
-- OConstant (CInt 1 1)
{-# INLINE true #-}
true :: IROperand
true = OConstant $ CInt 1 1

-- | Unit constant (i1 1).
--
-- Commonly used as a placeholder value or to represent a successful/present state.
--
-- >>> unit
-- OConstant (CInt 1 1)
{-# INLINE unit #-}
unit :: IROperand
unit = OConstant $ CInt 1 1

-- * Integer Constants

-- | Create an i1 (1-bit) integer constant.
--
-- ==== __Parameters__
--
-- * 'Integer' - The integer value (typically 0 or 1)
--
-- >>> i1 0
-- OConstant (CInt 1 0)
{-# INLINE i1 #-}
i1 :: Integer -> IROperand
i1 = OConstant . CInt 1

-- | Create an i8 (8-bit) integer constant.
--
-- ==== __Parameters__
--
-- * 'Integer' - The integer value
--
-- >>> i8 42
-- OConstant (CInt 8 42)
{-# INLINE i8 #-}
i8 :: Integer -> IROperand
i8 = OConstant . CInt 8

-- | Create an i32 (32-bit) integer constant.
--
-- ==== __Parameters__
--
-- * 'Integer' - The integer value
--
-- >>> i32 1000
-- OConstant (CInt 32 1000)
{-# INLINE i32 #-}
i32 :: Integer -> IROperand
i32 = OConstant . CInt 32

-- | Create an i64 (64-bit) integer constant.
--
-- ==== __Parameters__
--
-- * 'Integer' - The integer value
--
-- >>> i64 9999999
-- OConstant (CInt 64 9999999)
{-# INLINE i64 #-}
i64 :: Integer -> IROperand
i64 = OConstant . CInt 64

-- | Create an integer constant with a custom bit width.
--
-- ==== __Parameters__
--
-- * 'Int' - The bit width
-- * 'Integer' - The integer value
--
-- >>> int 16 32768
-- OConstant (CInt 16 32768)
{-# INLINE int #-}
int :: Int -> Integer -> IROperand
int w = OConstant . CInt w

-- * Floating-Point Constants

-- | Create a single-precision floating-point constant.
--
-- ==== __Parameters__
--
-- * 'Float' - The floating-point value
--
-- >>> float 3.14
-- OConstant (CFloat 3.14)
{-# INLINE float #-}
float :: Float -> IROperand
float = OConstant . CFloat

-- | Create a double-precision floating-point constant.
--
-- ==== __Parameters__
--
-- * 'Double' - The floating-point value
--
-- >>> double 2.71828
-- OConstant (CDouble 2.71828)
{-# INLINE double #-}
double :: Double -> IROperand
double = OConstant . CDouble

-- * Null Pointer Constants

-- | Create a null pointer constant of the specified type.
--
-- ==== __Parameters__
--
-- * 'IRType' - The type of the null pointer
--
-- >>> nullPtr TPtr
-- OConstant (CNull TPtr)
{-# INLINE nullPtr #-}
nullPtr :: IRType -> IROperand
nullPtr = OConstant . CNull

-- * Operand Constructors

-- | Create a reference to a local SSA value (register).
--
-- ==== __Parameters__
--
-- * 'IRType' - The type of the local value
-- * 'Name' - The name of the local value
--
-- >>> local (TInt 32) "x"
-- OLocal (TInt 32) "x"
{-# INLINE local #-}
local :: IRType -> Name -> IROperand
local = OLocal

-- | Create a reference to a global symbol.
--
-- ==== __Parameters__
--
-- * 'IRType' - The type of the global symbol
-- * 'Name' - The name of the global symbol
--
-- >>> global TPtr "printf"
-- OGlobal TPtr "printf"
{-# INLINE global #-}
global :: IRType -> Name -> IROperand
global = OGlobal

-- | Create a constant operand from an 'IRConstant'.
--
-- This is a convenience wrapper for the 'OConstant' constructor.
--
-- ==== __Parameters__
--
-- * 'IRConstant' - The constant value
--
-- >>> constant (CInt 32 42)
-- OConstant (CInt 32 42)
{-# INLINE constant #-}
constant :: IRConstant -> IROperand
constant = OConstant

-- * Aggregate Constants

-- | Create a structure constant from a list of field constants.
--
-- ==== __Parameters__
--
-- * ['IRConstant'] - The list of field values
--
-- >>> struct [CInt 32 1, CInt 32 2]
-- OConstant (CStruct [CInt 32 1, CInt 32 2])
{-# INLINE struct #-}
struct :: [IRConstant] -> IROperand
struct = OConstant . CStruct

-- | Create an array constant with the specified element type.
--
-- ==== __Parameters__
--
-- * 'IRType' - The element type of the array
-- * ['IRConstant'] - The list of element values
--
-- >>> array (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3]
-- OConstant (CArray (TInt 32) [CInt 32 1, CInt 32 2, CInt 32 3])
{-# INLINE array #-}
array :: IRType -> [IRConstant] -> IROperand
array t = OConstant . CArray t
