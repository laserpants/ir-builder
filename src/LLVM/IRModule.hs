module LLVM.IRModule (
  IRModule (..),
  IRDecl (..),
  IRGlobal (..),
  IRAttribute (..),
  IRFunction (..),
  IRBlock (..),
) where

import Common (Name)
import Data.ByteString (ByteString)
import Data.Text (Text)
import LLVM.IRInstruction (IRInstruction)
import LLVM.IROperand (IRConstant, IRTerminator)
import LLVM.IRType (IRType (..))

data IRModule = IRModule
  { modName :: Name
  , modDecls :: [IRDecl]
  , modGlobals :: [IRGlobal]
  , modFunctions :: [IRFunction]
  }

data IRDecl = IRDecl
  { declName :: Name
  , declType :: IRType
  }
  deriving (Show, Eq, Ord)

data IRGlobal
  = IRString Name ByteString
  | IRConstant Name IRType IRConstant
  | IRExtern Name IRType
  deriving (Show, Eq, Ord)

data IRAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline
  | NoInline
  | TailCall
  | MustTailCall
  | Cold
  | Hot
  | InlineHint
  | NoAlias
  | GC Text
  deriving (Show, Eq, Ord)

data IRFunction = IRFunction
  { fnName :: Name
  , fnRetType :: IRType
  , fnArgs :: [(IRType, Name)]
  , fnBlocks :: [IRBlock]
  , fnAttributes :: [IRAttribute]
  }

data IRBlock = IRBlock
  { blockLabel :: Name
  , blockInstructions :: [IRInstruction]
  , blockTerminator :: IRTerminator
  }
