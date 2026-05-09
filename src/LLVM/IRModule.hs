{-# LANGUAGE StrictData #-}

module LLVM.IRModule (
  IRModule (..),
  IRDecl (..),
  IRGlobal (..),
  IRAttribute (..),
  IRFunction (..),
  IRBlock (..),
  appendInstr,
) where

import Common (Name)
import Data.ByteString (ByteString)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IROperand (IRConstant, IRTerminator)
import LLVM.IRType (IRType (..))

data IRLinkage
  = LExternal
  | LInternal
  | LPrivate
  deriving (Show, Eq, Ord)

data IRModule = IRModule
  { moduleName :: Name
  , moduleDecls :: [IRDecl]
  , moduleGlobals :: [IRGlobal]
  , moduleFunctions :: [IRFunction]
  }

data IRDecl = IRDecl
  { declName :: Name
  , declType :: IRType
  }
  deriving (Show, Eq, Ord)

data IRGlobal
  = IRString IRLinkage Name ByteString
  | IRConstant IRLinkage Name IRType IRConstant
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
  { functionName :: Name
  , functionLinkage :: IRLinkage
  , functionRetType :: IRType
  , functionArgs :: [(IRType, Name)]
  , functionBlocks :: [IRBlock]
  , functionAttributes :: [IRAttribute]
  }
  deriving (Show, Eq, Ord)

data IRBlockArtifact
  = BlockInstr IRInstruction
  | BlockAnnotation IRAnnotation
  deriving (Show, Eq, Ord)

data IRBlock = IRBlock
  { blockLabel :: Name
  , blockInstructions :: [IRBlockArtifact]
  , blockTerminator :: IRTerminator
  }
  deriving (Show, Eq, Ord)

appendInstr :: IRFunction -> IRFunction
appendInstr = undefined
