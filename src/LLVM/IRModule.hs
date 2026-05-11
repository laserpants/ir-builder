{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRModule (
  IRLinkage (..),
  IRModule (..),
  IRDecl (..),
  IRGlobal (..),
  IRAttribute (..),
  IRFunction (..),
  IRBlockItem (..),
  IRBlock (..),
  verifyModule,
)
where

import Common (Name)
import Data.ByteString (ByteString)
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IROperand (IRConstant, IRTerminator (..))
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
  deriving (Show, Eq, Ord)

data IRDecl = IRDecl
  { declName :: Name
  , declType :: IRType
  }
  deriving (Show, Eq, Ord)

data IRGlobal
  = IRString IRLinkage Name ByteString
  | IRConstant IRLinkage Name IRType IRConstant
  | IRExtern Name IRType [IRType]
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

data IRBlockItem
  = BlockInstr IRInstruction
  | BlockAnnotation IRAnnotation
  deriving (Show, Eq, Ord)

data IRBlock = IRBlock
  { blockLabel :: Name
  , blockItems :: [IRBlockItem]
  , blockTerminator :: IRTerminator
  }
  deriving (Show, Eq, Ord)

verifyModule :: IRModule -> Either String ()
verifyModule IRModule{moduleFunctions} = mapM_ verifyFunction moduleFunctions

verifyFunction :: IRFunction -> Either String ()
verifyFunction f = do
  verifyNoDuplicateBlockNames f
  verifyNoDuplicateSSANames f
  verifyBranchTargetsExist f

verifyNoDuplicateBlockNames :: IRFunction -> Either String ()
verifyNoDuplicateBlockNames IRFunction{functionName, functionBlocks} =
  if null duplicates
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has duplicate block names: "
          ++ show (map Text.unpack duplicates)
 where
  labels = map blockLabel functionBlocks
  duplicates = labels \\ nub labels

verifyNoDuplicateSSANames :: IRFunction -> Either String ()
verifyNoDuplicateSSANames IRFunction{functionName, functionBlocks} =
  if null duplicates
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has duplicate SSA names: "
          ++ show (map Text.unpack duplicates)
 where
  ssaNames = concatMap extractSSANamesFromBlock functionBlocks
  duplicates = ssaNames \\ nub ssaNames

extractSSANamesFromBlock :: IRBlock -> [Name]
extractSSANamesFromBlock IRBlock{blockItems} =
  concatMap extractSSANamesFromItem blockItems

extractSSANamesFromItem :: IRBlockItem -> [Name]
extractSSANamesFromItem =
  \case
    BlockInstr _ -> []
    BlockAnnotation _ -> []

verifyBranchTargetsExist :: IRFunction -> Either String ()
verifyBranchTargetsExist IRFunction{functionName, functionBlocks} =
  if null invalidTargets
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has invalid branch targets: "
          ++ show (map Text.unpack invalidTargets)
 where
  validLabels = Set.fromList (map blockLabel functionBlocks)
  invalidTargets = concatMap (findInvalidBranchTargets validLabels) functionBlocks

findInvalidBranchTargets :: Set Name -> IRBlock -> [Name]
findInvalidBranchTargets validLabels IRBlock{blockTerminator} =
  case blockTerminator of
    IBr target ->
      [target | target `Set.notMember` validLabels]
    ICondBr _ t1 t2 ->
      [target | target <- [t1, t2], target `Set.notMember` validLabels]
    ISwitch _ default_ cases ->
      let targetLabels = default_ : map snd cases
       in [target | target <- targetLabels, target `Set.notMember` validLabels]
    _ ->
      []
