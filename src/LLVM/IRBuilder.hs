{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilder (..),
  IRBuilderEnv (..),
  compileModule,
  setTerminator,
  beginBlock,
  finalizeCurrentBlock,
  beginFunction,
  endFunction,
  define,
  emitInstruction,
  emitAnnotation,
  emitTerminator,
  emitGlobal,
  (<##>),
)
where

import Common (Name)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, State, execState, get, gets, modify, put)
import Data.Maybe (isJust)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.BlockBuilder (
  BlockBuilder (..),
  appendBlockBuilderItem,
  setBlockBuilderTerminator,
 )
import LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  appendBuilderEnvGlobals,
  emptyIRBuilderEnv,
  mapBuilderEnvCurrentBlock,
 )
import LLVM.IRBuilder.FunctionBuilder (
  FunctionBuilder (..),
  appendFunctionBuilderBlock,
 )
import LLVM.IRInstruction (IRInstruction (..))
import LLVM.IRModule (
  IRAttribute (..),
  IRBlock (..),
  IRBlockItem (..),
  IRFunction (..),
  IRGlobal,
  IRLinkage (..),
  IRModule (..),
 )
import LLVM.IROperand (IRTerminator)
import LLVM.IRRenderer (renderModule, runIRRenderer)
import LLVM.IRType (IRType)

newtype IRBuilder a = IRBuilder
  { runIRBuilder :: State IRBuilderEnv a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRBuilderEnv
    , MonadFix
    )

setTerminator :: IRTerminator -> IRBuilder ()
setTerminator term = modify $ mapBuilderEnvCurrentBlock (setBlockBuilderTerminator term)

emitInstruction :: IRInstruction (Maybe Text) -> IRBuilder ()
emitInstruction instr =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockInstr instr))

emitAnnotation :: IRAnnotation -> IRBuilder ()
emitAnnotation ann =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockAnnotation ann))

{- | Attach an inline comment to the previously emitted instruction.
Usage: @reg <- add ... <##> "comment"@
Renders as: @%reg = add ...  ; comment@
-}
(<##>) :: IRBuilder a -> Text -> IRBuilder a
m <##> comment = m <* modifyLastInstructionComment comment

-- | Internal: modify the last emitted instruction to attach a comment
modifyLastInstructionComment :: Text -> IRBuilder ()
modifyLastInstructionComment comment =
  modify $ mapBuilderEnvCurrentBlock updateLastItemComment
 where
  updateLastItemComment bb@BlockBuilder{blockBuilderItems} =
    case blockBuilderItems of
      [] ->
        error "No instruction to attach comment to"
      items ->
        let allButLast = init items
            lastItem = last items
         in case lastItem of
              BlockInstr instr ->
                let updatedInstr = instr{instrMetadata = Just comment}
                 in bb{blockBuilderItems = allButLast <> [BlockInstr updatedInstr]}
              BlockAnnotation _ ->
                error "Cannot attach comment to annotation; must attach to instruction"

emitTerminator :: IRTerminator -> IRBuilder ()
emitTerminator term = do
  block <- gets builderEnvCurrentBlock
  case block of
    Just BlockBuilder{blockBuilderTerminator}
      | isJust blockBuilderTerminator ->
          error "Block already terminated"
    Nothing ->
      error "No current block"
    _ ->
      pure ()

  setTerminator term

finalizeModule :: Name -> IRBuilderEnv -> IRModule
finalizeModule name env@IRBuilderEnv{builderEnvGlobals, builderEnvDecls} =
  IRModule
    { moduleName = name
    , moduleDecls = reverse builderEnvDecls
    , moduleGlobals = reverse builderEnvGlobals
    , moduleFunctions = finalizeFunctions env
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = builderEnvFunctions

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
 where
  env = execState (runIRBuilder builder) emptyIRBuilderEnv

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runIRRenderer . renderModule . buildModule name

beginFunction :: FunctionBuilder -> IRBuilder ()
beginFunction builder = do
  modify finalizeCurrentBlock

  IRBuilderEnv{..} <- get

  case builderEnvCurrentFunction of
    Just _ ->
      error "A current function is already active"
    Nothing ->
      pure ()

  put $
    IRBuilderEnv
      { builderEnvCurrentFunction = Just builder
      , builderEnvCurrentBlock = Nothing
      , ..
      }

endFunction :: IRBuilder ()
endFunction = do
  modify finalizeCurrentBlock

  IRBuilderEnv{..} <- get

  fun <-
    case builderEnvCurrentFunction of
      Nothing ->
        error "No current function"
      Just FunctionBuilder{..} ->
        pure $
          IRFunction
            { functionName = functionBuilderName
            , functionLinkage = functionBuilderLinkage
            , functionRetType = functionBuilderRetType
            , functionArgs = functionBuilderArgs
            , functionBlocks = functionBuilderBlocks
            , functionAttributes = functionBuilderAttributes
            }

  put $
    IRBuilderEnv
      { builderEnvCurrentFunction = Nothing
      , builderEnvCurrentBlock = Nothing
      , builderEnvFunctions = builderEnvFunctions <> [fun]
      , ..
      }

define ::
  IRType ->
  Name ->
  [(IRType, Name)] ->
  IRLinkage ->
  [IRAttribute] ->
  IRBuilder a ->
  IRBuilder a
define retType name args linkage attributes body = do
  beginFunction $
    FunctionBuilder
      { functionBuilderName = name
      , functionBuilderLinkage = linkage
      , functionBuilderRetType = retType
      , functionBuilderArgs = args
      , functionBuilderBlocks = []
      , functionBuilderAttributes = attributes
      }
  result <- body
  endFunction
  pure result

emitGlobal :: IRGlobal -> IRBuilder ()
emitGlobal global = modify (appendBuilderEnvGlobals [global])

beginBlock :: Name -> IRBuilder ()
beginBlock label = do
  modify finalizeCurrentBlock
  let newBlock =
        BlockBuilder
          { blockBuilderLabel = label
          , blockBuilderItems = []
          , blockBuilderTerminator = Nothing
          }
  modify $ \env -> env{builderEnvCurrentBlock = Just newBlock}

finalizeCurrentBlock :: IRBuilderEnv -> IRBuilderEnv
finalizeCurrentBlock IRBuilderEnv{..} =
  case builderEnvCurrentBlock of
    Nothing ->
      IRBuilderEnv{..}
    Just BlockBuilder{blockBuilderLabel, blockBuilderItems, blockBuilderTerminator} ->
      case blockBuilderTerminator of
        Nothing ->
          error "Cannot finalize block without terminator"
        Just term -> do
          let block =
                IRBlock
                  { blockLabel = blockBuilderLabel
                  , blockItems = blockBuilderItems
                  , blockTerminator = term
                  }

          IRBuilderEnv
            { builderEnvCurrentBlock = Nothing
            , builderEnvCurrentFunction = fmap (appendFunctionBuilderBlock block) builderEnvCurrentFunction
            , ..
            }
