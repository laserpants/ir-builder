{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder
  ( IRBuilderF (..),
    IRBuilder (..),
    IRBuilderEnv (..),
    compileModule,
    setTerminator,
    beginBlock,
    finalizeCurrentBlock,
    beginFunction,
    endFunction,
    emitInstruction,
    emitAnnotation,
    appendInstruction,
    appendAnnotation,
    emitTerminator,
  )
where

import Common (Name)
import Control.Monad.Free (liftF)
import Control.Monad.State (MonadState, State, execState, get, gets, modify, put)
import Control.Monad.Trans.Free (FreeT, MonadFree, iterT)
import Data.Maybe (isJust)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..), appendBlockBuilderItem, setBlockBuilderTerminator)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv, mapBuilderEnvCurrentBlock)
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..), appendFunctionBuilderBlock)
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule (IRBlock (..), IRBlockItem (..), IRFunction (..), IRModule (..))
import LLVM.IROperand (IRTerminator)
import LLVM.IRRenderer (renderModule, runIRRenderer)

data IRBuilderF next
  = EmitInstr IRInstruction next
  | EmitAnnotation IRAnnotation next
  deriving (Functor)

newtype IRBuilder a = IRBuilder
  { unpackIRBuilder :: FreeT IRBuilderF (State IRBuilderEnv) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState IRBuilderEnv,
      MonadFree IRBuilderF
    )

execIRBuilder :: IRBuilder a -> State IRBuilderEnv a
execIRBuilder = iterT interpretBuilderF . unpackIRBuilder

interpretBuilderF :: IRBuilderF (State IRBuilderEnv a) -> State IRBuilderEnv a
interpretBuilderF =
  \case
    EmitInstr instr next -> do
      appendInstruction instr
      next
    EmitAnnotation ann next -> do
      appendAnnotation ann
      next

appendInstruction :: IRInstruction -> State IRBuilderEnv ()
appendInstruction instr =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockInstr instr))

appendAnnotation :: IRAnnotation -> State IRBuilderEnv ()
appendAnnotation ann =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockAnnotation ann))

setTerminator :: IRTerminator -> IRBuilder ()
setTerminator term = modify $ mapBuilderEnvCurrentBlock (setBlockBuilderTerminator term)

emitInstruction :: IRInstruction -> IRBuilder ()
emitInstruction instr = liftF (EmitInstr instr ())

emitAnnotation :: IRAnnotation -> IRBuilder ()
emitAnnotation ann = liftF (EmitAnnotation ann ())

emitTerminator :: IRTerminator -> IRBuilder ()
emitTerminator term = do
  block <- gets builderEnvCurrentBlock
  case block of
    Just BlockBuilder {blockBuilderTerminator}
      | isJust blockBuilderTerminator ->
          error "Block already terminated"
    Nothing ->
      error "No current block"
    _ ->
      pure ()

  setTerminator term

finalizeModule :: Name -> IRBuilderEnv -> IRModule
finalizeModule name env@IRBuilderEnv {builderEnvGlobals, builderEnvDecls} =
  IRModule
    { moduleName = name,
      moduleDecls = reverse builderEnvDecls,
      moduleGlobals = reverse builderEnvGlobals,
      moduleFunctions = finalizeFunctions env
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = builderEnvFunctions

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
  where
    env = execState (execIRBuilder builder) emptyIRBuilderEnv

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runIRRenderer . renderModule . buildModule name

beginFunction :: FunctionBuilder -> IRBuilder ()
beginFunction builder = do
  modify finalizeCurrentBlock

  IRBuilderEnv {..} <- get

  case builderEnvCurrentFunction of
    Just _ ->
      error "A current function is already active"
    Nothing ->
      pure ()

  put $
    IRBuilderEnv
      { builderEnvCurrentFunction = Just builder,
        builderEnvCurrentBlock = Nothing,
        ..
      }

endFunction :: IRBuilder ()
endFunction = do
  modify finalizeCurrentBlock

  IRBuilderEnv {..} <- get

  fun <-
    case builderEnvCurrentFunction of
      Nothing ->
        error "No current function"
      Just FunctionBuilder {..} ->
        pure $
          IRFunction
            { functionName = functionBuilderName,
              functionLinkage = functionBuilderLinkage,
              functionRetType = functionBuilderRetType,
              functionArgs = functionBuilderArgs,
              functionBlocks = functionBuilderBlocks,
              functionAttributes = functionBuilderAttributes
            }

  put $
    IRBuilderEnv
      { builderEnvCurrentFunction = Nothing,
        builderEnvCurrentBlock = Nothing,
        builderEnvFunctions = builderEnvFunctions <> [fun],
        ..
      }

beginBlock :: Name -> IRBuilder ()
beginBlock label = do
  IRBuilderEnv {..} <- get

  finalizedBlocks <-
    case builderEnvCurrentBlock of
      Nothing ->
        pure builderEnvBlocks
      Just BlockBuilder {..} ->
        case blockBuilderTerminator of
          Nothing ->
            error "Cannot finalize block without terminator"
          Just term ->
            let finalBlock =
                  IRBlock
                    { blockLabel = blockBuilderLabel,
                      blockItems = blockBuilderItems,
                      blockTerminator = term
                    }
             in pure (builderEnvBlocks <> [finalBlock])

  let newBlock =
        BlockBuilder
          { blockBuilderLabel = label,
            blockBuilderItems = [],
            blockBuilderTerminator = Nothing
          }

  put $
    IRBuilderEnv
      { builderEnvBlocks = finalizedBlocks,
        builderEnvCurrentBlock = Just newBlock,
        ..
      }

finalizeCurrentBlock :: IRBuilderEnv -> IRBuilderEnv
finalizeCurrentBlock IRBuilderEnv {..} =
  case builderEnvCurrentBlock of
    Nothing ->
      IRBuilderEnv {..}
    Just BlockBuilder {blockBuilderLabel, blockBuilderItems, blockBuilderTerminator} ->
      case blockBuilderTerminator of
        Nothing ->
          error "Cannot finalize block without terminator"
        Just term -> do
          let block =
                IRBlock
                  { blockLabel = blockBuilderLabel,
                    blockItems = blockBuilderItems,
                    blockTerminator = term
                  }

          IRBuilderEnv
            { builderEnvCurrentBlock = Nothing,
              builderEnvCurrentFunction = fmap (appendFunctionBuilderBlock block) builderEnvCurrentFunction,
              ..
            }
