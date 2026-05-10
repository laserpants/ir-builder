{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilderF (..),
  IRBuilder (..),
  IRBuilderEnv (..),
  compileModule,
  setTerminator,
  beginBlock,
  emitInstr,
  emitAnn,
  emitInstruction,
  emitAnnotation,
  emitTerminator,
) where

import Common (Name)
import Control.Monad.Free (liftF)
import Control.Monad.State (MonadState, State, execState, get, gets, modify, put)
import Control.Monad.Trans.Free (FreeT, MonadFree, iterT)
import Data.Function ((&))
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
  clearBuilderEnvCurrentBlock,
  emptyIRBuilderEnv,
  mapBuilderEnvCurrentBlock,
  mapBuilderEnvCurrentFunction,
 )
import LLVM.IRBuilder.FunctionBuilder (appendFunctionBuilderBlock)
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
    ( Functor
    , Applicative
    , Monad
    , MonadState IRBuilderEnv
    , MonadFree IRBuilderF
    )

execIRBuilder :: IRBuilder a -> State IRBuilderEnv a
execIRBuilder = iterT emit . unpackIRBuilder

emit :: IRBuilderF (State IRBuilderEnv a) -> State IRBuilderEnv a
emit =
  \case
    EmitInstr instr next -> do
      emitInstruction instr
      next
    EmitAnnotation ann next -> do
      emitAnnotation ann
      next

emitInstruction :: IRInstruction -> State IRBuilderEnv ()
emitInstruction instr =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockInstr instr))

emitAnnotation :: IRAnnotation -> State IRBuilderEnv ()
emitAnnotation ann =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockAnnotation ann))

setTerminator :: IRTerminator -> IRBuilder ()
setTerminator term = modify $ mapBuilderEnvCurrentBlock (setBlockBuilderTerminator term)

emitInstr :: IRInstruction -> IRBuilder ()
emitInstr instr = liftF (EmitInstr instr ())

emitAnn :: IRAnnotation -> IRBuilder ()
emitAnn ann = liftF (EmitAnnotation ann ())

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
finalizeModule name env@IRBuilderEnv{..} =
  IRModule
    { moduleName = name
    , moduleDecls = reverse builderEnvDecls
    , moduleGlobals = reverse builderEnvGlobals
    , moduleFunctions = finalizeFunctions env
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = undefined

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
 where
  env = execState (execIRBuilder builder) emptyIRBuilderEnv

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runIRRenderer . renderModule . buildModule name

beginFunction :: Name -> IRBuilder ()
beginFunction = undefined

endFunction :: IRBuilder ()
endFunction = undefined

beginBlock :: Name -> IRBuilder ()
beginBlock label = do
  IRBuilderEnv{..} <- get

  finalizedBlocks <-
    case builderEnvCurrentBlock of
      Nothing ->
        pure builderEnvBlocks
      Just BlockBuilder{..} ->
        case blockBuilderTerminator of
          Nothing ->
            error "Cannot finalize block without terminator"
          Just term ->
            let finalBlock =
                  IRBlock
                    { blockLabel = blockBuilderLabel
                    , blockItems = reverse blockBuilderItems
                    , blockTerminator = term
                    }
             in pure (builderEnvBlocks <> [finalBlock])

  let newBlock =
        BlockBuilder
          { blockBuilderLabel = label
          , blockBuilderItems = []
          , blockBuilderTerminator = Nothing
          }

  put $
    IRBuilderEnv
      { builderEnvBlocks = finalizedBlocks
      , builderEnvCurrentBlock = Just newBlock
      , ..
      }

finalizeCurrentBlock :: IRBuilderEnv -> IRBuilderEnv
finalizeCurrentBlock env@IRBuilderEnv{..} =
  case builderEnvCurrentBlock of
    Nothing ->
      env
    Just BlockBuilder{blockBuilderLabel, blockBuilderItems, blockBuilderTerminator} ->
      case blockBuilderTerminator of
        Nothing ->
          error "Cannot finalize block without terminator"
        Just term -> do
          let block =
                IRBlock
                  { blockLabel = blockBuilderLabel
                  , blockItems = reverse blockBuilderItems
                  , blockTerminator = term
                  }

          IRBuilderEnv
            { builderEnvCurrentBlock = Nothing
            , builderEnvCurrentFunction = fmap (appendFunctionBuilderBlock block) builderEnvCurrentFunction
            , ..
            }
