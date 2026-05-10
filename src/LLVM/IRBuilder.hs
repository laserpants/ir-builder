{-# LANGUAGE DeriveFunctor #-}
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
  beginBlock,
  emitInstruction,
  emitAnnotation,
  emitTerminator,
) where

import Common (Name)
import Control.Monad.State (MonadState, State, execState, get, gets, modify, put)
import Control.Monad.Trans.Free (FreeT, MonadFree, iterT)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..), appendBlockBuilderItem, setBlockBuilderTerminator)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv, mapBuilderEnvCurrentBlock)
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

emitTerminator :: IRTerminator -> State IRBuilderEnv ()
emitTerminator term = do
  block <- gets builderEnvCurrentBlock
  case block of
    Just BlockBuilder{blockBuilderTerminator}
      | isJust blockBuilderTerminator ->
          error "Block already terminated"
    _ ->
      pure ()

  modify $
    mapBuilderEnvCurrentBlock $
      setBlockBuilderTerminator term

emitAnnotation :: IRAnnotation -> State IRBuilderEnv ()
emitAnnotation ann =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockAnnotation ann))

execIRBuilder :: IRBuilder a -> State IRBuilderEnv a
execIRBuilder = iterT emit . unpackIRBuilder

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

beginBlock :: Name -> State IRBuilderEnv ()
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
             in pure $ Map.insert blockBuilderLabel finalBlock builderEnvBlocks

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
