{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilder (..),
  IRBuilderEnv (..),
  compileModule,
  compileModuleWith,
  buildModuleWith,
  buildModule,
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
  getCurrentBlockM,
  getCurrentFunctionM,
  liftEither,
  (<##>),
)
where

import Common (Name)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState, StateT, get, gets, modify, put, runStateT)
import Data.Maybe (isJust)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.Error (IRBuilderError (..))
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
  setBuilderEnvCurrentBlock,
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
  { runIRBuilder :: StateT IRBuilderEnv (ExceptT IRBuilderError Identity) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRBuilderEnv
    , MonadError IRBuilderError
    , MonadFix
    )

-- | Get the current block, throwing NoCurrentBlock if none exists
getCurrentBlockM :: IRBuilder BlockBuilder
getCurrentBlockM = do
  maybeBlock <- gets builderEnvCurrentBlock
  case maybeBlock of
    Just block -> pure block
    Nothing -> throwError NoCurrentBlock

-- | Get the current function, throwing NoCurrentFunction if none exists
getCurrentFunctionM :: IRBuilder FunctionBuilder
getCurrentFunctionM = do
  maybeFunc <- gets builderEnvCurrentFunction
  case maybeFunc of
    Just func -> pure func
    Nothing -> throwError NoCurrentFunction

-- Private helper function (not exported, but useful for the error handling layer)
liftEither :: Either IRBuilderError a -> IRBuilder a
liftEither = either throwError pure

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
modifyLastInstructionComment comment = do
  block <- gets builderEnvCurrentBlock
  case block of
    Nothing ->
      throwError NoCurrentBlock
    Just bb@BlockBuilder{blockBuilderItems} ->
      case blockBuilderItems of
        [] ->
          throwError NoInstructionForComment
        items ->
          let allButLast = init items
              lastItem = last items
           in case lastItem of
                BlockInstr instr -> do
                  let updatedInstr = instr{instrMetadata = Just comment}
                  modify (\env -> env{builderEnvCurrentBlock = Just (bb{blockBuilderItems = allButLast <> [BlockInstr updatedInstr]})})
                BlockAnnotation ann ->
                  throwError (CommentOnAnnotation ann)

emitTerminator :: IRTerminator -> IRBuilder ()
emitTerminator term = do
  block <- gets builderEnvCurrentBlock
  case block of
    Just BlockBuilder{blockBuilderTerminator, blockBuilderLabel}
      | isJust blockBuilderTerminator ->
          throwError (BlockAlreadyTerminated blockBuilderLabel)
    Nothing ->
      throwError NoCurrentBlock
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

buildModuleWith :: Name -> IRBuilder a -> Either IRBuilderError (IRModule, a)
buildModuleWith name builder = do
  let result = runExceptT (runStateT (runIRBuilder builder) emptyIRBuilderEnv)
  (a, env) <- runIdentity result
  let module_ = finalizeModule name env
  pure (module_, a)

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder =
  case buildModuleWith name builder of
    Left err -> error $ "IRBuilder failed: " ++ show err
    Right (m, _) -> m

compileModuleWith :: Name -> IRBuilder a -> Either IRBuilderError Text
compileModuleWith name builder = do
  (module_, _) <- buildModuleWith name builder
  pure $ runIRRenderer $ renderModule module_

compileModule :: Name -> IRBuilder a -> Text
compileModule name builder =
  case compileModuleWith name builder of
    Left err -> error $ "IRBuilder compilation failed: " ++ show err
    Right t -> t

beginFunction :: FunctionBuilder -> IRBuilder ()
beginFunction builder = do
  finalizeCurrentBlock

  IRBuilderEnv{..} <- get

  case builderEnvCurrentFunction of
    Just (FunctionBuilder{..}) ->
      throwError (CurrentFunctionActive functionBuilderName)
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
  finalizeCurrentBlock

  IRBuilderEnv{..} <- get

  fun <-
    case builderEnvCurrentFunction of
      Nothing ->
        throwError NoCurrentFunction
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
  finalizeCurrentBlock
  let newBlock =
        BlockBuilder
          { blockBuilderLabel = label
          , blockBuilderItems = []
          , blockBuilderTerminator = Nothing
          }
  modify (setBuilderEnvCurrentBlock newBlock)

finalizeCurrentBlock :: IRBuilder ()
finalizeCurrentBlock = do
  IRBuilderEnv{..} <- get
  case builderEnvCurrentBlock of
    Nothing ->
      pure ()
    Just BlockBuilder{blockBuilderLabel, blockBuilderItems, blockBuilderTerminator} ->
      case blockBuilderTerminator of
        Nothing ->
          throwError (BlockMissingTerminator blockBuilderLabel)
        Just term -> do
          let block =
                IRBlock
                  { blockLabel = blockBuilderLabel
                  , blockItems = blockBuilderItems
                  , blockTerminator = term
                  }

          put $
            IRBuilderEnv
              { builderEnvCurrentBlock = Nothing
              , builderEnvCurrentFunction = fmap (appendFunctionBuilderBlock block) builderEnvCurrentFunction
              , ..
              }
