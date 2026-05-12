{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- | This module provides a high-level monadic DSL for constructing LLVM
IR modules, functions, blocks, and instructions. The 'IRBuilder' monad
encapsulates the state of IR construction and provides error handling for
common failure modes.

Example usage:

@
module <- compileModule "myModule" $ do
  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    result <- add i32 (OConstant (CInt 32 1)) (OConstant (CInt 32 2))
    ret result
@

= Core types and compilation

Types and functions for the IR builder monad and module compilation.

= Function definition

Monadic operations for defining functions with return types, parameters, and attributes.

= Block management

Operations for managing basic blocks within functions.

= Instruction emission

Functions for emitting various types of IR instructions and annotations.

= Utilities

Helper functions and operators for common DSL patterns.

= Error handling

Error introspection and lifting functions for explicit error handling.
-}
module LLVM.IRBuilder (
  -- * Core types and compilation
  IRBuilder (..),
  IRBuilderEnv (..),
  compileModule,
  compileModuleWith,
  buildModuleWith,
  buildModule,

  -- * Function definition
  define,
  beginFunction,
  endFunction,

  -- * Block management
  beginBlock,
  finalizeCurrentBlock,

  -- * Instruction emission
  emitInstruction,
  emitAnnotation,
  emitTerminator,
  emitGlobal,
  setTerminator,

  -- * Utilities
  (<##>),

  -- * Error handling
  getCurrentBlockM,
  getCurrentFunctionM,
  liftEither,
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
import LLVM.IRBuilder.Error (IRBuilderError (..))
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

-- ============================================================================
-- Core types
-- ============================================================================

{- | The 'IRBuilder' monad for constructing LLVM IR.

This is the main monad used for all IR construction operations. It combines:

- 'StateT' for maintaining the builder environment (current function, block, etc.)
- 'ExceptT' for error handling with typed 'IRBuilderError' exceptions
- 'Identity' as the base monad

Use @runIRBuilder@ to extract the inner transformer stack for execution.
The monad supports 'MonadFix' via derived instances, enabling forward references
with @mdo@ notation in recursive block structures like loops with phi nodes.
-}
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

-- ============================================================================
-- Error handling helpers
-- ============================================================================

{- | Retrieve the current active block, throwing 'NoCurrentBlock' if none exists.

This is useful for explicit error handling patterns where you need the current
block or want to handle the error case directly instead of relying on other
operations to fail.

__Throws:__ 'NoCurrentBlock' if no block is currently active.
-}
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

{- | Lift an 'Either' computation into the 'IRBuilder' monad.

Useful for integrating external computations that return 'Either IRBuilderError a'
into the builder pipeline:

@
result <- liftEither (someExternalComputation ...)
@
-}
liftEither :: Either IRBuilderError a -> IRBuilder a
liftEither = either throwError pure

-- ============================================================================
-- Instruction emission
-- ============================================================================

{- | Emit a terminator instruction for the current block.

Every basic block must end with exactly one terminator (e.g., 'ret', 'br', 'condbr').

This function validates that:

1. A block is currently active
2. The block doesn't already have a terminator

__Throws:__

- 'NoCurrentBlock' if no block is active
- 'BlockAlreadyTerminated' if the block already has a terminator
-}
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

{- | Set the terminator instruction for the current block.

This is a lower-level variant used internally. Prefer 'emitTerminator' for
proper error handling.
-}
setTerminator :: IRTerminator -> IRBuilder ()
setTerminator term = modify $ mapBuilderEnvCurrentBlock (setBlockBuilderTerminator term)

{- | Emit an instruction into the current block.

Instructions are appended to the current block's instruction list. Each
instruction may have an optional inline comment attached via '<##>'.

__Example:__

@
add i32 (OConstant (CInt 32 1)) (OConstant (CInt 32 2)) <##> "compute sum"
@

__Throws:__ implicitly propagates 'NoCurrentBlock' if no block is active
-}
emitInstruction :: IRInstruction (Maybe Text) -> IRBuilder ()
emitInstruction instr =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockInstr instr))

{- | Emit a comment annotation into the current block.

Annotations are block-level comments useful for documenting logic sections.
Unlike inline comments (via '<##>'), annotations stand alone as 'IRBlockItem's.

__Example:__

@
emitAnnotation (commentBlock ["Section: input validation", "Check bounds..."])
@
-}
emitAnnotation :: IRAnnotation -> IRBuilder ()
emitAnnotation ann =
  modify $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockAnnotation ann))

{- | Attach an inline comment to the previously emitted instruction.

This operator must immediately follow an instruction-emitting expression.
The comment is attached to the instruction's metadata and renders as a
line comment in LLVM assembly.

__Usage:__

@
result <- add i32 a b <##> "sum of a and b"
@

This renders as:

@
%1 = add i32 %a, %b  ; sum of a and b
@

__Throws:__

- 'NoCurrentBlock' if no block is active
- 'NoInstructionForComment' if no instruction was just emitted
- 'CommentOnAnnotation' if applied to an annotation block instead of an instruction
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

-- Internal helpers (not exported)

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

-- ============================================================================
-- Module compilation
-- ============================================================================

{- | Build an LLVM IR module with explicit error handling.

This is the result-returning variant of 'buildModule'. It executes the builder
computation and returns the result as an 'Either', allowing callers to handle
errors explicitly rather than via @error@.

__Args:__

- First argument: module name
- Second argument: builder computation

__Returns:__ @'Either' 'IRBuilderError' ('IRModule', a)@ where:

  - Left: construction error
  - Right: tuple of (module, builder result)

__Errors:__ Returns 'Left' on any 'IRBuilderError' during construction
-}
buildModuleWith :: Name -> IRBuilder a -> Either IRBuilderError (IRModule, a)
buildModuleWith name builder = do
  let result = runExceptT (runStateT (runIRBuilder builder) emptyIRBuilderEnv)
  (a, env) <- runIdentity result
  let module_ = finalizeModule name env
  pure (module_, a)

{- | Build an LLVM IR module, terminating on error.

This is the primary entry point for IR module construction. It executes the
builder monad and returns the complete IR module. If any error occurs during
construction, the program terminates with 'error'.

For explicit error handling, use 'buildModuleWith'.

__Args:__

- First argument: module name
- Second argument: builder computation

__Returns:__ The constructed 'IRModule'

__Throws (via error):__ Any 'IRBuilderError' encountered during construction
-}
buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder =
  case buildModuleWith name builder of
    Left err -> error $ "IRBuilder failed: " ++ show err
    Right (m, _) -> m

{- | Compile an LLVM IR module to text with explicit error handling.

This is the result-returning variant of 'compileModule'. It executes the builder,
finalizes all blocks and functions, and renders the module to LLVM assembly text.

For automatic error handling (terminates on error), use 'compileModule'.

__Args:__

- First argument: module name
- Second argument: builder computation

__Returns:__ @'Either' 'IRBuilderError' 'Text'@ where:

  - Left: construction or rendering error
  - Right: LLVM assembly text

__Errors:__ Returns 'Left' on any 'IRBuilderError' during building
-}
compileModuleWith :: Name -> IRBuilder a -> Either IRBuilderError Text
compileModuleWith name builder = do
  (module_, _) <- buildModuleWith name builder
  pure $ runIRRenderer $ renderModule module_

{- | Compile an LLVM IR module to text, terminating on error.

This is the primary entry point for IR generation. It executes the builder
monad, finalizes all pending blocks and functions, and renders the complete
module to LLVM assembly text format.

On error, this function will call @error@ with a descriptive message.
For explicit error handling, use 'compileModuleWith'.

__Example:__

@
let code = compileModule "myModule" $ do
  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    x <- add i32 (OConstant (CInt 32 1)) (OConstant (CInt 32 2))
    ret x
putStrLn code
@

__Args:__

- First argument: module name
- Second argument: builder computation

__Returns:__ LLVM assembly as 'Text'

__Throws (via error):__ Any 'IRBuilderError' encountered during building
-}
compileModule :: Name -> IRBuilder a -> Text
compileModule name builder =
  case compileModuleWith name builder of
    Left err -> error $ "IRBuilder compilation failed: " ++ show err
    Right t -> t

-- ============================================================================
-- Function definition
-- ============================================================================

{- | Begin a new function definition in the current module.

This is a lower-level operation. For a higher-level interface, prefer 'define'.

This function:

1. Finalizes the current block (if any)
2. Verifies no function is already active
3. Activates the given function

__Throws:__ 'CurrentFunctionActive' if a function is already being defined
-}
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

{- | Finalize the current function definition.

This is a lower-level operation. For a higher-level interface, prefer 'define'.

This function:

1. Finalizes the current block (if any)
2. Constructs the function from current state
3. Adds it to the module's function list
4. Clears the current function context

__Throws:__ 'NoCurrentFunction' if no function is currently being defined
-}
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

{- | Define a function with the given signature and body.

This is the primary high-level interface for function definition. It combines
'beginFunction' and 'endFunction' around a computation.

__Args:__

- @retType@: the return type
- @name@: function name
- @args@: list of (type, name) parameter pairs
- @linkage@: function linkage (e.g., 'LExternal', 'LInternal')
- @attributes@: function attributes (e.g., @[APure]@)
- @body@: monadic computation that builds the function body

__Returns:__ the result of the body computation

__Example:__

@
define i32 "add" [(i32, "a"), (i32, "b")] LExternal [] $ do
  beginBlock "entry"
  result <- add i32 (OLocal i32 "a") (OLocal i32 "b")
  ret result
@

__Throws:__ Any error from body computation or finalization
-}
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

-- ============================================================================
-- Block management
-- ============================================================================

{- | Emit a global value (declaration or constant) into the module.

Global values include:

- External declarations (functions, global variables)
- Constant globals
- String constants

Globals are collected in the module and rendered at the top level.

__Example:__

@
emitGlobal (declare i32 "printf" [TPtr] ...)
emitGlobal (IRGlobalConstant "myString" (IRConstantString "hello") ...)
@
-}
emitGlobal :: IRGlobal -> IRBuilder ()
emitGlobal global = modify (appendBuilderEnvGlobals [global])

{- | Begin a new basic block within the current function.

Each block has a label and contains instructions ending with a terminator.

This function:

1. Finalizes the previous block (if any)
2. Creates a new empty block with the given label
3. Makes it the current block

Blocks are accumulated within the function and rendered in order.

__Args:__ block label (e.g., "entry", "loop", "exit")

__Throws:__ 'BlockMissingTerminator' if the previous block lacks a terminator

__Example:__

@
beginBlock "entry"
beginBlock "loop"
beginBlock "exit"
@
-}
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

{- | Finalize the current block and add it to the function's block list.

This is normally called automatically by 'beginBlock', 'beginFunction', and
'endFunction'. It's exported for advanced use cases.

This function:

1. Validates the current block has a terminator
2. Constructs an 'IRBlock' from current state
3. Adds it to the current function
4. Clears the current block context

__Throws:__ 'BlockMissingTerminator' if the block lacks a terminator
-}
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
