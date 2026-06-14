{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module provides a high-level monadic DSL for constructing LLVM
IR modules, functions, blocks, and instructions. The 'IRBuilder' monad
encapsulates the state of IR construction and provides error handling for
common failure modes.

Example usage:

@
module <- compileModule "my_module" $ do
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
  IRBuilderT (..),
  IRBuilder,
  MonadIRBuilder (..),
  runIRBuilder,
  IRBuilderEnv (..),
  lift,
  compileModule,
  compileModuleWith,
  buildModuleWith,
  buildModule,
  compileModuleM,
  compileModuleWithM,
  buildModuleWithM,
  buildModuleM,

  -- * Function definition
  define,
  beginFunction,
  endFunction,

  -- * Block management
  beginBlock,
  block,
  finalizeCurrentBlock,

  -- * Instruction emission
  emitInstruction,
  emitAnnotation,
  emitTerminator,
  emitGlobal,
  emitTypeDecl,
  declare,
  setTerminator,

  -- * Utilities
  (<##>),

  -- * Error handling
  getCurrentBlockM,
  getCurrentFunctionM,
  liftEither,
)
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState, StateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.BlockBuilder (
  BlockBuilder (..),
  appendBlockBuilderItem,
  setBlockBuilderTerminator,
 )
import LLVM.IRBuilder.Class (MonadIRBuilder (..))
import LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  appendBuilderEnvDecls,
  appendBuilderEnvGlobals,
  emptyIRBuilderEnv,
  mapBuilderEnvCurrentBlock,
  overBuilderEnvFreshLabel,
  setBuilderEnvCurrentBlock,
 )
import LLVM.IRBuilder.Error (IRBuilderError (..))
import LLVM.IRBuilder.FunctionBuilder (
  FunctionBuilder (..),
  appendFunctionBuilderBlock,
 )
import LLVM.IRBuilder.Supply (freshLabel)
import LLVM.IRInstruction (IRInstruction (..))
import LLVM.IRModule (
  IRAttribute (..),
  IRBlock (..),
  IRBlockItem (..),
  IRDecl (..),
  IRFunction (..),
  IRGlobal (..),
  IRLinkage (..),
  IRModule (..),
 )
import LLVM.IROperand (IRTerminator)
import LLVM.IRRenderer (renderModule, runIRRenderer)
import LLVM.IRType (IRName, IRType)

-- ============================================================================
-- Core types
-- ============================================================================

{- | The 'IRBuilderT' monad transformer for constructing LLVM IR.

This transformer allows IR construction operations to be embedded in any monad.
It combines:

- 'StateT' for maintaining the builder environment (current function, block, etc.)
- 'ExceptT' for error handling with typed 'IRBuilderError' exceptions
- A parameterized base monad @m@

Use @runIRBuilderT@ to extract the inner transformer stack for execution.
The monad supports 'MonadFix' when the base monad does, enabling forward references
with @mdo@ notation in recursive block structures like loops with phi nodes.
-}
newtype IRBuilderT m a = IRBuilderT
  { runIRBuilderT :: StateT IRBuilderEnv (ExceptT IRBuilderError m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRBuilderEnv
    , MonadError IRBuilderError
    )

{- | Specialized 'IRBuilderT' using 'Identity' as the base monad.

This is the original non-transformer version, maintained for backward compatibility.
Most existing code uses this type.
-}
type IRBuilder = IRBuilderT Identity

{- | Extract the transformer stack from an 'IRBuilder' computation.

For backward compatibility with code that uses 'runIRBuilder' directly.
-}
runIRBuilder :: IRBuilder a -> StateT IRBuilderEnv (ExceptT IRBuilderError Identity) a
runIRBuilder = runIRBuilderT

-- Manual instances for constrained typeclasses

instance (MonadFix m) => MonadFix (IRBuilderT m) where
  mfix f = IRBuilderT (mfix (runIRBuilderT . f))

instance MonadTrans IRBuilderT where
  lift = IRBuilderT . lift . lift

instance (MonadIO m) => MonadIO (IRBuilderT m) where
  liftIO = lift . liftIO

-- | IRBuilderT instance for MonadIRBuilder
instance (Monad m) => MonadIRBuilder (IRBuilderT m) where
  getIRBuilderEnv = get
  putIRBuilderEnv = put
  throwIRBuilderError = throwError

-- ============================================================================
-- Error handling helpers
-- ============================================================================

{- | Retrieve the current active block, throwing 'NoCurrentBlock' if none exists.

This is useful for explicit error handling patterns where you need the current
block or want to handle the error case directly instead of relying on other
operations to fail.

__Throws:__ 'NoCurrentBlock' if no block is currently active.
-}
getCurrentBlockM :: (MonadIRBuilder m) => m BlockBuilder
getCurrentBlockM = do
  maybeBlock <- getsIRBuilderEnv builderEnvCurrentBlock
  case maybeBlock of
    Just bb -> pure bb
    Nothing -> throwIRBuilderError NoCurrentBlock

-- | Get the current function, throwing 'NoCurrentFunction' if none exists
getCurrentFunctionM :: (MonadIRBuilder m) => m FunctionBuilder
getCurrentFunctionM = do
  maybeFunc <- getsIRBuilderEnv builderEnvCurrentFunction
  case maybeFunc of
    Just func -> pure func
    Nothing -> throwIRBuilderError NoCurrentFunction

{- | Lift an 'Either' computation into the 'IRBuilder' monad.

Useful for integrating external computations that return 'Either IRBuilderError a'
into the builder pipeline:

@
result <- liftEither (someExternalComputation ...)
@
-}
liftEither :: (MonadIRBuilder m) => Either IRBuilderError a -> m a
liftEither = either throwIRBuilderError pure

-- ============================================================================
-- Instruction emission
-- ============================================================================

{- | Internal: ensure a current block exists, creating an implicit 'entry' block
if none is active. This mirrors LLVM IR semantics where the first block's
label is implicit.
-}
ensureBlock :: (MonadIRBuilder m) => m ()
ensureBlock = do
  maybeBlock <- getsIRBuilderEnv builderEnvCurrentBlock
  case maybeBlock of
    Just _ -> pure ()
    Nothing -> beginBlock (pack "entry")

{- | Emit a terminator instruction for the current block.

Every basic block must end with exactly one terminator (e.g., 'ret', 'br', 'condbr').

If no block is currently active, an implicit block labelled @"entry"@ is created
automatically, mirroring LLVM IR semantics.

This function validates that the block doesn't already have a terminator.

__Throws:__ 'BlockAlreadyTerminated' if the block already has a terminator
-}
emitTerminator :: (MonadIRBuilder m) => IRTerminator -> m ()
emitTerminator term = do
  ensureBlock
  maybeBlock <- getsIRBuilderEnv builderEnvCurrentBlock
  case maybeBlock of
    Just BlockBuilder{blockBuilderTerminator, blockBuilderLabel}
      | isJust blockBuilderTerminator ->
          throwIRBuilderError (BlockAlreadyTerminated blockBuilderLabel)
    _ ->
      pure ()

  setTerminator term

{- | Set the terminator instruction for the current block.

This is a lower-level variant used internally. Prefer 'emitTerminator' for
proper error handling.
-}
setTerminator :: (MonadIRBuilder m) => IRTerminator -> m ()
setTerminator term = modifyIRBuilderEnv $ mapBuilderEnvCurrentBlock (setBlockBuilderTerminator term)

{- | Emit an instruction into the current block.

Instructions are appended to the current block's instruction list. Each
instruction may have an optional inline comment attached via '<##>'.

If no block is currently active, an implicit block labelled @"entry"@ is
created automatically.

__Example:__

@
add i32 (OConstant (CInt 32 1)) (OConstant (CInt 32 2)) <##> "compute sum"
@
-}
emitInstruction :: (MonadIRBuilder m) => IRInstruction (Maybe Text) -> m ()
emitInstruction instr = do
  ensureBlock
  modifyIRBuilderEnv $
    mapBuilderEnvCurrentBlock
      (appendBlockBuilderItem (BlockInstr instr))

{- | Emit a comment annotation into the current block.

Annotations are block-level comments useful for documenting logic sections.
Unlike inline comments (via '<##>'), annotations stand alone as 'IRBlockItem's.

If no block is currently active, an implicit block labelled @"entry"@ is
created automatically.

__Example:__

@
emitAnnotation (commentBlock ["Section: input validation", "Check bounds..."])
@
-}
emitAnnotation :: (MonadIRBuilder m) => IRAnnotation -> m ()
emitAnnotation ann = do
  ensureBlock
  modifyIRBuilderEnv $
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
(<##>) :: (MonadIRBuilder m) => m a -> Text -> m a
m <##> comment = m <* modifyLastInstructionComment comment

-- | Internal: modify the last emitted instruction to attach a comment
modifyLastInstructionComment :: (MonadIRBuilder m) => Text -> m ()
modifyLastInstructionComment comment = do
  maybeBlock <- getsIRBuilderEnv builderEnvCurrentBlock
  case maybeBlock of
    Nothing ->
      throwIRBuilderError NoCurrentBlock
    Just bb@BlockBuilder{blockBuilderItems} ->
      case blockBuilderItems of
        [] ->
          throwIRBuilderError NoInstructionForComment
        items ->
          let allButLast = init items
              lastItem = last items
           in case lastItem of
                BlockInstr instr -> do
                  let updatedInstr = instr{instrMetadata = Just comment}
                  modifyIRBuilderEnv (\env -> env{builderEnvCurrentBlock = Just (bb{blockBuilderItems = allButLast <> [BlockInstr updatedInstr]})})
                BlockAnnotation ann ->
                  throwIRBuilderError (CommentOnAnnotation ann)

-- Internal helpers (not exported)

finalizeModule :: IRName -> IRBuilderEnv -> IRModule
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

{- | Build an LLVM IR module in any 'MonadIRBuilder' context, with result.

This is the generalized version of 'buildModuleWith' that works with any monad
implementing 'MonadIRBuilder', such as custom monad stacks built on top of
'IRBuilderT'. It isolates the module construction in a fresh environment and
returns both the module and the computation result.

The function:

1. Saves the current builder environment
2. Resets to an empty environment
3. Executes the builder computation
4. Extracts the final environment to construct the module
5. Restores the original environment
6. Returns the module and computation result

__Args:__

- First argument: module name
- Second argument: builder computation in any 'MonadIRBuilder'

__Returns:__ Tuple of ('IRModule', computation result)

__Throws:__ Propagates any 'IRBuilderError' via 'throwIRBuilderError'

__Example:__

@
moduleAndResult <- buildModuleWithM "my_module" $ do
 define i32 "main" [] LExternal [] $ do
   beginBlock "entry"
   customMonadOperation  -- works with custom MonadIRBuilder instances
   ret (OConstant (CInt 32 0))
@
-}
buildModuleWithM :: (MonadIRBuilder m) => IRName -> m a -> m (IRModule, a)
buildModuleWithM name builder = do
  savedEnv <- getIRBuilderEnv
  putIRBuilderEnv emptyIRBuilderEnv
  result <- builder
  finalEnv <- getIRBuilderEnv
  putIRBuilderEnv savedEnv
  let module_ = finalizeModule name finalEnv
  pure (module_, result)

{- | Build an LLVM IR module in any 'MonadIRBuilder' context.

This is the generalized version of 'buildModule' that works with any monad
implementing 'MonadIRBuilder'. It's a convenience wrapper around 'buildModuleWithM'
that discards the computation result.

__Args:__

- First argument: module name
- Second argument: builder computation in any 'MonadIRBuilder'

__Returns:__ The constructed 'IRModule'

__Throws:__ Propagates any 'IRBuilderError' via 'throwIRBuilderError'

__Example:__

@
module_ <- buildModuleM "my_module" $ do
 define i32 "main" [] LExternal [] $ do
   beginBlock "entry"
   customMonadOperation
   ret (OConstant (CInt 32 0))
@
-}
buildModuleM :: (MonadIRBuilder m) => IRName -> m a -> m IRModule
buildModuleM name builder = fst <$> buildModuleWithM name builder

{- | Compile an LLVM IR module to text in any 'MonadIRBuilder' context, with result.

This is the generalized version of 'compileModuleWith' that works with any monad
implementing 'MonadIRBuilder'. It builds the module and renders it to LLVM assembly,
returning both the text and the computation result.

__Args:__

- First argument: module name
- Second argument: builder computation in any 'MonadIRBuilder'

__Returns:__ Tuple of (LLVM assembly 'Text', computation result)

__Throws:__ Propagates any 'IRBuilderError' via 'throwIRBuilderError'
-}
compileModuleWithM :: (MonadIRBuilder m) => IRName -> m a -> m (Text, a)
compileModuleWithM name builder = do
  (module_, result) <- buildModuleWithM name builder
  let text = runIRRenderer $ renderModule module_
  pure (text, result)

{- | Compile an LLVM IR module to text in any 'MonadIRBuilder' context.

This is the generalized version of 'compileModule' that works with any monad
implementing 'MonadIRBuilder'. It's a convenience wrapper around 'compileModuleWithM'
that discards the computation result.

__Args:__

- First argument: module name
- Second argument: builder computation in any 'MonadIRBuilder'

__Returns:__ LLVM assembly as 'Text'

__Throws:__ Propagates any 'IRBuilderError' via 'throwIRBuilderError'

__Example:__

@
text <- compileModuleM "my_module" $ do
 define i32 "main" [] LExternal [] $ do
   beginBlock "entry"
   customMonadOperation
   ret (OConstant (CInt 32 0))
@
-}
compileModuleM :: (MonadIRBuilder m) => IRName -> m a -> m Text
compileModuleM name builder = fst <$> compileModuleWithM name builder

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
buildModuleWith :: IRName -> IRBuilder a -> Either IRBuilderError (IRModule, a)
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
buildModule :: IRName -> IRBuilder a -> IRModule
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
compileModuleWith :: IRName -> IRBuilder a -> Either IRBuilderError Text
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
let code = compileModule "my_module" $ do
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
compileModule :: IRName -> IRBuilder a -> Text
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
beginFunction :: (MonadIRBuilder m) => FunctionBuilder -> m ()
beginFunction builder = do
  finalizeCurrentBlock

  IRBuilderEnv{..} <- getIRBuilderEnv

  case builderEnvCurrentFunction of
    Just (FunctionBuilder{..}) ->
      throwIRBuilderError (CurrentFunctionActive functionBuilderName)
    Nothing ->
      pure ()

  putIRBuilderEnv $
    IRBuilderEnv
      { builderEnvCurrentFunction = Just builder
      , builderEnvCurrentBlock = Nothing
      , builderEnvFreshReg = 0
      , builderEnvFreshLabel = 0
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
endFunction :: (MonadIRBuilder m) => m ()
endFunction = do
  finalizeCurrentBlock

  IRBuilderEnv{..} <- getIRBuilderEnv

  fun <-
    case builderEnvCurrentFunction of
      Nothing ->
        throwIRBuilderError NoCurrentFunction
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

  putIRBuilderEnv $
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
  (MonadIRBuilder m) =>
  -- | Return type of the function
  IRType ->
  -- | Function name
  IRName ->
  -- | Parameter list as @(type, name)@ pairs
  [(IRType, IRName)] ->
  -- | Linkage visibility (e.g. 'LExternal', 'LInternal')
  IRLinkage ->
  -- | Function attributes (e.g. @[NoInline, NoReturn]@)
  [IRAttribute] ->
  -- | Body computation that builds the function
  m a ->
  m a
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
emitGlobal :: (MonadIRBuilder m) => IRGlobal -> m ()
emitGlobal global = modifyIRBuilderEnv (appendBuilderEnvGlobals [global])

{- | Emit a named type declaration into the module.

Renders at the top of the IR output as:

@
%IRName = type <type>
@

Duplicate declarations (same name) are silently ignored, so it is safe
to call this function multiple times for the same type.

__Example:__

@
emitTypeDecl "Node" (TStruct [TInt 32, TPtr, TPtr])
-- renders: %Node = type { i32, ptr, ptr }
@
-}
emitTypeDecl :: (MonadIRBuilder m) => IRName -> IRType -> m ()
emitTypeDecl name ty = modifyIRBuilderEnv $ \env ->
  if any (\d -> declName d == name) (builderEnvDecls env)
    then env
    else appendBuilderEnvDecls [IRDecl name ty] env

{- | Emit an external function declaration into the module.

Renders as:

@
declare <retType> @<name>(<argTypes>)
@

Duplicate declarations (same function name) are silently ignored, so it is
safe to call this freely without tracking what has already been declared.

__Example:__

@
declare "printf" TVoid [TPtr]
declare "malloc" TPtr [TInt 64]
@
-}
declare :: (MonadIRBuilder m) => IRName -> IRType -> [IRType] -> m ()
declare name retTy argTys = modifyIRBuilderEnv $ \env ->
  let isDupe (IRExtern n _ _) = n == name
      isDupe _ = False
   in if any isDupe (builderEnvGlobals env)
        then env
        else appendBuilderEnvGlobals [IRExtern name retTy argTys] env

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
beginBlock :: (MonadIRBuilder m) => IRName -> m ()
beginBlock label = do
  finalizeCurrentBlock
  let newBlock =
        BlockBuilder
          { blockBuilderLabel = label
          , blockBuilderItems = []
          , blockBuilderTerminator = Nothing
          }
  modifyIRBuilderEnv (setBuilderEnvCurrentBlock newBlock)

{- | Begin a fresh basic block with a suffixed label.

This is the high-level alternative to 'beginBlock' for use in 'mdo' blocks
where the generated label must be captured and referenced by other
instructions (e.g., 'br', 'condbr', 'phi').

The hint is suffixed with a fresh integer to guarantee uniqueness across
nested or repeated uses of the same logical name:

@
-- "loop" becomes e.g. "loop.1", "body" becomes "body.2"
define i64 "fact" [(i64, "n")] LExternal [] $ mdo
beginBlock "entry"
br loopLabel

loopLabel <- block "loop"
...
condbr cond bodyLabel exitLabel

bodyLabel <- block "body"
...
br loopLabel

exitLabel <- block "exit"
ret result
@

__Returns:__ the generated block label (e.g., @"loop.1"@)

__Throws:__ 'BlockMissingTerminator' if the previous block lacks a terminator
-}
block :: (MonadIRBuilder m) => IRName -> m IRName
block hint = do
  label <- freshLabel hint
  beginBlock label
  pure label

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
finalizeCurrentBlock :: (MonadIRBuilder m) => m ()
finalizeCurrentBlock = do
  IRBuilderEnv{..} <- getIRBuilderEnv
  case builderEnvCurrentBlock of
    Nothing ->
      pure ()
    Just BlockBuilder{blockBuilderLabel, blockBuilderItems, blockBuilderTerminator} ->
      case blockBuilderTerminator of
        Nothing ->
          throwIRBuilderError (BlockMissingTerminator blockBuilderLabel)
        Just term -> do
          let irBlock =
                IRBlock
                  { blockLabel = blockBuilderLabel
                  , blockItems = blockBuilderItems
                  , blockTerminator = term
                  }

          putIRBuilderEnv $
            IRBuilderEnv
              { builderEnvCurrentBlock = Nothing
              , builderEnvCurrentFunction = fmap (appendFunctionBuilderBlock irBlock) builderEnvCurrentFunction
              , ..
              }
