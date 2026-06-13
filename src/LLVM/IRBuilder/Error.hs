{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.Error (
  IRBuilderError (..),
  displayError,
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRType (IRName)

{- | Errors that can occur during IR building.

This type encodes all possible failure modes in the IRBuilder monad.
Using an algebraic type allows callers to handle specific errors gracefully.
-}
data IRBuilderError
  = -- | No instruction exists to attach a comment to (used by <##> operator)
    NoInstructionForComment
  | -- | Cannot attach comment to annotation; comments only attach to instructions
    CommentOnAnnotation IRAnnotation
  | -- | Attempted to set terminator when block already has one
    BlockAlreadyTerminated IRName
  | -- | Attempted operation requiring an active block when none exists.
    -- Thrown by 'getCurrentBlockM' and the '<##>' operator.
    -- Note: the emit functions ('emitInstruction', 'emitAnnotation', 'emitTerminator')
    -- auto-create an implicit @\"entry\"@ block instead of throwing this error.
    NoCurrentBlock
  | -- | Attempted to begin a function when one is already active
    CurrentFunctionActive IRName
  | -- | Attempted to end function when no function is active
    NoCurrentFunction
  | -- | Cannot finalize a block without a terminator
    BlockMissingTerminator IRName
  deriving (Show, Eq)

-- | Display an error in a human-friendly format suitable for end-user output.
displayError :: IRBuilderError -> Text
displayError NoInstructionForComment =
  Text.pack "Cannot attach comment: no instruction was just emitted. "
    <> Text.pack "The <##> operator can only follow instructions that produce a register (add, load, etc.), "
    <> Text.pack "not annotations or terminators."
displayError (CommentOnAnnotation _) =
  Text.pack "Cannot attach comment to annotation. "
    <> Text.pack "Comments via <##> can only be attached to instructions, not to annotation blocks."
displayError (BlockAlreadyTerminated blockName) =
  Text.pack "Block '"
    <> blockName
    <> Text.pack "' already has a terminator. "
    <> Text.pack "Each block can only have one terminator instruction (ret, br, etc.)."
displayError NoCurrentBlock =
  Text.pack "No current block is active. "
    <> Text.pack "You must call beginBlock before emitting instructions or setting a terminator."
displayError (CurrentFunctionActive funcName) =
  Text.pack "Function '"
    <> funcName
    <> Text.pack "' is already active. "
    <> Text.pack "Call endFunction before starting a new function definition."
displayError NoCurrentFunction =
  Text.pack "No current function is active. "
    <> Text.pack "You must call beginFunction (via define) before calling endFunction."
displayError (BlockMissingTerminator blockName) =
  Text.pack "Cannot finalize block '"
    <> blockName
    <> Text.pack "': it lacks a terminator. "
    <> Text.pack "All blocks must end with ret, br, condbr, switch, or unreachable."
