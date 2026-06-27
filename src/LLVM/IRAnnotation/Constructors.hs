{-# LANGUAGE OverloadedStrings #-}

module LLVM.IRAnnotation.Constructors (
  comment,
  commentBlock,
  withComment,
  (<##>),
)
where

import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder ((<##>))
import LLVM.IRBuilder.Class (MonadIRBuilder)

{- | Create a single-line comment annotation.

__Example:__

@emitAnnotation (comment "This is a comment")@

__Output:__

@; This is a comment@
-}
comment :: Text -> IRAnnotation
comment = Comment

{- | Create a multi-line comment block annotation.

__Example:__

@emitAnnotation (commentBlock ["Line 1", "Line 2", "Line 3"])@

__Output:__

> ; Line 1
> ; Line 2
> ; Line 3
-}
commentBlock :: [Text] -> IRAnnotation
commentBlock = CommentBlock

{- | Attach an inline comment to the result of a builder action.

__Example:__

@reg <- withComment "comment explaining this" $ add i32 a b@

__Output:__

@%reg = add i32 %a, %b  ; comment explaining this@
-}
withComment :: (MonadIRBuilder m) => Text -> m a -> m a
withComment cmt m = m <##> cmt
