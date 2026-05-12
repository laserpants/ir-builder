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
import LLVM.IRBuilder (IRBuilder, (<##>))

{- | Create a single-line comment annotation.

Usage: @emitAnnotation (comment "This is a comment")@

Renders as: @; This is a comment@
-}
comment :: Text -> IRAnnotation
comment = Comment

{- | Create a multi-line comment block annotation.
j
Usage: @emitAnnotation (commentBlock ["Line 1", "Line 2", "Line 3"])@

Renders as:
> ; Line 1
> ; Line 2
> ; Line 3
-}
commentBlock :: [Text] -> IRAnnotation
commentBlock = CommentBlock

{- | Alternative syntax for inline comments using function composition.

Usage: @reg <- withComment "comment explaining this" $ add i32 a b@

Renders as: @%reg = add i32 %a, %b  ; comment explaining this@
-}
withComment :: Text -> IRBuilder a -> IRBuilder a
withComment cmt m = m <##> cmt
