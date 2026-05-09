module LLVM.IRAnnotation (IRAnnotation (..)) where

import Data.Text (Text)

data IRAnnotation
  = Comment Text
  | CommentBlock [Text]
  deriving (Show, Eq, Ord)
