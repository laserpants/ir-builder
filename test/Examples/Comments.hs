module Examples.Comments (commentsModule) where

import LLVM.IR (IRBuilder)
import LLVM.IR.Examples (addNumbers)

commentsModule :: IRBuilder ()
commentsModule = addNumbers
