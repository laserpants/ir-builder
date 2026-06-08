module LLVM.IROperand.Constructors (false, true) where

import LLVM.IROperand

false :: IROperand
false = OConstant $ CInt 1 0

true :: IROperand
true = OConstant $ CInt 1 1
