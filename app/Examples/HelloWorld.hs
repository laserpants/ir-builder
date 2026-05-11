{-# LANGUAGE OverloadedStrings #-}

module Examples.HelloWorld (helloWorld) where

import LLVM.IRBuilder
import LLVM.IRInstruction (IRTailMarker (..))
import LLVM.IRInstruction.Constructors (callVoid, gep)
import LLVM.IRModule (IRGlobal (..), IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRTerminator.Constructors (ret)
import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors (i32, i8, ptr)

helloWorld :: IRBuilder ()
helloWorld = do
  -- declare i32 @puts(ptr)
  emitGlobal (IRExtern "puts" i32 [ptr])

  -- @.str = private constant [14 x i8] c"Hello, World!\00"
  emitGlobal (IRString LPrivate ".str" "Hello, World!\0")

  -- define i32 @main()
  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"

    -- %1 = getelementptr [14 x i8], ptr @.str, i32 0, i32 0
    r1 <-
      gep
        (TArray 14 i8)
        (OGlobal TPtr ".str")
        (OConstant (CInt 32 0))
        (OConstant (CInt 32 0))

    -- call i32 @puts(%1)
    callVoid NoTail i32 (OGlobal i32 "puts") [r1]

    -- ret i32 0
    ret (OConstant (CInt 32 0))

-- main :: IO ()
-- main = Data.Text.IO.putStrLn (compileModule "hello_world" helloWorld)
