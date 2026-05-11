{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO
import LLVM.IRBuilder
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..))
import LLVM.IRInstruction (IRTailMarker (..))
import LLVM.IRInstruction.Constructors (callVoid, gep)
import LLVM.IRModule (IRGlobal (..), IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRTerminator.Constructors (ret)
import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors (i32, i8)

helloWorld :: IRBuilder ()
helloWorld = do
  -- declare i32 @puts(i8*)
  emitGlobal (IRExtern "puts" i32)

  -- @.str = private constant [14 x i8] c"Hello, World!\00"
  emitGlobal (IRString LPrivate ".str" "Hello, World!\0")

  -- define i32 @main()
  defineFunction
    FunctionBuilder
      { functionBuilderName = "main"
      , functionBuilderLinkage = LExternal
      , functionBuilderRetType = i32
      , functionBuilderArgs = []
      , functionBuilderBlocks = []
      , functionBuilderAttributes = []
      }
    $ do
      beginBlock "entry"

      -- %1 = getelementptr [14 x i8], ptr @.str, i32 0, i32 0
      strPtr <-
        gep
          (TArray 14 i8)
          (OGlobal TPtr ".str")
          (OConstant (CInt 32 0))
          (OConstant (CInt 32 0))

      -- call i32 @puts(%1)
      callVoid NoTail i32 (OGlobal i32 "puts") [strPtr]

      -- ret i32 0
      ret (OConstant (CInt 32 0))

main :: IO ()
main = Data.Text.IO.putStrLn (compileModule "hello_world" helloWorld)
