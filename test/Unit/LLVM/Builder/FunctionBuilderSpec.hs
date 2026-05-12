{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.FunctionBuilderSpec (spec) where

import Data.Text (Text)
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..), appendFunctionBuilderBlock)
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
import LLVM.IRModule (IRBlock (..), IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, it, shouldBe)

emptyFB :: FunctionBuilder
emptyFB =
  FunctionBuilder
    { functionBuilderName = "foo",
      functionBuilderLinkage = LExternal,
      functionBuilderRetType = TInt 32,
      functionBuilderArgs = [],
      functionBuilderBlocks = [],
      functionBuilderAttributes = []
    }

testBlock :: IRBlock
testBlock =
  IRBlock
    { blockLabel = "entry",
      blockItems = [],
      blockTerminator = IRet (OConstant (CInt 32 0))
    }

testBlock2 :: IRBlock
testBlock2 =
  IRBlock
    { blockLabel = "exit",
      blockItems = [],
      blockTerminator = IRet (OConstant (CInt 32 1))
    }

testInstr :: IRInstruction (Maybe Text)
testInstr =
  IRInstruction
    { instrResult = Just ("r", TInt 32),
      instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b"),
      instrMetadata = Nothing
    }

spec :: Spec
spec = describe "LLVM.IRBuilder.FunctionBuilder" $ do
  describe "FunctionBuilder construction" $ do
    it "stores the function name" $
      functionBuilderName emptyFB `shouldBe` "foo"

    it "stores the return type" $
      functionBuilderRetType emptyFB `shouldBe` TInt 32

    it "initializes with no blocks" $
      functionBuilderBlocks emptyFB `shouldBe` []

  describe "appendFunctionBuilderBlock" $ do
    it "appends a block" $ do
      let fb = appendFunctionBuilderBlock testBlock emptyFB
      length (functionBuilderBlocks fb) `shouldBe` 1

    it "appends blocks in order" $ do
      let fb =
            appendFunctionBuilderBlock
              testBlock2
              (appendFunctionBuilderBlock testBlock emptyFB)
      map blockLabel (functionBuilderBlocks fb) `shouldBe` ["entry", "exit"]

    it "does not affect other fields" $ do
      let fb = appendFunctionBuilderBlock testBlock emptyFB
      functionBuilderName fb `shouldBe` "foo"
      functionBuilderRetType fb `shouldBe` TInt 32
