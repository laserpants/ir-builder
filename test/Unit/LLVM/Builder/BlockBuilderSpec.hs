{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.BlockBuilderSpec (spec) where

import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.BlockBuilder
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
import LLVM.IRModule (IRBlockItem (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRType (..))
import Test.Hspec

emptyBlock :: BlockBuilder
emptyBlock =
  BlockBuilder
    { blockBuilderLabel = "entry"
    , blockBuilderItems = []
    , blockBuilderTerminator = Nothing
    }

testInstr :: IRInstruction
testInstr =
  IRInstruction
    { instrResult = Just ("r", TInt 32)
    , instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b")
    }

spec :: Spec
spec = describe "LLVM.IRBuilder.BlockBuilder" $ do
  describe "BlockBuilder construction" $ do
    it "creates a block builder with label" $
      blockBuilderLabel emptyBlock `shouldBe` "entry"

    it "initializes with empty items" $
      blockBuilderItems emptyBlock `shouldBe` []

    it "initializes with no terminator" $
      blockBuilderTerminator emptyBlock `shouldBe` Nothing

  describe "appendBlockBuilderItem" $ do
    it "appends an instruction item" $ do
      let b = appendBlockBuilderItem (BlockInstr testInstr) emptyBlock
      length (blockBuilderItems b) `shouldBe` 1

    it "appends in order" $ do
      let ann1 = BlockAnnotation (Comment "first")
          ann2 = BlockAnnotation (Comment "second")
          b = appendBlockBuilderItem ann2 (appendBlockBuilderItem ann1 emptyBlock)
      blockBuilderItems b `shouldBe` [ann1, ann2]

    it "preserves existing items" $ do
      let b1 = appendBlockBuilderItem (BlockInstr testInstr) emptyBlock
          b2 = appendBlockBuilderItem (BlockAnnotation (Comment "note")) b1
      length (blockBuilderItems b2) `shouldBe` 2

    it "does not affect the terminator" $ do
      let b = appendBlockBuilderItem (BlockInstr testInstr) emptyBlock
      blockBuilderTerminator b `shouldBe` Nothing

  describe "setBlockBuilderTerminator" $ do
    it "sets the terminator" $ do
      let b = setBlockBuilderTerminator (IBr "exit") emptyBlock
      blockBuilderTerminator b `shouldBe` Just (IBr "exit")

    it "overwrites a previous terminator" $ do
      let b =
            setBlockBuilderTerminator
              (IBr "other")
              (setBlockBuilderTerminator (IBr "first") emptyBlock)
      blockBuilderTerminator b `shouldBe` Just (IBr "other")

    it "does not affect existing items" $ do
      let b1 = appendBlockBuilderItem (BlockInstr testInstr) emptyBlock
          b2 = setBlockBuilderTerminator (IRet (OConstant (CInt 32 0))) b1
      length (blockBuilderItems b2) `shouldBe` 1
