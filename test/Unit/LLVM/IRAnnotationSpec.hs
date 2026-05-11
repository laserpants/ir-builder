{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRAnnotationSpec where

import LLVM.IRAnnotation (IRAnnotation (Comment, CommentBlock))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRAnnotation" $ do
  describe "Comment" $ do
    it "creates simple comments" $ do
      Comment "test comment" `shouldBe` Comment "test comment"

  describe "CommentBlock" $ do
    it "creates comment blocks" $ do
      let block = CommentBlock ["line1", "line2"]
      block `shouldBe` CommentBlock ["line1", "line2"]

  describe "Equality" $ do
    it "compares comments correctly" $ do
      (Comment "a" == Comment "a") `shouldBe` True
      (Comment "a" == Comment "b") `shouldBe` False

    it "compares comment blocks correctly" $ do
      let block1 = CommentBlock ["x"]
          block2 = CommentBlock ["x"]
      (block1 == block2) `shouldBe` True
