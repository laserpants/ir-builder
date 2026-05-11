{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Type.ConstructorsSpec (spec) where

import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors (i1, i32, i64, i8, ptr)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRType.Constructors" $ do
  describe "i1" $
    it "produces TInt 1" $
      i1 `shouldBe` TInt 1

  describe "i8" $
    it "produces TInt 8" $
      i8 `shouldBe` TInt 8

  describe "i32" $
    it "produces TInt 32" $
      i32 `shouldBe` TInt 32

  describe "i64" $
    it "produces TInt 64" $
      i64 `shouldBe` TInt 64

  describe "ptr" $ do
    it "produces TPtr" $
      ptr `shouldBe` TPtr

  describe "consistency" $ do
    it "all integer constructors produce distinct types" $ do
      let types = [i1, i8, i32, i64]
      length types `shouldBe` length (foldr (\t acc -> if t `elem` acc then acc else t : acc) [] types)
