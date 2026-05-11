{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Type.ConstructorsSpec (spec) where

import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors
import Test.Hspec

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
    it "wraps a type in TPtr" $
      ptr (TInt 32) `shouldBe` TPtr (TInt 32)

    it "works with nested pointer types" $
      ptr (ptr (TInt 8)) `shouldBe` TPtr (TPtr (TInt 8))

    it "works with float" $
      ptr TFloat `shouldBe` TPtr TFloat

  describe "i8Ptr" $
    it "produces TPtr (TInt 8)" $
      i8Ptr `shouldBe` TPtr (TInt 8)

  describe "consistency" $ do
    it "i8Ptr equals ptr i8" $
      i8Ptr `shouldBe` ptr i8

    it "all integer constructors produce distinct types" $ do
      let types = [i1, i8, i32, i64]
      length types `shouldBe` length (foldr (\t acc -> if t `elem` acc then acc else t : acc) [] types)
