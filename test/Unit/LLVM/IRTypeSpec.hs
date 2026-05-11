{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRTypeSpec where

import Fixtures.TestData
import LLVM.IRType
import Test.Hspec

spec :: Spec
spec = describe "LLVM.IRType" $ do
  describe "TInt" $ do
    it "creates TInt variants" $ do
      typeI32 `shouldBe` TInt 32
      typeI64 `shouldBe` TInt 64

  describe "TFloat" $ do
    it "creates TFloat type" $ typeFloat `shouldBe` TFloat

  describe "TDouble" $ do
    it "creates TDouble type" $ typeDouble `shouldBe` TDouble

  describe "TVoid" $ do
    it "creates TVoid type" $ typeVoid `shouldBe` TVoid

  describe "TPtr" $ do
    it "creates pointer types" $ do
      typePtr `shouldBe` TPtr (TInt 32)
      TPtr TFloat `shouldBe` TPtr TFloat

  describe "TArray" $ do
    it "creates array types" $ do
      typeArray `shouldBe` TArray 10 (TInt 32)

  describe "TStruct" $ do
    it "creates struct types" $ do
      typeStruct `shouldBe` TStruct [TInt 32, TFloat]

  describe "TVector" $ do
    it "creates vector types" $ do
      typeVector `shouldBe` TVector 4 (TInt 32)

  describe "TNamed" $ do
    it "creates named types" $ do
      TNamed "MyType" `shouldBe` TNamed "MyType"

  describe "TOpaque" $ do
    it "creates opaque types" $ do
      TOpaque "OpaqueType" `shouldBe` TOpaque "OpaqueType"

  describe "TFun" $ do
    it "creates function types" $ do
      TFun (TInt 32) [TInt 32, TFloat] `shouldBe` TFun (TInt 32) [TInt 32, TFloat]

  describe "Equality and Ordering" $ do
    it "distinguishes different type integers" $ do
      typeI32 `shouldNotBe` typeI64

    it "respects Eq and Ord instances" $ do
      typeI32 `shouldBe` TInt 32
      (TInt 32 == TInt 32) `shouldBe` True
      (TInt 32 == TInt 64) `shouldBe` False
