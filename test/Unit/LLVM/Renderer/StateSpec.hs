{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Renderer.StateSpec (spec) where

import LLVM.IRRenderer.State
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRRenderer.State" $ do
  describe "emptyIRRendererState" $
    it "can be constructed" $
      emptyIRRendererState `shouldBe` IRRendererState

  describe "IRRendererState" $ do
    it "supports equality" $
      IRRendererState `shouldBe` IRRendererState

    it "supports Show" $
      show IRRendererState `shouldBe` "IRRendererState"
