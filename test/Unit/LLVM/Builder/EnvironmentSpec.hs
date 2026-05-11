{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.EnvironmentSpec (spec) where

import LLVM.IRBuilder.Environment
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRBuilder.Environment" $ do
  describe "emptyIRBuilderEnv" $ do
    it "initializes fresh counter to 0" $
      builderEnvFresh emptyIRBuilderEnv `shouldBe` 0

    it "initializes with no current block" $
      builderEnvCurrentBlock emptyIRBuilderEnv `shouldBe` Nothing

    it "initializes with no current function" $
      builderEnvCurrentFunction emptyIRBuilderEnv `shouldBe` Nothing

    it "initializes with empty blocks list" $
      builderEnvBlocks emptyIRBuilderEnv `shouldBe` []

    it "initializes with empty functions list" $
      builderEnvFunctions emptyIRBuilderEnv `shouldBe` []

    it "initializes with empty globals list" $
      builderEnvGlobals emptyIRBuilderEnv `shouldBe` []

    it "initializes with empty decls list" $
      builderEnvDecls emptyIRBuilderEnv `shouldBe` []

  describe "overBuilderEnvFresh" $ do
    it "applies a function to the fresh counter" $
      builderEnvFresh (overBuilderEnvFresh (+ 1) emptyIRBuilderEnv) `shouldBe` 1

    it "increments multiple times" $
      builderEnvFresh (overBuilderEnvFresh (+ 5) emptyIRBuilderEnv) `shouldBe` 5

    it "does not affect other fields" $ do
      let env = overBuilderEnvFresh (+ 1) emptyIRBuilderEnv
      builderEnvCurrentBlock env `shouldBe` Nothing
      builderEnvCurrentFunction env `shouldBe` Nothing

  describe "clearBuilderEnvCurrentBlock" $ do
    it "sets current block to Nothing" $
      builderEnvCurrentBlock (clearBuilderEnvCurrentBlock emptyIRBuilderEnv) `shouldBe` Nothing

    it "does not affect other fields" $ do
      let env = clearBuilderEnvCurrentBlock emptyIRBuilderEnv
      builderEnvFresh env `shouldBe` 0
      builderEnvCurrentFunction env `shouldBe` Nothing
