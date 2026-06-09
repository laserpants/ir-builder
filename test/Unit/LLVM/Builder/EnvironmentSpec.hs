{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.EnvironmentSpec (spec) where

import LLVM.IRBuilder.Environment
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRBuilder.Environment" $ do
  describe "emptyIRBuilderEnv" $ do
    it "initializes fresh register counter to 0" $
      builderEnvFreshReg emptyIRBuilderEnv `shouldBe` 0

    it "initializes fresh label counter to 0" $
      builderEnvFreshLabel emptyIRBuilderEnv `shouldBe` 0

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

  describe "overBuilderEnvFreshReg" $ do
    it "applies a function to the fresh register counter" $
      builderEnvFreshReg (overBuilderEnvFreshReg (+ 1) emptyIRBuilderEnv) `shouldBe` 1

    it "increments multiple times" $
      builderEnvFreshReg (overBuilderEnvFreshReg (+ 5) emptyIRBuilderEnv) `shouldBe` 5

    it "does not affect other fields" $ do
      let env = overBuilderEnvFreshReg (+ 1) emptyIRBuilderEnv
      builderEnvCurrentBlock env `shouldBe` Nothing
      builderEnvCurrentFunction env `shouldBe` Nothing

    it "does not affect the fresh label counter" $ do
      let env = overBuilderEnvFreshReg (+ 1) emptyIRBuilderEnv
      builderEnvFreshLabel env `shouldBe` 0

  describe "overBuilderEnvFreshLabel" $ do
    it "applies a function to the fresh label counter" $
      builderEnvFreshLabel (overBuilderEnvFreshLabel (+ 1) emptyIRBuilderEnv) `shouldBe` 1

    it "increments multiple times" $
      builderEnvFreshLabel (overBuilderEnvFreshLabel (+ 5) emptyIRBuilderEnv) `shouldBe` 5

    it "does not affect other fields" $ do
      let env = overBuilderEnvFreshLabel (+ 1) emptyIRBuilderEnv
      builderEnvCurrentBlock env `shouldBe` Nothing
      builderEnvCurrentFunction env `shouldBe` Nothing

    it "does not affect the fresh register counter" $ do
      let env = overBuilderEnvFreshLabel (+ 1) emptyIRBuilderEnv
      builderEnvFreshReg env `shouldBe` 0

  describe "clearBuilderEnvCurrentBlock" $ do
    it "sets current block to Nothing" $
      builderEnvCurrentBlock (clearBuilderEnvCurrentBlock emptyIRBuilderEnv) `shouldBe` Nothing

    it "does not affect other fields" $ do
      let env = clearBuilderEnvCurrentBlock emptyIRBuilderEnv
      builderEnvFreshReg env `shouldBe` 0
      builderEnvCurrentFunction env `shouldBe` Nothing
