{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IROperandSpec where

import Fixtures.TestData
import LLVM.IROperand
  ( IRConstant (CInt),
    IROperand (OConstant, OGlobal, OLocal),
    IRTerminator (IBr, ICondBr, IRet, ISwitch, IUnreachable),
    opComponents,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IROperand" $ do
  describe "OLocal" $ do
    it "creates local operands" $ do
      operandLocal32 `shouldBe` OLocal typeI32 "x"

  describe "OGlobal" $ do
    it "creates global operands" $ do
      operandGlobal32 `shouldBe` OGlobal typeI32 "global"

  describe "OConstant" $ do
    it "creates constant operands" $ do
      operandConstInt `shouldBe` OConstant (CInt 32 42)

  describe "opComponents" $ do
    it "extracts name and type from OLocal" $ do
      opComponents operandLocal32 `shouldBe` Just ("x", typeI32)

    it "extracts name and type from OGlobal" $ do
      opComponents operandGlobal32 `shouldBe` Just ("global", typeI32)

    it "returns Nothing for OConstant" $ do
      opComponents operandConstInt `shouldBe` Nothing

  describe "IRTerminator" $ do
    it "creates IRet terminator" $ do
      termRet `shouldBe` IRet operandLocal32

    it "creates IBr terminator" $ do
      termBr `shouldBe` IBr "entry"

    it "creates ICondBr terminator" $ do
      termCondBr `shouldBe` ICondBr operandLocal32 "entry" "exit"

    it "creates ISwitch terminator" $ do
      let sw = ISwitch operandLocal32 "default" []
      sw `shouldBe` ISwitch operandLocal32 "default" []

    it "creates IUnreachable terminator" $ do
      IUnreachable `shouldBe` IUnreachable
