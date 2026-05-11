{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRInstructionSpec (spec) where

import Fixtures.TestData
import LLVM.IRInstruction
import LLVM.IROperand (IROperand (OLocal))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LLVM.IRInstruction" $ do
  describe "IRICmpCond" $ do
    it "creates all icmp conditions" $ do
      [ICmpEq, ICmpNe, ICmpUGt, ICmpUGe, ICmpULt, ICmpULe, ICmpSGt, ICmpSGe, ICmpSLt, ICmpSLe]
        `shouldBe` [ICmpEq, ICmpNe, ICmpUGt, ICmpUGe, ICmpULt, ICmpULe, ICmpSGt, ICmpSGe, ICmpSLt, ICmpSLe]

  describe "IRFCmpCond" $ do
    it "creates all fcmp conditions" $ do
      length
        [ FCmpOEq
        , FCmpOGt
        , FCmpOGe
        , FCmpOLt
        , FCmpOLe
        , FCmpONe
        , FCmpUeq
        , FCmpUGt
        , FCmpUGe
        , FCmpULt
        , FCmpULe
        , FCmpUNe
        , FCmpOrd
        , FCmpUno
        , FCmpTrue
        , FCmpFalse
        ]
        `shouldBe` 16

  describe "IRInstrOp" $ do
    it "creates arithmetic operations" $ do
      instrAdd `shouldBe` IAdd typeI32 operandLocal32 (OLocal typeI32 "y")
      instrSub `shouldBe` ISub typeI32 operandLocal32 (OLocal typeI32 "y")
      instrMul `shouldBe` IMul typeI32 operandLocal32 (OLocal typeI32 "y")

    it "creates memory operations" $ do
      instrLoad `shouldBe` ILoad typeI32 operandGlobal32
      instrStore `shouldBe` IStore operandLocal32 operandGlobal32

  describe "IRInstruction" $ do
    it "creates instructions with results" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("result", typeI32)
              , instrOp = instrAdd
              }
      instr
        `shouldBe` IRInstruction
          { instrResult = Just ("result", typeI32)
          , instrOp = instrAdd
          }

    it "creates instructions without results" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing
              , instrOp = instrStore
              }
      instr
        `shouldBe` IRInstruction
          { instrResult = Nothing
          , instrOp = instrStore
          }
