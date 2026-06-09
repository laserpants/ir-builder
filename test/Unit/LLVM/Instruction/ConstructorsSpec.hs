{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Instruction.ConstructorsSpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import LLVM.IRBuilder (IRBuilder, IRBuilderT (..), runIRBuilder)
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..))
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv)
import LLVM.IRBuilder.Error (IRBuilderError)
import LLVM.IRInstruction (IRFCmpCond (..), IRICmpCond (..), IRInstrOp (..), IRInstruction (..), IRTailMarker (..))
import LLVM.IRInstruction.Constructors
import LLVM.IRModule (IRBlockItem (..))
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Prelude hiding (and, or)

runBuilder :: IRBuilder a -> IRBuilderEnv -> Either IRBuilderError (a, IRBuilderEnv)
runBuilder b env = runIdentity (runExceptT (runStateT (runIRBuilder b) env))

-- | Run a builder action that emits instructions and return the last-emitted operand result
-- plus the collected block items via interpretting EmitInstr effects directly
runInstrBuilder :: IRBuilder IROperand -> (IROperand, [IRBlockItem])
runInstrBuilder action = (result, blockBuilderItems bb)
  where
    initialEnv =
      emptyIRBuilderEnv
        { builderEnvCurrentBlock =
            Just
              BlockBuilder
                { blockBuilderLabel = "entry",
                  blockBuilderItems = [],
                  blockBuilderTerminator = Nothing
                }
        }
    (result, finalEnv) = case runBuilder action initialEnv of
      Right (r, e) -> (r, e)
      Left err -> error $ show err
    bb = case builderEnvCurrentBlock finalEnv of
      Just b -> b
      Nothing -> error "no current block"

-- | Extract the instrOp from the last block item
lastInstrOp :: [IRBlockItem] -> Maybe IRInstrOp
lastInstrOp [] = Nothing
lastInstrOp items =
  case last items of
    BlockInstr i -> Just (instrOp i)
    _ -> Nothing

a32, b32 :: IROperand
a32 = OLocal (TInt 32) "a"
b32 = OLocal (TInt 32) "b"

af, bf :: IROperand
af = OLocal TFloat "af"
bf = OLocal TFloat "bf"

spec :: Spec
spec = describe "LLVM.IRInstruction.Constructors" $ do
  describe "Arithmetic" $ do
    it "add emits IAdd" $ do
      let (_, items) = runInstrBuilder (add (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IAdd (TInt 32) a32 b32)

    it "sub emits ISub" $ do
      let (_, items) = runInstrBuilder (sub (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (ISub (TInt 32) a32 b32)

    it "mul emits IMul" $ do
      let (_, items) = runInstrBuilder (mul (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IMul (TInt 32) a32 b32)

    it "sdiv emits ISDiv" $ do
      let (_, items) = runInstrBuilder (sdiv (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (ISDiv (TInt 32) a32 b32)

    it "udiv emits IUDiv" $ do
      let (_, items) = runInstrBuilder (udiv (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IUDiv (TInt 32) a32 b32)

    it "srem emits ISRem" $ do
      let (_, items) = runInstrBuilder (srem (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (ISRem (TInt 32) a32 b32)

    it "urem emits IURem" $ do
      let (_, items) = runInstrBuilder (urem (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IURem (TInt 32) a32 b32)

  describe "Bitwise" $ do
    it "and emits IAnd" $ do
      let (_, items) = runInstrBuilder (and (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IAnd (TInt 32) a32 b32)

    it "or emits IOr" $ do
      let (_, items) = runInstrBuilder (or (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IOr (TInt 32) a32 b32)

    it "xor emits IXOr" $ do
      let (_, items) = runInstrBuilder (xor (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IXOr (TInt 32) a32 b32)

    it "shl emits IShl" $ do
      let (_, items) = runInstrBuilder (shl (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IShl (TInt 32) a32 b32)

    it "lshr emits ILShr" $ do
      let (_, items) = runInstrBuilder (lshr (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (ILShr (TInt 32) a32 b32)

    it "ashr emits IAShr" $ do
      let (_, items) = runInstrBuilder (ashr (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IAShr (TInt 32) a32 b32)

  describe "Floating-Point" $ do
    it "fadd emits IFAdd" $ do
      let (_, items) = runInstrBuilder (fadd TFloat af bf)
      lastInstrOp items `shouldBe` Just (IFAdd TFloat af bf)

    it "fsub emits IFSub" $ do
      let (_, items) = runInstrBuilder (fsub TFloat af bf)
      lastInstrOp items `shouldBe` Just (IFSub TFloat af bf)

    it "fmul emits IFMul" $ do
      let (_, items) = runInstrBuilder (fmul TFloat af bf)
      lastInstrOp items `shouldBe` Just (IFMul TFloat af bf)

    it "fdiv emits IFDiv" $ do
      let (_, items) = runInstrBuilder (fdiv TFloat af bf)
      lastInstrOp items `shouldBe` Just (IFDiv TFloat af bf)

    it "fneg emits IFNeg" $ do
      let (_, items) = runInstrBuilder (fneg TFloat af)
      lastInstrOp items `shouldBe` Just (IFNeg TFloat af)

  describe "Comparison" $ do
    it "icmp emits IICmp and returns i1" $ do
      let (result, items) = runInstrBuilder (icmp ICmpEq (TInt 32) a32 b32)
      lastInstrOp items `shouldBe` Just (IICmp ICmpEq (TInt 32) a32 b32)
      case result of
        OLocal (TInt 1) _ -> pure ()
        _ -> expectationFailure "expected OLocal (TInt 1)"

    it "fcmp emits IFCmp and returns i1" $ do
      let (result, items) = runInstrBuilder (fcmp FCmpOEq TFloat af bf)
      lastInstrOp items `shouldBe` Just (IFCmp FCmpOEq TFloat af bf)
      case result of
        OLocal (TInt 1) _ -> pure ()
        _ -> expectationFailure "expected OLocal (TInt 1)"

  describe "Memory" $ do
    it "alloca emits IAlloca and returns ptr" $ do
      let n = OConstant (CInt 32 1)
          (result, items) = runInstrBuilder (alloca (TInt 32) n)
      lastInstrOp items `shouldBe` Just (IAlloca (TInt 32) n)
      case result of
        OLocal TPtr _ -> pure ()
        _ -> expectationFailure "expected OLocal TPtr"

    it "load emits ILoad" $ do
      let ptr = OGlobal TPtr "g"
          (_, items) = runInstrBuilder (load (TInt 32) ptr)
      lastInstrOp items `shouldBe` Just (ILoad (TInt 32) ptr)

    it "store emits IStore with no result" $ do
      let ptr = OGlobal TPtr "g"
          initialEnv =
            emptyIRBuilderEnv
              { builderEnvCurrentBlock =
                  Just BlockBuilder {blockBuilderLabel = "entry", blockBuilderItems = [], blockBuilderTerminator = Nothing}
              }
          (_, finalEnv) = case runBuilder (store a32 ptr) initialEnv of
            Right (_, e) -> ((), e)
            Left err -> error $ show err
          items = maybe [] blockBuilderItems (builderEnvCurrentBlock finalEnv)
      lastInstrOp items `shouldBe` Just (IStore a32 ptr)

    it "gep emits IGep and returns ptr" $ do
      let base = OGlobal TPtr "arr"
          idx0 = OConstant (CInt 32 0)
          idx1 = OConstant (CInt 32 1)
          (result, items) = runInstrBuilder (gep (TInt 32) base idx0 idx1)
      lastInstrOp items `shouldBe` Just (IGep (TInt 32) base idx0 idx1)
      case result of
        OLocal TPtr _ -> pure ()
        _ -> expectationFailure "expected OLocal TPtr"

  describe "Casts" $ do
    it "bitcast emits IBitcast" $ do
      let (_, items) = runInstrBuilder (bitcast a32 TFloat)
      lastInstrOp items `shouldBe` Just (IBitcast a32 TFloat)

    it "sext emits ISext" $ do
      let (_, items) = runInstrBuilder (sext (OLocal (TInt 8) "x") (TInt 32))
      lastInstrOp items `shouldBe` Just (ISext (OLocal (TInt 8) "x") (TInt 32))

    it "zext emits IZext" $ do
      let (_, items) = runInstrBuilder (zext (OLocal (TInt 8) "x") (TInt 32))
      lastInstrOp items `shouldBe` Just (IZext (OLocal (TInt 8) "x") (TInt 32))

    it "trunc emits ITrunc" $ do
      let (_, items) = runInstrBuilder (trunc a32 (TInt 8))
      lastInstrOp items `shouldBe` Just (ITrunc a32 (TInt 8))

    it "inttoptr emits IInttoptr" $ do
      let (_, items) = runInstrBuilder (inttoptr a32 TPtr)
      lastInstrOp items `shouldBe` Just (IInttoptr a32 TPtr)

    it "ptrtoint emits IPtrtoint" $ do
      let ptrOp = OLocal TPtr "p"
          (_, items) = runInstrBuilder (ptrtoint ptrOp (TInt 64))
      lastInstrOp items `shouldBe` Just (IPtrtoint ptrOp (TInt 64))

  describe "Control Flow" $ do
    it "call emits ICall and returns result" $ do
      let fn = OGlobal (TInt 32) "myfunc"
          (result, items) = runInstrBuilder (call NoTail (TInt 32) fn [a32])
      lastInstrOp items `shouldBe` Just (ICall NoTail (TInt 32) fn [a32])
      case result of
        OLocal (TInt 32) _ -> pure ()
        _ -> expectationFailure "expected OLocal (TInt 32)"

  describe "Miscellaneous" $ do
    it "phi emits IPhi" $ do
      let incoming = [(a32, "block1"), (b32, "block2")]
          (_, items) = runInstrBuilder (phi (TInt 32) incoming)
      lastInstrOp items `shouldBe` Just (IPhi (TInt 32) incoming)

    it "select emits ISelect" $ do
      let cond = OLocal (TInt 1) "c"
          (_, items) = runInstrBuilder (select (TInt 32) cond a32 b32)
      lastInstrOp items `shouldBe` Just (ISelect (TInt 32) cond a32 b32)
