{-# LANGUAGE OverloadedStrings #-}

module Integration.ExamplesSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Examples.Comments (commentsModule)
import Examples.Factorial (factorialModule)
import Examples.HelloWorld (helloWorld)
import LLVM.IR (compileModule)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

contains :: Text -> Text -> Bool
contains = Text.isInfixOf

spec :: Spec
spec = describe "Examples" $ do
  describe "HelloWorld" $ do
    let ir = compileModule "hello_world" helloWorld
    it "declares puts" $
      ir `shouldSatisfy` contains "declare i32 @puts(ptr)"
    it "defines main" $
      ir `shouldSatisfy` contains "define i32 @main()"
    it "calls puts" $
      ir `shouldSatisfy` contains "call i32 @puts"
    it "returns i32 0" $
      ir `shouldSatisfy` contains "ret i32 0"

  describe "Factorial" $ do
    let ir = compileModule "factorial" factorialModule
    it "defines fact" $
      ir `shouldSatisfy` contains "define i64 @fact(i64 %n)"
    it "has a phi node" $
      ir `shouldSatisfy` contains "phi i64"
    it "uses icmp sgt" $
      ir `shouldSatisfy` contains "icmp sgt i64"
    it "defines main" $
      ir `shouldSatisfy` contains "define i32 @main()"

  describe "Comments" $ do
    let ir = compileModule "comments" commentsModule
    it "defines add_numbers" $
      ir `shouldSatisfy` contains "define i32 @add_numbers(i32 %a, i32 %b)"
    it "emits block comment" $
      ir `shouldSatisfy` contains "; Explanation:"
    it "emits add instruction" $
      ir `shouldSatisfy` contains "add i32"
    it "emits inline comment" $
      ir `shouldSatisfy` contains "; add the two input numbers"
