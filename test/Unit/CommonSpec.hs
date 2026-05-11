{-# LANGUAGE OverloadedStrings #-}

module Unit.CommonSpec where

import Common (Name)
import Test.Hspec

spec :: Spec
spec = describe "Common.Name" $ do
  it "Name is a Text type alias" $ do
    let n = "test_name" :: Name
    show n `shouldBe` "\"test_name\""
