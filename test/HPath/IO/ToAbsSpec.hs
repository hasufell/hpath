{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.ToAbsSpec where


import Test.Hspec
import HPath
import HPath.IO



spec :: Spec
spec = describe "HPath.IO.toAbs" $ do

    -- successes --
    it "toAbs returns absolute paths unchanged" $ do
      p1 <- parseAbs "/a/b/c/d"
      to <- toAbs p1
      p1 `shouldBe` to

    it "toAbs returns even existing absolute paths unchanged" $ do
      p1 <- parseAbs "/home"
      to <- toAbs p1
      p1 `shouldBe` to


