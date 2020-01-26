{-# LANGUAGE OverloadedStrings #-}


module System.Posix.RawFilePath.Directory.ToAbsSpec where


import Test.Hspec
import "hpath-directory" System.Posix.RawFilePath.Directory



spec :: Spec
spec = describe "System.Posix.RawFilePath.Directory.toAbs" $ do

    -- successes --
    it "toAbs returns absolute paths unchanged" $ do
      let p1 = "/a/b/c/d"
      to <- toAbs p1
      p1 `shouldBe` to

    it "toAbs returns even existing absolute paths unchanged" $ do
      let p1 = "/home"
      to <- toAbs p1
      p1 `shouldBe` to


