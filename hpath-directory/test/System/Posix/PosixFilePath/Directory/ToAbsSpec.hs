{-# LANGUAGE OverloadedStrings #-}


module System.Posix.PosixFilePath.Directory.ToAbsSpec where


import Test.Hspec
import "hpath-directory" System.Posix.PosixFilePath.Directory



spec :: Spec
spec = describe "System.Posix.PosixFilePath.Directory.toAbs" $ do

    -- successes --
    it "toAbs returns absolute paths unchanged" $ do
      let p1 = "/a/b/c/d"
      to <- toAbs p1
      p1 `shouldBe` to

    it "toAbs returns even existing absolute paths unchanged" $ do
      let p1 = "/home"
      to <- toAbs p1
      p1 `shouldBe` to


