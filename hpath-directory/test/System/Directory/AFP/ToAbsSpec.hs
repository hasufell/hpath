{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.ToAbsSpec where


import Test.Hspec
import System.Directory.OsPath
import System.OsPath (encodeFS)



spec :: Spec
spec = describe "System.Posix.PosixFilePath.Directory.toAbs" $ do

    -- successes --
    it "toAbs returns absolute paths unchanged" $ do
      p1 <- encodeFS "/a/b/c/d"
      to <- toAbs p1
      p1 `shouldBe` to

    it "toAbs returns even existing absolute paths unchanged" $ do
      p1 <- encodeFS "/home"
      to <- toAbs p1
      p1 `shouldBe` to


