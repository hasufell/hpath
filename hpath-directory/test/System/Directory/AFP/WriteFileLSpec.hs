{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.WriteFileLSpec where


import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "WriteFileLSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createRegularFile' "fileWithContent"
  createRegularFile' "fileWithoutContent"
  createSymlink' "inputFileSymL" "fileWithContent"
  createDir' "alreadyExistsD"
  createRegularFile' "noPerms"
  noPerms "noPerms"
  createDir' "noPermsD"
  createRegularFile' "noPermsD/inputFile"
  noPerms "noPermsD"
  writeFile' "fileWithContent" "BLKASL"


cleanupFiles :: IO ()
cleanupFiles = do
  deleteFile' "fileWithContent"
  deleteFile' "fileWithoutContent"
  deleteFile' "inputFileSymL"
  deleteDir' "alreadyExistsD"
  normalFilePerms "noPerms"
  deleteFile' "noPerms"
  normalDirPerms "noPermsD"
  deleteFile' "noPermsD/inputFile"
  deleteDir' "noPermsD"


spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.WriteFileL" $ do

    -- successes --
    it "WriteFileL file with content, everything clear" $ do
      writeFileL' "fileWithContent" "blahfaselllll"
      out <- readFile' "fileWithContent"
      out `shouldBe` "blahfaselllll"

    it "WriteFileL file with content, everything clear" $ do
      writeFileL' "fileWithContent" "gagagaga"
      out <- readFile' "fileWithContent"
      out `shouldBe` "gagagaga"

    it "WriteFileL file with content, everything clear" $ do
      writeFileL' "fileWithContent" ""
      out <- readFile' "fileWithContent"
      out `shouldBe` ""

    it "WriteFileL file without content, everything clear" $ do
      writeFileL' "fileWithoutContent" "blahfaselllll"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "blahfaselllll"

    it "WriteFileL, everything clear" $ do
      writeFileL' "fileWithoutContent" "gagagaga"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "gagagaga"

    it "WriteFileL symlink, everything clear" $ do
      writeFileL' "inputFileSymL" "blahfaselllll"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "blahfaselllll"

    it "WriteFileL symlink, everything clear" $ do
      writeFileL' "inputFileSymL" "gagagaga"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "gagagaga"


    -- posix failures --
    it "WriteFileL to dir, inappropriate type" $ do
      writeFileL' "alreadyExistsD" ""
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "WriteFileL, no permissions to file" $ do
      writeFileL' "noPerms" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "WriteFileL, no permissions to file" $ do
      writeFileL' "noPermsD/inputFile" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "WriteFileL, file does not exist" $ do
      writeFileL' "gaga" ""
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)
