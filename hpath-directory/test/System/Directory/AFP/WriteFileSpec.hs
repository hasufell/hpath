{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.WriteFileSpec where


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
  setTmpDir "WriteFileSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createRegularFile' "fileWithContent"
  createRegularFile' "fileWithoutContent"
  createSymlink' "inputFileSymL" "fileWithContent" False
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
  describe "System.Posix.PosixFilePath.Directory.writeFile" $ do

    -- successes --
    it "writeFile file with content, everything clear" $ do
      writeFile' "fileWithContent" "blahfaselllll"
      out <- readFile' "fileWithContent"
      out `shouldBe` "blahfaselllll"

    it "writeFile file with content, everything clear" $ do
      writeFile' "fileWithContent" "gagagaga"
      out <- readFile' "fileWithContent"
      out `shouldBe` "gagagaga"

    it "writeFile file with content, everything clear" $ do
      writeFile' "fileWithContent" ""
      out <- readFile' "fileWithContent"
      out `shouldBe` ""

    it "writeFile file without content, everything clear" $ do
      writeFile' "fileWithoutContent" "blahfaselllll"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "blahfaselllll"

    it "writeFile, everything clear" $ do
      writeFile' "fileWithoutContent" "gagagaga"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "gagagaga"

    it "writeFile symlink, everything clear" $ do
      writeFile' "inputFileSymL" "blahfaselllll"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "blahfaselllll"

    it "writeFile symlink, everything clear" $ do
      writeFile' "inputFileSymL" "gagagaga"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "gagagaga"


    -- posix failures --
    it "writeFile to dir, inappropriate type" $ do
      writeFile' "alreadyExistsD" ""
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "writeFile, no permissions to file" $ do
      writeFile' "noPerms" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "writeFile, no permissions to file" $ do
      writeFile' "noPermsD/inputFile" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "writeFile, file does not exist" $ do
      writeFile' "gaga" ""
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)
