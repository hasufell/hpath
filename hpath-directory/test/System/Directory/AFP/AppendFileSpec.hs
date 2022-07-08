{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.AppendFileSpec where


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
  setTmpDir "AppendFileSpec"
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
  describe "System.Posix.PosixFilePath.Directory.appendFile" $ do

    -- successes --
    it "appendFile file with content, everything clear" $ do
      appendFile' "fileWithContent" "blahfaselllll"
      out <- readFile' "fileWithContent"
      out `shouldBe` "BLKASLblahfaselllll"

    it "appendFile file with content, everything clear" $ do
      appendFile' "fileWithContent" "gagagaga"
      out <- readFile' "fileWithContent"
      out `shouldBe` "BLKASLblahfaselllllgagagaga"

    it "appendFile file with content, everything clear" $ do
      appendFile' "fileWithContent" ""
      out <- readFile' "fileWithContent"
      out `shouldBe` "BLKASLblahfaselllllgagagaga"

    it "appendFile file without content, everything clear" $ do
      appendFile' "fileWithoutContent" "blahfaselllll"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "blahfaselllll"

    it "appendFile, everything clear" $ do
      appendFile' "fileWithoutContent" "gagagaga"
      out <- readFile' "fileWithoutContent"
      out `shouldBe` "blahfaselllllgagagaga"

    it "appendFile symlink, everything clear" $ do
      appendFile' "inputFileSymL" "blahfaselllll"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "BLKASLblahfaselllllgagagagablahfaselllll"

    it "appendFile symlink, everything clear" $ do
      appendFile' "inputFileSymL" "gagagaga"
      out <- readFile' "inputFileSymL"
      out `shouldBe` "BLKASLblahfaselllllgagagagablahfaselllllgagagaga"


    -- posix failures --
    it "appendFile to dir, inappropriate type" $ do
      appendFile' "alreadyExistsD" ""
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "appendFile, no permissions to file" $ do
      appendFile' "noPerms" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "appendFile, no permissions to file" $ do
      appendFile' "noPermsD/inputFile" ""
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "appendFile, file does not exist" $ do
      appendExistingFile' "gaga" ""
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)
