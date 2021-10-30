{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.ReadFileSpec where


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
  setTmpDir "ReadFileSpec"
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
  writeFile' "fileWithContent" "Blahfaselgagaga"


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
  describe "System.Posix.PosixFilePath.Directory.readFile" $ do

    -- successes --
    it "readFile file with content, everything clear" $ do
      out <- readFileL "fileWithContent"
      out `shouldBe` "Blahfaselgagaga"

    it "readFile symlink, everything clear" $ do
      out <- readFileL "inputFileSymL"
      out `shouldBe` "Blahfaselgagaga"

    it "readFile empty file, everything clear" $ do
      out <- readFileL "fileWithoutContent"
      out `shouldBe` ""


    -- posix failures --
    it "readFile directory, wrong file type" $ do
      readFileL "alreadyExistsD"
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "readFile file, no permissions" $ do
      readFileL "noPerms"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFile file, no permissions on dir" $ do
      readFileL "noPermsD/inputFile"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFile file, no such file" $ do
      readFileL "lalala"
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)


    -- successes --
    it "readFile (Strict) file with content, everything clear" $ do
      out <- readFile' "fileWithContent"
      out `shouldBe` "Blahfaselgagaga"

    it "readFile (Strict) symlink, everything clear" $ do
      out <- readFile' "inputFileSymL"
      out `shouldBe` "Blahfaselgagaga"

    it "readFile (Strict) empty file, everything clear" $ do
      out <- readFile' "fileWithoutContent"
      out `shouldBe` ""


    -- posix failures --
    it "readFile (Strict) directory, wrong file type" $ do
      readFile' "alreadyExistsD"
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "readFile (Strict) file, no permissions" $ do
      readFile' "noPerms"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFile (Strict) file, no permissions on dir" $ do
      readFile' "noPermsD/inputFile"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFile (Strict) file, no such file" $ do
      readFile' "lalala"
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)

