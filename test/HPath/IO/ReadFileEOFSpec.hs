{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.ReadFileEOFSpec where


import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import System.Process
import Utils



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "ReadFileEOFSpec"
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
  describe "HPath.IO.readFileEOF" $ do

    -- successes --
    it "readFileEOF (Strict) file with content, everything clear" $ do
      out <- readFileEOF' "fileWithContent"
      out `shouldBe` "Blahfaselgagaga"

    it "readFileEOF (Strict) symlink, everything clear" $ do
      out <- readFileEOF' "inputFileSymL"
      out `shouldBe` "Blahfaselgagaga"

    it "readFileEOF (Strict) empty file, everything clear" $ do
      out <- readFileEOF' "fileWithoutContent"
      out `shouldBe` ""


    -- posix failures --
    it "readFileEOF (Strict) directory, wrong file type" $ do
      readFileEOF' "alreadyExistsD"
        `shouldThrow` (\e -> ioeGetErrorType e == InappropriateType)

    it "readFileEOF (Strict) file, no permissions" $ do
      readFileEOF' "noPerms"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFileEOF (Strict) file, no permissions on dir" $ do
      readFileEOF' "noPermsD/inputFile"
        `shouldThrow` (\e -> ioeGetErrorType e == PermissionDenied)

    it "readFileEOF (Strict) file, no such file" $ do
      readFileEOF' "lalala"
        `shouldThrow` (\e -> ioeGetErrorType e == NoSuchThing)
