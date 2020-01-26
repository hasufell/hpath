{-# LANGUAGE OverloadedStrings #-}

module System.Posix.RawFilePath.Directory.CreateDirRecursiveSpec where


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
  setTmpDir "CreateDirRecursiveSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createDir' "alreadyExists"
  createRegularFile' "alreadyExistsF"
  createDir' "noPerms"
  createDir' "noWritePerms"
  noPerms "noPerms"
  noWritableDirPerms "noWritePerms"

cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerms"
  deleteDir' "alreadyExists"
  deleteDir' "noPerms"
  deleteDir' "noWritePerms"
  deleteFile' "alreadyExistsF"


spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.RawFilePath.Directory.createDirRecursive" $ do

    -- successes --
    it "createDirRecursive, all fine" $ do
      createDirRecursive' "newDir"
      deleteDir' "newDir"

    it "createDirRecursive, parent directories do not exist" $ do
      createDirRecursive' "some/thing/dada"
      deleteDir' "some/thing/dada"
      deleteDir' "some/thing"
      deleteDir' "some"

    it "createDirRecursive, destination directory already exists" $
      createDirRecursive' "alreadyExists"

    -- posix failures --
    it "createDirRecursive, destination already exists and is a file" $
      createDirRecursive' "alreadyExistsF"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "createDirRecursive, can't write to output directory" $
      createDirRecursive' "noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDirRecursive, can't open output directory" $
      createDirRecursive' "noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)



