{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CreateRegularFileSpec where


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
  setTmpDir "CreateRegularFileSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createRegularFile' "alreadyExists"
  createDir' "noPerms"
  createDir' "noWritePerms"
  noPerms "noPerms"
  noWritableDirPerms "noWritePerms"

cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerms"
  deleteFile' "alreadyExists"
  deleteDir' "noPerms"
  deleteDir' "noWritePerms"


spec :: Spec
spec = beforeAll_ upTmpDir $ before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.createRegularFile" $ do

    -- successes --
    it "createRegularFile, all fine" $ do
      createRegularFile' "newDir"
      removeFileIfExists "newDir"

    -- posix failures --
    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, destination file already exists" $
      createRegularFile' "alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

