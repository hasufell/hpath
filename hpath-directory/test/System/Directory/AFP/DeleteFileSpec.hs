{-# LANGUAGE OverloadedStrings #-}

module System.Directory.AFP.DeleteFileSpec where


import System.Directory.AbstractFilePath
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
  setTmpDir "DeleteFileSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "foo"
  createSymlink' "syml" "foo" False
  createDir' "dir"
  createDir' "noPerms"
  noPerms "noPerms"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  deleteFile' "foo"
  deleteFile' "syml"
  deleteDir' "dir"
  deleteDir' "noPerms"



spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.deleteFile" $ do

    -- successes --
    it "deleteFile, regular file, all fine" $ do
      createRegularFile' "testFile"
      deleteFile' "testFile"
      dirExists "testFile" >>= (`shouldBe` False)

    it "deleteFile, symlink, all fine" $ do
      recreateSymlink' "syml"
                       "testFile"
                       Strict
      deleteFile' "testFile"
      dirExists "testFile" >>= (`shouldBe` False)

    -- posix failures --
    it "deleteFile, wrong file type (directory)" $
      deleteFile' "dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType || ioeGetErrorType e == PermissionDenied)

    it "deleteFile, file does not exist" $
      deleteFile' "doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, can't read directory" $
      deleteFile' "noPerms/blah"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

