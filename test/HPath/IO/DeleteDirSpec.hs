{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.DeleteDirSpec where


import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import System.Posix.Files.ByteString
  (
    getSymbolicLinkStatus
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "file"
  createDir' "dir"
  createRegularFile' "dir/.keep"
  createSymlink' "dirSym" "dir/"
  createDir' "noPerms"
  createRegularFile' "noPerms/.keep"
  createDir' "noWritable"
  createRegularFile' "noWritable/.keep"


cleanupFiles :: IO ()
cleanupFiles = do
  deleteFile' "file"
  deleteFile' "dir/.keep"
  deleteDir' "dir"
  deleteFile' "dirSym"
  deleteFile' "noPerms/.keep"
  deleteDir' "noPerms"
  deleteFile' "noWritable/.keep"
  deleteDir' "noWritable"


spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.deleteDir" $ do

    -- successes --
    it "deleteDir, empty directory, all fine" $ do
      createDir' "testDir"
      deleteDir' "testDir"
      getSymbolicLinkStatus "testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory with null permissions, all fine" $ do
      createDir' "noPerms/testDir"
      noPerms "noPerms/testDir"
      deleteDir' "noPerms/testDir"
      getSymbolicLinkStatus "testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDir, wrong file type (symlink to directory)" $
      deleteDir' "dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, wrong file type (regular file)" $
      deleteDir' "file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, directory does not exist" $
      deleteDir' "doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory not empty" $
      deleteDir' "dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

    it "deleteDir, can't open parent directory" $ do
      createDir' "noPerms/foo"
      noPerms "noPerms"
      (deleteDir' "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)
      normalDirPerms "noPerms"
      deleteDir' "noPerms/foo"

    it "deleteDir, can't write to parent directory, still fine" $ do
      createDir' "noWritable/foo"
      noWritableDirPerms "noWritable"
      (deleteDir' "noWritable/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)
      normalDirPerms "noWritable"
      deleteDir' "noWritable/foo"



