{-# LANGUAGE OverloadedStrings #-}

module System.Posix.PosixFilePath.Directory.DeleteDirRecursiveSpec where


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



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "DeleteDirRecursiveSpec"
  createTmpDir


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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.deleteDirRecursive" $ do

    -- successes --
    it "deleteDirRecursive, empty directory, all fine" $ do
      createDir' "testDir"
      deleteDirRecursive' "testDir"
      getSymbolicLinkStatus "testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDirRecursive, empty directory with null permissions, all fine" $ do
      createDir' "noPerms/testDir"
      noPerms "noPerms/testDir"
      deleteDirRecursive' "noPerms/testDir"

    it "deleteDirRecursive, non-empty directory, all fine" $ do
      createDir' "nonEmpty"
      createDir' "nonEmpty/dir1"
      createDir' "nonEmpty/dir2"
      createDir' "nonEmpty/dir2/dir3"
      createRegularFile' "nonEmpty/file1"
      createRegularFile' "nonEmpty/dir1/file2"
      deleteDirRecursive' "nonEmpty"
      getSymbolicLinkStatus "nonEmpty"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDirRecursive, can't open parent directory" $ do
      createDir' "noPerms/foo"
      noPerms "noPerms"
      (deleteDirRecursive' "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)
      normalDirPerms "noPerms"
      deleteDir' "noPerms/foo"

    it "deleteDirRecursive, can't write to parent directory" $ do
      createDir' "noWritable/foo"
      noWritableDirPerms "noWritable"
      (deleteDirRecursive' "noWritable/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)
      normalDirPerms "noWritable"
      deleteDir' "noWritable/foo"

    it "deleteDirRecursive, wrong file type (symlink to directory)" $
      deleteDirRecursive' "dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, wrong file type (regular file)" $
      deleteDirRecursive' "file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, directory does not exist" $
      deleteDirRecursive' "doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)


