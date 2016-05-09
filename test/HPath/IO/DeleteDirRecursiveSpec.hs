{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.DeleteDirRecursiveSpec where


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


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/deleteDirRecursiveSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.deleteDirRecursive" $ do

    -- successes --
    it "deleteDirRecursive, empty directory, all fine" $ do
      createDir' (specDir `ba` "testDir")
      deleteDirRecursive' (specDir `ba` "testDir")
      getSymbolicLinkStatus (specDir `ba` "testDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDirRecursive, empty directory with null permissions, all fine" $ do
      createDir' (specDir `ba` "noPerms/testDir")
      noPerms (specDir `ba` "noPerms/testDir")
      deleteDirRecursive' (specDir `ba` "noPerms/testDir")

    it "deleteDirRecursive, non-empty directory, all fine" $ do
      createDir' (specDir `ba` "nonEmpty")
      createDir' (specDir `ba` "nonEmpty/dir1")
      createDir' (specDir `ba` "nonEmpty/dir2")
      createDir' (specDir `ba` "nonEmpty/dir2/dir3")
      createRegularFile' (specDir `ba` "nonEmpty/file1")
      createRegularFile' (specDir `ba` "nonEmpty/dir1/file2")
      deleteDirRecursive' (specDir `ba` "nonEmpty")
      getSymbolicLinkStatus (specDir `ba` "nonEmpty")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDirRecursive, can't open parent directory" $ do
      createDir' (specDir `ba` "noPerms/foo")
      noPerms (specDir `ba` "noPerms")
      (deleteDirRecursive' (specDir `ba` "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms (specDir `ba` "noPerms")
        >> deleteDir' (specDir `ba` "noPerms/foo")

    it "deleteDirRecursive, can't write to parent directory" $ do
      createDir' (specDir `ba` "noWritable/foo")
      noWritableDirPerms (specDir `ba` "noWritable")
      (deleteDirRecursive' (specDir `ba` "noWritable/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms (specDir `ba` "noWritable")
      deleteDir' (specDir `ba` "noWritable/foo")

    it "deleteDirRecursive, wrong file type (symlink to directory)" $
      deleteDirRecursive' (specDir `ba` "dirSym")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, wrong file type (regular file)" $
      deleteDirRecursive' (specDir `ba` "file")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, directory does not exist" $
      deleteDirRecursive' (specDir `ba` "doesNotExist")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)


