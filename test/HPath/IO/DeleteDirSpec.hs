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


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/deleteDirSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.deleteDir" $ do

    -- successes --
    it "deleteDir, empty directory, all fine" $ do
      createDir' (specDir `ba` "testDir")
      deleteDir' (specDir `ba` "testDir")
      getSymbolicLinkStatus (specDir `ba` "testDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory with null permissions, all fine" $ do
      createDir' (specDir `ba` "noPerms/testDir")
      noPerms (specDir `ba` "noPerms/testDir")
      deleteDir' (specDir `ba` "noPerms/testDir")
      getSymbolicLinkStatus (specDir `ba` "testDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDir, wrong file type (symlink to directory)" $
      deleteDir' (specDir `ba` "dirSym")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, wrong file type (regular file)" $
      deleteDir' (specDir `ba` "file")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, directory does not exist" $
      deleteDir' (specDir `ba` "doesNotExist")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory not empty" $
      deleteDir' (specDir `ba` "dir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

    it "deleteDir, can't open parent directory" $ do
      createDir' (specDir `ba` "noPerms/foo")
      noPerms (specDir `ba` "noPerms")
      (deleteDir' (specDir `ba` "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms (specDir `ba` "noPerms")
        >> deleteDir' (specDir `ba` "noPerms/foo")

    it "deleteDir, can't write to parent directory, still fine" $ do
      createDir' (specDir `ba` "noWritable/foo")
      noWritableDirPerms (specDir `ba` "noWritable")
      (deleteDir' (specDir `ba` "noWritable/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms (specDir `ba` "noWritable")
      deleteDir' (specDir `ba` "noWritable/foo")



