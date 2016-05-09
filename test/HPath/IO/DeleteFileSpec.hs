{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.DeleteFileSpec where


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
specDir = "test/HPath/IO/deleteFileSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.deleteFile" $ do

    -- successes --
    it "deleteFile, regular file, all fine" $ do
      createRegularFile' (specDir `ba` "testFile")
      deleteFile' (specDir `ba` "testFile")
      getSymbolicLinkStatus (specDir `ba` "testFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, symlink, all fine" $ do
      recreateSymlink' (specDir `ba` "syml")
                       (specDir `ba` "testFile")
      deleteFile' (specDir `ba` "testFile")
      getSymbolicLinkStatus (specDir `ba` "testFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteFile, wrong file type (directory)" $
      deleteFile' (specDir `ba` "dir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteFile, file does not exist" $
      deleteFile' (specDir `ba` "doesNotExist")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, can't read directory" $
      deleteFile' (specDir `ba` "noPerms/blah")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

