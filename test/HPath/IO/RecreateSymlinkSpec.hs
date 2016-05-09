{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.RecreateSymlinkSpec where


import Test.Hspec
import HPath.IO.Errors
import System.IO.Error
  (
    ioeGetErrorType
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
specDir = "test/HPath/IO/recreateSymlinkSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.recreateSymlink" $ do

    -- successes --
    it "recreateSymLink, all fine" $ do
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "movedFile")
      removeFileIfExists (specDir `ba` "movedFile")

    it "recreateSymLink, all fine" $ do
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "dir/movedFile")
      removeFileIfExists (specDir `ba` "dir/movedFile")

    -- posix failures --
    it "recreateSymLink, wrong input type (file)" $
      recreateSymlink' (specDir `ba` "myFile")
                       (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, wrong input type (directory)" $
      recreateSymlink' (specDir `ba` "dir")
                       (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, can't write to destination directory" $
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "noWritePerm/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open destination directory" $
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "noPerms/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open source directory" $
      recreateSymlink' (specDir `ba` "noPerms/myFileL")
                       (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, destination file already exists" $
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, destination already exists and is a dir" $
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    -- custom failures --
    it "recreateSymLink, source and destination are the same file" $
      recreateSymlink' (specDir `ba` "myFileL")
                       (specDir `ba` "myFileL")
        `shouldThrow`
        isSameFile

