{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.MoveFileSpec where


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
specDir = "test/HPath/IO/moveFileSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.moveFile" $ do

    -- successes --
    it "moveFile, all fine" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "movedFile")

    it "moveFile, all fine" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "dir/movedFile")

    it "moveFile, all fine on symlink" $
      moveFile' (specDir `ba` "myFileL")
                (specDir `ba` "movedFile")

    it "moveFile, all fine on directory" $
      moveFile' (specDir `ba` "dir")
                (specDir `ba` "movedFile")

    -- posix failures --
    it "moveFile, source file does not exist" $
      moveFile' (specDir `ba` "fileDoesNotExist")
                (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile, can't write to destination directory" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "noWritePerm/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open destination directory" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "noPerms/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open source directory" $
      moveFile' (specDir `ba` "noPerms/myFile")
                (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFile, destination file already exists" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "alreadyExists")
        `shouldThrow`
        isFileDoesExist

    it "moveFile, move from file to dir" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        isDirDoesExist

    it "moveFile, source and dest are same file" $
      moveFile' (specDir `ba` "myFile")
                (specDir `ba` "myFile")
        `shouldThrow`
        isSameFile

