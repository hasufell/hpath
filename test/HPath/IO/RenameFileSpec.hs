{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.RenameFileSpec where


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
specDir = "test/HPath/IO/renameFileSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.renameFile" $ do

    -- successes --
    it "renameFile, all fine" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "renamedFile")

    it "renameFile, all fine" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "dir/renamedFile")

    it "renameFile, all fine on symlink" $
      renameFile' (specDir `ba` "myFileL")
                  (specDir `ba` "renamedFile")

    it "renameFile, all fine on directory" $
      renameFile' (specDir `ba` "dir")
                  (specDir `ba` "renamedFile")

    -- posix failures --
    it "renameFile, source file does not exist" $
      renameFile' (specDir `ba` "fileDoesNotExist")
                  (specDir `ba` "renamedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "renameFile, can't write to output directory" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "noWritePerm/renamedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open output directory" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "noPerms/renamedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open source directory" $
      renameFile' (specDir `ba` "noPerms/myFile")
                  (specDir `ba` "renamedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "renameFile, destination file already exists" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "alreadyExists")
        `shouldThrow`
        isFileDoesExist

    it "renameFile, move from file to dir" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        isDirDoesExist

    it "renameFile, source and dest are same file" $
      renameFile' (specDir `ba` "myFile")
                  (specDir `ba` "myFile")
        `shouldThrow`
        isSameFile

