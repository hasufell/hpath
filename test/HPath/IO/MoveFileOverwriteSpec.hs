{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.MoveFileOverwriteSpec where


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
specDir = "test/HPath/IO/moveFileOverwriteSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.moveFileOverwrite" $ do

    -- successes --
    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "movedFile")

    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "dir/movedFile")

    it "moveFileOverwrite, all fine on symlink" $
      moveFileOverwrite' (specDir `ba` "myFileL")
                         (specDir `ba` "movedFile")

    it "moveFileOverwrite, all fine on directory" $
      moveFileOverwrite' (specDir `ba` "dir")
                         (specDir `ba` "movedFile")

    it "moveFileOverwrite, destination file already exists" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "alreadyExists")

    -- posix failures --
    it "moveFileOverwrite, source file does not exist" $
      moveFileOverwrite' (specDir `ba` "fileDoesNotExist")
                         (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFileOverwrite, can't write to destination directory" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "noWritePerm/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open destination directory" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "noPerms/movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open source directory" $
      moveFileOverwrite' (specDir `ba` "noPerms/myFile")
                         (specDir `ba` "movedFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFileOverwrite, move from file to dir" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        isDirDoesExist

    it "moveFileOverwrite, source and dest are same file" $
      moveFileOverwrite' (specDir `ba` "myFile")
                         (specDir `ba` "myFile")
        `shouldThrow`
        isSameFile

