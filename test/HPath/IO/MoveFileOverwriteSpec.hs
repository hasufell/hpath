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


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "myFile"
  createSymlink' "myFileL" "myFile"
  createDir' "alreadyExistsD"
  createDir' "dir"
  createDir' "noPerms"
  createDir' "noWritePerm"
  noPerms "noPerms"
  noWritableDirPerms "noWritePerm"
  writeFile' "myFile" "Blahfaselgagaga"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerm"
  deleteFile' "myFile"
  deleteFile' "myFileL"
  deleteDir' "alreadyExistsD"
  deleteDir' "dir"
  deleteDir' "noPerms"
  deleteDir' "noWritePerm"



spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.moveFileOverwrite" $ do

    -- successes --
    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' "myFile"
                         "movedFile"

    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' "myFile"
                         "dir/movedFile"

    it "moveFileOverwrite, all fine on symlink" $
      moveFileOverwrite' "myFileL"
                         "movedFile"

    it "moveFileOverwrite, all fine on directory" $
      moveFileOverwrite' "dir"
                         "movedFile"

    it "moveFileOverwrite, destination file already exists" $
      moveFileOverwrite' "myFile"
                         "alreadyExists"

    -- posix failures --
    it "moveFileOverwrite, source file does not exist" $
      moveFileOverwrite' "fileDoesNotExist"
                         "movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFileOverwrite, can't write to destination directory" $
      moveFileOverwrite' "myFile"
                         "noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open destination directory" $
      moveFileOverwrite' "myFile"
                         "noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open source directory" $
      moveFileOverwrite' "noPerms/myFile"
                         "movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFileOverwrite, move from file to dir" $
      moveFileOverwrite' "myFile"
                         "alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "moveFileOverwrite, source and dest are same file" $
      moveFileOverwrite' "myFile"
                         "myFile"
        `shouldThrow`
        isSameFile

