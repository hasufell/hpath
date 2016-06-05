{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.RecreateSymlinkSpec where




import Test.Hspec
import HPath.IO
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


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "myFile"
  createSymlink' "myFileL" "myFile"
  createRegularFile' "alreadyExists"
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
  deleteFile' "alreadyExists"
  deleteDir' "alreadyExistsD"
  deleteDir' "dir"
  deleteDir' "noPerms"
  deleteDir' "noWritePerm"


spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.recreateSymlink" $ do

    -- successes --
    it "recreateSymLink (Strict), all fine" $ do
      recreateSymlink' "myFileL"
                       "movedFile"
                       Strict
      removeFileIfExists "movedFile"

    it "recreateSymLink (Strict), all fine" $ do
      recreateSymlink' "myFileL"
                       "dir/movedFile"
                       Strict
      removeFileIfExists "dir/movedFile"

    -- posix failures --
    it "recreateSymLink (Strict), wrong input type (file)" $
      recreateSymlink' "myFile"
                       "movedFile"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink (Strict), wrong input type (directory)" $
      recreateSymlink' "dir"
                       "movedFile"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink (Strict), can't write to destination directory" $
      recreateSymlink' "myFileL"
                       "noWritePerm/movedFile"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink (Strict), can't open destination directory" $
      recreateSymlink' "myFileL"
                       "noPerms/movedFile"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink (Strict), can't open source directory" $
      recreateSymlink' "noPerms/myFileL"
                       "movedFile"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink (Strict), destination file already exists" $
      recreateSymlink' "myFileL"
                       "alreadyExists"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink (Strict), destination already exists and is a dir" $
      recreateSymlink' "myFileL"
                       "alreadyExistsD"
                       Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    -- custom failures --
    it "recreateSymLink (Strict), source and destination are the same file" $
      recreateSymlink' "myFileL"
                       "myFileL"
                       Strict
        `shouldThrow`
        isSameFile

