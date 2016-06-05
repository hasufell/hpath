{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.RecreateSymlinkOverwriteSpec where


-- TODO: exception if destination exists but is not a file + `OverWrite` CopyMode


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
    it "recreateSymLink (Overwrite), all fine" $ do
      recreateSymlink' "myFileL"
                       "movedFile"
                       Overwrite
      removeFileIfExists "movedFile"

    it "recreateSymLink (Overwrite), all fine" $ do
      recreateSymlink' "myFileL"
                       "dir/movedFile"
                       Overwrite
      removeFileIfExists "dir/movedFile"

    it "recreateSymLink (Overwrite), destination file already exists" $
      recreateSymlink' "myFileL"
                       "alreadyExists"
                       Overwrite

    it "recreateSymLink (Overwrite), destination already exists and is a dir" $ do
      recreateSymlink' "myFileL"
                       "alreadyExistsD"
                       Overwrite
      deleteFile' "alreadyExistsD"
      createDir' "alreadyExistsD"

    -- posix failures --
    it "recreateSymLink (Overwrite), wrong input type (file)" $
      recreateSymlink' "myFile"
                       "movedFile"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink (Overwrite), wrong input type (directory)" $
      recreateSymlink' "dir"
                       "movedFile"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink (Overwrite), can't write to destination directory" $
      recreateSymlink' "myFileL"
                       "noWritePerm/movedFile"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink (Overwrite), can't open destination directory" $
      recreateSymlink' "myFileL"
                       "noPerms/movedFile"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink (Overwrite), can't open source directory" $
      recreateSymlink' "noPerms/myFileL"
                       "movedFile"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "recreateSymLink (Overwrite), source and destination are the same file" $
      recreateSymlink' "myFileL"
                       "myFileL"
                       Overwrite
        `shouldThrow`
        isSameFile

