{-# LANGUAGE OverloadedStrings #-}

module System.Directory.AFP.RecreateSymlinkOverwriteSpec where


-- TODO: exception if destination exists but is not a file + `OverWrite` CopyMode


import Test.Hspec
import "hpath-directory" System.Directory.AFP
import System.Directory.Types
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils


upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "RecreateSymlinkOverwriteSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "myFile"
  createSymlink' "myFileL" "myFile"
  createRegularFile' "alreadyExists"
  createDir' "alreadyExistsD"
  createDir' "dir"
  createDir' "noPerms"
  createDir' "noWritePerm"
  createDir' "alreadyExistsD2"
  createRegularFile' "alreadyExistsD2/lala"
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
  deleteFile' "alreadyExistsD2/lala"
  deleteDir' "alreadyExistsD"
  deleteDir' "alreadyExistsD2"
  deleteDir' "dir"
  deleteDir' "noPerms"
  deleteDir' "noWritePerm"


spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.recreateSymlink" $ do

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

    it "recreateSymLink (Overwrite), destination already exists and is an empty dir" $ do
      recreateSymlink' "myFileL"
                       "alreadyExistsD"
                       Overwrite
      deleteFile' "alreadyExistsD"
      createDir' "alreadyExistsD"

    -- posix failures --
    it "recreateSymLink (Overwrite), destination already exists and is a non-empty dir" $
      recreateSymlink' "myFileL"
                       "alreadyExistsD2"
                       Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

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
        (\e -> case e of
                SameFile{} -> True
                _          -> False)

