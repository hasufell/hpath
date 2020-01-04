{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.MoveFileOverwriteSpec where


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



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "MoveFileOverwriteSpec"
  createTmpDir


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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "HPath.IO.moveFile" $ do

    -- successes --
    it "moveFile (Overwrite), all fine" $
      moveFile' "myFile"
                "movedFile"
                Overwrite

    it "moveFile (Overwrite), all fine" $
      moveFile' "myFile"
                "dir/movedFile"
                Overwrite

    it "moveFile (Overwrite), all fine on symlink" $
      moveFile' "myFileL"
                "movedFile"
                Overwrite

    it "moveFile (Overwrite), all fine on directory" $
      moveFile' "dir"
                "movedFile"
                Overwrite

    it "moveFile (Overwrite), destination file already exists" $
      moveFile' "myFile"
                "alreadyExists"
                Overwrite

    -- posix failures --
    it "moveFile (Overwrite), source file does not exist" $
      moveFile' "fileDoesNotExist"
                "movedFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile (Overwrite), can't write to destination directory" $
      moveFile' "myFile"
                "noWritePerm/movedFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile (Overwrite), can't open destination directory" $
      moveFile' "myFile"
                "noPerms/movedFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile (Overwrite), can't open source directory" $
      moveFile' "noPerms/myFile"
                "movedFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --

    it "moveFile (Overwrite), move from file to dir" $
      moveFile' "myFile"
                "alreadyExistsD"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "moveFile (Overwrite), source and dest are same file" $
      moveFile' "myFile"
                "myFile"
                Overwrite
        `shouldThrow`
        isSameFile
