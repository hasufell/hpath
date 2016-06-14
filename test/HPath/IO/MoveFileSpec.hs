{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.MoveFileSpec where


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
  setTmpDir "MoveFileSpec"
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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "HPath.IO.moveFile" $ do

    -- successes --
    it "moveFile (Strict), all fine" $
      moveFile' "myFile"
                "movedFile"
                Strict

    it "moveFile (Strict), all fine" $
      moveFile' "myFile"
                "dir/movedFile"
                Strict

    it "moveFile (Strict), all fine on symlink" $
      moveFile' "myFileL"
                "movedFile"
                Strict

    it "moveFile (Strict), all fine on directory" $
      moveFile' "dir"
                "movedFile"
                Strict

    -- posix failures --
    it "moveFile (Strict), source file does not exist" $
      moveFile' "fileDoesNotExist"
                "movedFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile (Strict), can't write to destination directory" $
      moveFile' "myFile"
                "noWritePerm/movedFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile (Strict), can't open destination directory" $
      moveFile' "myFile"
                "noPerms/movedFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile (Strict), can't open source directory" $
      moveFile' "noPerms/myFile"
                "movedFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFile (Strict), destination file already exists" $
      moveFile' "myFile"
                "alreadyExists"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "moveFile (Strict), move from file to dir" $
      moveFile' "myFile"
                "alreadyExistsD"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "moveFile (Strict), source and dest are same file" $
      moveFile' "myFile"
                "myFile"
                Strict
        `shouldThrow`
        isSameFile
