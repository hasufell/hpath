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



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "RenameFileSpec"
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
  describe "HPath.IO.renameFile" $ do

    -- successes --
    it "renameFile, all fine" $
      renameFile' "myFile"
                  "renamedFile"

    it "renameFile, all fine" $
      renameFile' "myFile"
                  "dir/renamedFile"

    it "renameFile, all fine on symlink" $
      renameFile' "myFileL"
                  "renamedFile"

    it "renameFile, all fine on directory" $
      renameFile' "dir"
                  "renamedFile"

    -- posix failures --
    it "renameFile, source file does not exist" $
      renameFile' "fileDoesNotExist"
                  "renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "renameFile, can't write to output directory" $
      renameFile' "myFile"
                  "noWritePerm/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open output directory" $
      renameFile' "myFile"
                  "noPerms/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open source directory" $
      renameFile' "noPerms/myFile"
                  "renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "renameFile, destination file already exists" $
      renameFile' "myFile"
                  "alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "renameFile, move from file to dir" $
      renameFile' "myFile"
                  "alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "renameFile, source and dest are same file" $
      renameFile' "myFile"
                  "myFile"
        `shouldThrow`
        isSameFile

