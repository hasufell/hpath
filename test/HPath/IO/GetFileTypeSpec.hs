{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.GetFileTypeSpec where


import HPath.IO
import Test.Hspec
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
  setTmpDir "GetFileTypeSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "regularfile"
  createSymlink' "symlink" "regularfile"
  createSymlink' "brokenSymlink" "broken"
  createDir' "directory"
  createSymlink' "symlinkD" "directory"
  createDir' "noPerms"
  noPerms "noPerms"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  deleteFile' "regularfile"
  deleteFile' "symlink"
  deleteFile' "brokenSymlink"
  deleteDir' "directory"
  deleteFile' "symlinkD"
  deleteDir' "noPerms"



spec :: Spec
spec = beforeAll_ upTmpDir $ before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.getFileType" $ do

    -- successes --
    it "getFileType, regular file" $
      getFileType' "regularfile"
        `shouldReturn` RegularFile

    it "getFileType, directory" $
      getFileType' "directory"
        `shouldReturn` Directory

    it "getFileType, directory with null permissions" $
      getFileType' "noPerms"
        `shouldReturn` Directory

    it "getFileType, symlink to file" $
      getFileType' "symlink"
        `shouldReturn` SymbolicLink

    it "getFileType, symlink to directory" $
      getFileType' "symlinkD"
        `shouldReturn` SymbolicLink

    it "getFileType, broken symlink" $
      getFileType' "brokenSymlink"
        `shouldReturn` SymbolicLink

    -- posix failures --
    it "getFileType, file does not exist" $
      getFileType' "nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getFileType, can't open directory" $
      getFileType' "noPerms/forz"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

