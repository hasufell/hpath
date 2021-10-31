{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Posix.PosixFilePath.Directory.GetFileTypeSpec where

import Test.Hspec

#ifndef WINDOWS

import "hpath-directory" System.Posix.PosixFilePath.Directory
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
  createSymlink' "symlink" "regularfile" False
  createSymlink' "brokenSymlink" "broken" False
  createDir' "directory"
  createSymlink' "symlinkD" "directory" True
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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.getFileType" $ do

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

#else
spec :: Spec
spec = pure ()
#endif
