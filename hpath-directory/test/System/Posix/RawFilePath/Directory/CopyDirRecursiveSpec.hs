{-# LANGUAGE OverloadedStrings #-}


module System.Posix.RawFilePath.Directory.CopyDirRecursiveSpec where


import Test.Hspec
import "hpath-directory" System.Posix.RawFilePath.Directory
import System.Posix.RawFilePath.Directory.Errors
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import System.Exit
import System.Process
import Utils
import Data.ByteString.UTF8 (toString)



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "CopyDirRecursiveSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createRegularFile' "alreadyExists"
  createRegularFile' "wrongInput"
  createSymlink' "wrongInputSymL" "inputDir/"
  createDir' "alreadyExistsD"
  createDir' "noPerms"
  createDir' "noWritePerm"

  createDir' "inputDir"
  createDir' "inputDir/bar"
  createDir' "inputDir/foo"
  createRegularFile' "inputDir/foo/inputFile1"
  createRegularFile' "inputDir/inputFile2"
  createRegularFile' "inputDir/bar/inputFile3"
  writeFile' "inputDir/foo/inputFile1" "SDAADSdsada"
  writeFile' "inputDir/inputFile2" "Blahfaselgagaga"
  writeFile' "inputDir/bar/inputFile3"
    "fdfdssdffsd3223sasdasdasdadasasddasdasdasasd4"

  noPerms "noPerms"
  noWritableDirPerms "noWritePerm"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerm"
  deleteFile' "alreadyExists"
  deleteFile' "wrongInput"
  deleteFile' "wrongInputSymL"
  deleteDir' "alreadyExistsD"
  deleteDir' "noPerms"
  deleteDir' "noWritePerm"
  deleteFile' "inputDir/foo/inputFile1"
  deleteFile' "inputDir/inputFile2"
  deleteFile' "inputDir/bar/inputFile3"
  deleteDir' "inputDir/foo"
  deleteDir' "inputDir/bar"
  deleteDir' "inputDir"




spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.RawFilePath.Directory.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive (Strict, FailEarly), all fine" $ do
      copyDirRecursive' "inputDir"
                        "outputDir"
                        Strict
                        FailEarly
      removeDirIfExists "outputDir"

    it "copyDirRecursive (Strict, FailEarly), all fine and compare" $ do
      tmpDir' <- getRawTmpDir
      copyDirRecursive' "inputDir"
                        "outputDir"
                        Strict
                        FailEarly
      (system $ "diff -r "
                          ++ toString tmpDir' ++ "inputDir" ++ " "
                          ++ toString tmpDir' ++ "outputDir"
                          ++ " >/dev/null")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"

    -- posix failures --
    it "copyDirRecursive (Strict, FailEarly), source directory does not exist" $
      copyDirRecursive' "doesNotExist"
                        "outputDir"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive (Strict, FailEarly), no write permission on output dir" $
      copyDirRecursive' "inputDir"
                        "noWritePerm/foo"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive (Strict, FailEarly), cannot open output dir" $
      copyDirRecursive' "inputDir"
                        "noPerms/foo"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive (Strict, FailEarly), cannot open source dir" $
      copyDirRecursive' "noPerms/inputDir"
                        "foo"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive (Strict, FailEarly), destination dir already exists" $
      copyDirRecursive' "inputDir"
                        "alreadyExistsD"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive (Strict, FailEarly), destination already exists and is a file" $
      copyDirRecursive' "inputDir"
                        "alreadyExists"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive (Strict, FailEarly), wrong input (regular file)" $
      copyDirRecursive' "wrongInput"
                        "outputDir"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive (Strict, FailEarly), wrong input (symlink to directory)" $
      copyDirRecursive' "wrongInputSymL"
                        "outputDir"
                        Strict
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursive (Strict, FailEarly), destination in source" $
      copyDirRecursive' "inputDir"
                        "inputDir/foo"
                        Strict
                        FailEarly
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursive (Strict, FailEarly), destination and source same directory" $
      copyDirRecursive' "inputDir"
                        "inputDir"
                        Strict
                        FailEarly
        `shouldThrow`
        isSameFile


