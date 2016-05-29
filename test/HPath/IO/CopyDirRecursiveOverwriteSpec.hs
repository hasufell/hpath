{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.CopyDirRecursiveOverwriteSpec where


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
import System.Exit
import System.Process
import Utils
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)



setupFiles :: IO ()
setupFiles = do
  createRegularFile' "alreadyExists"
  createRegularFile' "wrongInput"
  createSymlink' "wrongInputSymL" "inputDir/"
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

  createDir' "alreadyExistsD"
  createDir' "alreadyExistsD/bar"
  createDir' "alreadyExistsD/foo"
  createRegularFile' "alreadyExistsD/foo/inputFile1"
  createRegularFile' "alreadyExistsD/inputFile2"
  createRegularFile' "alreadyExistsD/bar/inputFile3"
  writeFile' "alreadyExistsD/foo/inputFile1" "DAAsada"
  writeFile' "alreadyExistsD/inputFile2" "ahfaagaga"
  writeFile' "alreadyExistsD/bar/inputFile3"
    "f3223sasdasdaasdasdasasd4"

  noPerms "noPerms"
  noWritableDirPerms "noWritePerm"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerm"
  deleteFile' "alreadyExists"
  deleteFile' "wrongInput"
  deleteFile' "wrongInputSymL"
  deleteDir' "noPerms"
  deleteDir' "noWritePerm"
  deleteFile' "inputDir/foo/inputFile1"
  deleteFile' "inputDir/inputFile2"
  deleteFile' "inputDir/bar/inputFile3"
  deleteDir' "inputDir/foo"
  deleteDir' "inputDir/bar"
  deleteDir' "inputDir"
  deleteFile' "alreadyExistsD/foo/inputFile1"
  deleteFile' "alreadyExistsD/inputFile2"
  deleteFile' "alreadyExistsD/bar/inputFile3"
  deleteDir' "alreadyExistsD/foo"
  deleteDir' "alreadyExistsD/bar"
  deleteDir' "alreadyExistsD"



spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.copyDirRecursiveOverwrite" $ do

    -- successes --
    it "copyDirRecursiveOverwrite, all fine" $ do
      copyDirRecursiveOverwrite' "inputDir"
                                 "outputDir"
      removeDirIfExists "outputDir"

    it "copyDirRecursiveOverwrite, all fine and compare" $ do
      copyDirRecursiveOverwrite' "inputDir"
                                 "outputDir"
      (system $ "diff -r --no-dereference "
                          ++ toString tmpDir ++ "inputDir" ++ " "
                          ++ toString tmpDir ++ "outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"

    it "copyDirRecursiveOverwrite, destination dir already exists" $ do
      (system $ "diff -r --no-dereference "
                          ++ toString tmpDir ++ "inputDir" ++ " "
                          ++ toString tmpDir ++ "alreadyExistsD")
        `shouldReturn` (ExitFailure 1)
      copyDirRecursiveOverwrite' "inputDir"
                                 "alreadyExistsD"
      (system $ "diff -r --no-dereference "
                          ++ toString tmpDir ++ "inputDir" ++ " "
                          ++ toString tmpDir ++ "alreadyExistsD")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"

    -- posix failures --
    it "copyDirRecursiveOverwrite, source directory does not exist" $
      copyDirRecursiveOverwrite' "doesNotExist"
                                 "outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursiveOverwrite, no write permission on output dir" $
      copyDirRecursiveOverwrite' "inputDir"
                                 "noWritePerm/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open output dir" $
      copyDirRecursiveOverwrite' "inputDir"
                                 "noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open source dir" $
      copyDirRecursiveOverwrite' "noPerms/inputDir"
                                 "foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, destination already exists and is a file" $
      copyDirRecursiveOverwrite' "inputDir"
                                 "alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (regular file)" $
      copyDirRecursiveOverwrite' "wrongInput"
                                 "outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (symlink to directory)" $
      copyDirRecursiveOverwrite' "wrongInputSymL"
                                 "outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursiveOverwrite, destination in source" $
      copyDirRecursiveOverwrite' "inputDir"
                                 "inputDir/foo"
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursiveOverwrite, destination and source same directory" $
      copyDirRecursiveOverwrite' "inputDir"
                                 "inputDir"
        `shouldThrow`
        isSameFile
