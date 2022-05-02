{-# LANGUAGE OverloadedStrings #-}


module System.Directory.AFP.CopyDirRecursiveOverwriteSpec where


import Test.Hspec
import System.Directory.AbstractFilePath hiding (writeFile')
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
import System.AbstractFilePath



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "CopyDirRecursiveOverwriteSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "alreadyExists"
  createRegularFile' "wrongInput"
  createSymlink' "wrongInputSymL" "inputDir/" True
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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive (Overwrite, FailEarly), all fine" $ do
      copyDirRecursive' "inputDir"
                        "outputDir"
                        Overwrite
                        FailEarly
      removeDirIfExists "outputDir"

    it "copyDirRecursive (Overwrite, FailEarly), all fine and compare" $ do
      tmpDir' <- getRawTmpDir
      tmpDirS <- fromAbstractFilePathIO tmpDir'
      copyDirRecursive' "inputDir"
                        "outputDir"
                        Overwrite
                        FailEarly
      (system $ "diff -r "
                          ++ tmpDirS ++ "inputDir" ++ " "
                          ++ tmpDirS ++ "outputDir"
                          ++ " >/dev/null")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"

    it "copyDirRecursive (Overwrite, FailEarly), destination dir already exists" $ do
      tmpDir' <- getRawTmpDir
      tmpDirS <- fromAbstractFilePathIO tmpDir'
      (system $ "diff -r "
                          ++ tmpDirS ++ "inputDir" ++ " "
                          ++ tmpDirS ++ "alreadyExistsD"
                          ++ " >/dev/null")
        `shouldReturn` (ExitFailure 1)
      copyDirRecursive' "inputDir"
                        "alreadyExistsD"
                        Overwrite
                        FailEarly
      (system $ "diff -r "
                          ++ tmpDirS ++ "inputDir" ++ " "
                          ++ tmpDirS ++ "alreadyExistsD"
                          ++ " >/dev/null")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"


    -- posix failures --
    it "copyDirRecursive, source directory does not exist" $
      copyDirRecursive' "doesNotExist"
                        "outputDir"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive, no write permission on output dir" $
      copyDirRecursive' "inputDir"
                        "noWritePerm/foo"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open output dir" $
      copyDirRecursive' "inputDir"
                        "noPerms/foo"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open source dir" $
      copyDirRecursive' "noPerms/inputDir"
                        "foo"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, destination already exists and is a file" $
      copyDirRecursive' "inputDir"
                        "alreadyExists"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive, wrong input (regular file)" $
      copyDirRecursive' "wrongInput"
                        "outputDir"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive, wrong input (symlink to directory)" $
      copyDirRecursive' "wrongInputSymL"
                        "outputDir"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursive (Overwrite, FailEarly), destination in source" $
      copyDirRecursive' "inputDir"
                        "inputDir/foo"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> case e of
                DestinationInSource{} -> True
                _                     -> False)

    it "copyDirRecursive (Overwrite, FailEarly), destination and source same directory" $
      copyDirRecursive' "inputDir"
                        "inputDir"
                        Overwrite
                        FailEarly
        `shouldThrow`
        (\e -> case e of
                SameFile{} -> True
                _          -> False)
