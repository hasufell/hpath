{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.CopyDirRecursiveCollectFailuresSpec where


import Test.Hspec
import Data.List (sort)
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
import System.Exit
import System.Process
import Utils
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (toString)



upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "CopyDirRecursiveCollectFailuresSpec"
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

  createDir' "inputDir1"
  createDir' "inputDir1/foo2"
  createDir' "inputDir1/foo2/foo3"
  createDir' "inputDir1/foo2/foo4"
  createRegularFile' "inputDir1/foo2/inputFile1"
  createRegularFile' "inputDir1/foo2/inputFile2"
  createRegularFile' "inputDir1/foo2/inputFile3"
  createRegularFile' "inputDir1/foo2/foo4/inputFile4"
  createRegularFile' "inputDir1/foo2/foo4/inputFile6"
  createRegularFile' "inputDir1/foo2/foo3/inputFile5"
  noPerms "inputDir1/foo2/foo3"

  createDir' "outputDir1"
  createDir' "outputDir1/foo2"
  createDir' "outputDir1/foo2/foo4"
  createDir' "outputDir1/foo2/foo4/inputFile4"
  createRegularFile' "outputDir1/foo2/foo4/inputFile6"
  noPerms "outputDir1/foo2/foo4/inputFile4"
  noPerms "outputDir1/foo2/foo4"

  noPerms "noPerms"
  noWritableDirPerms "noWritePerm"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerm"

  normalDirPerms "inputDir1/foo2/foo3"
  deleteFile' "inputDir1/foo2/foo4/inputFile4"
  deleteFile' "inputDir1/foo2/foo4/inputFile6"
  deleteFile' "inputDir1/foo2/inputFile1"
  deleteFile' "inputDir1/foo2/inputFile2"
  deleteFile' "inputDir1/foo2/inputFile3"
  deleteFile' "inputDir1/foo2/foo3/inputFile5"
  deleteDir' "inputDir1/foo2/foo3"
  deleteDir' "inputDir1/foo2/foo4"
  deleteDir' "inputDir1/foo2"
  deleteDir' "inputDir1"

  normalDirPerms "outputDir1/foo2/foo4"
  normalDirPerms "outputDir1/foo2/foo4/inputFile4"
  deleteFile' "outputDir1/foo2/foo4/inputFile6"
  deleteDir' "outputDir1/foo2/foo4/inputFile4"
  deleteDir' "outputDir1/foo2/foo4"
  deleteDir' "outputDir1/foo2"
  deleteDir' "outputDir1"

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
  describe "HPath.IO.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive (Strict, CollectFailures), all fine and compare" $ do
      tmpDir' <- getRawTmpDir
      copyDirRecursive' "inputDir"
                        "outputDir"
                        Strict
                        CollectFailures
      (system $ "diff -r --no-dereference "
                          ++ toString tmpDir' ++ "inputDir" ++ " "
                          ++ toString tmpDir' ++ "outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists "outputDir"

    -- posix failures --
    it "copyDirRecursive (Strict, CollectFailures), source directory does not exist" $
      copyDirRecursive' "doesNotExist"
                        "outputDir"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive (Strict, CollectFailures), cannot open source dir" $
      copyDirRecursive' "noPerms/inputDir"
                        "foo"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)


    -- custom failures
    it "copyDirRecursive (Overwrite, CollectFailures), various failures" $ do
      copyDirRecursive' "inputDir1/foo2"
                        "outputDir1/foo2"
                        Overwrite
                        CollectFailures
        `shouldThrow`
        (\(RecursiveFailure ex@[_, _]) ->
          any (\(h, e) -> ioeGetErrorType e == InappropriateType
                          && isCopyFileFailed h) ex &&
          any (\(h, e) -> ioeGetErrorType e == PermissionDenied
                          && isReadContentsFailed h) ex)
      normalDirPerms "outputDir1/foo2/foo4"
      normalDirPerms "outputDir1/foo2/foo4/inputFile4"
      c <- allDirectoryContents' "outputDir1"
      tmpDir' <- getRawTmpDir
      let shouldC = (fmap (\x -> tmpDir' `BS.append` x)
                          ["outputDir1"
                          ,"outputDir1/foo2"
                          ,"outputDir1/foo2/inputFile1"
                          ,"outputDir1/foo2/inputFile2"
                          ,"outputDir1/foo2/inputFile3"
                          ,"outputDir1/foo2/foo4"
                          ,"outputDir1/foo2/foo4/inputFile6"
                          ,"outputDir1/foo2/foo4/inputFile4"])
      deleteFile' "outputDir1/foo2/inputFile1"
      deleteFile' "outputDir1/foo2/inputFile2"
      deleteFile' "outputDir1/foo2/inputFile3"
      sort c `shouldBe` sort shouldC


    it "copyDirRecursive (Strict, CollectFailures), no write permission on output dir" $
      copyDirRecursive' "inputDir"
                        "noWritePerm/foo"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\(RecursiveFailure [(CreateDirFailed{}, e)]) -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive (Strict, CollectFailures), cannot open output dir" $
      copyDirRecursive' "inputDir"
                        "noPerms/foo"
                        Strict
                        CollectFailures
        `shouldThrow`
        isRecursiveFailure

    it "copyDirRecursive (Strict, CollectFailures), destination dir already exists" $
      copyDirRecursive' "inputDir"
                        "alreadyExistsD"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\(RecursiveFailure [(CreateDirFailed{}, e)]) -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive (Strict, CollectFailures), destination already exists and is a file" $
      copyDirRecursive' "inputDir"
                        "alreadyExists"
                        Strict
                        CollectFailures
        `shouldThrow`
        isRecursiveFailure

    it "copyDirRecursive (Strict, CollectFailures), wrong input (regular file)" $
      copyDirRecursive' "wrongInput"
                        "outputDir"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\(RecursiveFailure [(ReadContentsFailed{}, e)]) -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive (Strict, CollectFailures), wrong input (symlink to directory)" $
      copyDirRecursive' "wrongInputSymL"
                        "outputDir"
                        Strict
                        CollectFailures
        `shouldThrow`
        (\(RecursiveFailure [(ReadContentsFailed{}, e)]) -> ioeGetErrorType e == InvalidArgument)

    it "copyDirRecursive (Strict, CollectFailures), destination in source" $
      copyDirRecursive' "inputDir"
                        "inputDir/foo"
                        Strict
                        CollectFailures
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursive (Strict, CollectFailures), destination and source same directory" $
      copyDirRecursive' "inputDir"
                        "inputDir"
                        Strict
                        CollectFailures
        `shouldThrow`
        isSameFile


