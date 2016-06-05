{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.CopyFileSpec where


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
import System.Exit
import System.Process
import Utils
import Data.ByteString.UTF8 (toString)
import Data.IORef
  (
    readIORef
  )


upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "CopyFileSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createRegularFile' "inputFile"
  createRegularFile' "alreadyExists"
  createSymlink' "inputFileSymL" "inputFile"
  createDir' "alreadyExistsD"
  createDir' "noPerms"
  createRegularFile' "noPerms/inputFile"
  createDir' "outputDirNoWrite"
  createDir' "wrongInput"
  noPerms "noPerms"
  noWritableDirPerms "outputDirNoWrite"
  writeFile' "inputFile" "Blahfaselgagaga"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "outputDirNoWrite"
  deleteFile' "noPerms/inputFile"
  deleteFile' "inputFile"
  deleteFile' "alreadyExists"
  deleteFile' "inputFileSymL"
  deleteDir' "alreadyExistsD"
  deleteDir' "noPerms"
  deleteDir' "outputDirNoWrite"
  deleteDir' "wrongInput"


spec :: Spec
spec = beforeAll_ upTmpDir $ before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.copyFile" $ do

    -- successes --
    it "copyFile (Strict), everything clear" $ do
      copyFile' "inputFile"
                "outputFile"
                Strict
      removeFileIfExists "outputFile"

    it "copyFile (Strict), and compare" $ do
      tmpDir' <- getRawTmpDir
      copyFile' "inputFile"
                "outputFile"
                Strict
      (system $ "cmp -s " ++ toString tmpDir' ++ "inputFile" ++ " "
                          ++ toString tmpDir' ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "outputFile"

    -- posix failures --
    it "copyFile (Strict), input file does not exist" $
      copyFile' "noSuchFile"
                "outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile (Strict), no permission to write to output directory" $
      copyFile' "inputFile"
                "outputDirNoWrite/outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Strict), cannot open output directory" $
      copyFile' "inputFile"
                "noPerms/outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Strict), cannot open source directory" $
      copyFile' "noPerms/inputFile"
                "outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Strict), wrong input type (symlink)" $
      copyFile' "inputFileSymL"
                "outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile (Strict), wrong input type (directory)" $
      copyFile' "wrongInput"
                "outputFile"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile (Strict), output file already exists" $
      copyFile' "inputFile"
                "alreadyExists"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile (Strict), output file already exists and is a dir" $
      copyFile' "inputFile"
                "alreadyExistsD"
                Strict
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    -- custom failures --
    it "copyFile (Strict), output and input are same file" $
      copyFile' "inputFile"
                "inputFile"
                Strict
        `shouldThrow`
        isSameFile
