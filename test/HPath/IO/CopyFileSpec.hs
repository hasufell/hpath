{-# LANGUAGE OverloadedStrings #-}


module HPath.IO.CopyFileSpec where


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
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.copyFile" $ do

    -- successes --
    it "copyFile, everything clear" $ do
      copyFile' "inputFile"
                "outputFile"
      removeFileIfExists "outputFile"

    it "copyFile, and compare" $ do
      copyFile' "inputFile"
                "outputFile"
      (system $ "cmp -s " ++ toString tmpDir ++ "inputFile" ++ " "
                          ++ toString tmpDir ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "outputFile"

    -- posix failures --
    it "copyFile, input file does not exist" $
      copyFile' "noSuchFile"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile, no permission to write to output directory" $
      copyFile' "inputFile"
                "outputDirNoWrite/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open output directory" $
      copyFile' "inputFile"
                "noPerms/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open source directory" $
      copyFile' "noPerms/inputFile"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, wrong input type (symlink)" $
      copyFile' "inputFileSymL"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile, wrong input type (directory)" $
      copyFile' "wrongInput"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile, output file already exists" $
      copyFile' "inputFile"
                "alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists and is a dir" $
      copyFile' "inputFile"
                "alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    -- custom failures --
    it "copyFile, output and input are same file" $
      copyFile' "inputFile"
                "inputFile"
        `shouldThrow`
        isSameFile
