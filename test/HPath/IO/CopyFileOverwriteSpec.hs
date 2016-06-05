{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CopyFileOverwriteSpec where


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
  setTmpDir "CopyFileOverwriteSpec"
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
  writeFile' "alreadyExists" "dsaldsalkaklsdlkasksdadasl"


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
    it "copyFile (Overwrite), everything clear" $ do
      copyFile' "inputFile"
                "outputFile"
                Overwrite
      removeFileIfExists "outputFile"

    it "copyFile (Overwrite), output file already exists, all clear" $ do
      tmpDir' <- getRawTmpDir
      copyFile' "alreadyExists" "alreadyExists.bak" Strict
      copyFile' "inputFile" "alreadyExists" Overwrite
      (system $ "cmp -s " ++ toString tmpDir' ++ "inputFile" ++ " "
                          ++ toString tmpDir' ++ "alreadyExists")
        `shouldReturn` ExitSuccess
      removeFileIfExists "alreadyExists"
      copyFile' "alreadyExists.bak" "alreadyExists" Strict
      removeFileIfExists "alreadyExists.bak"

    it "copyFile (Overwrite), and compare" $ do
      tmpDir' <- getRawTmpDir
      copyFile' "inputFile"
                "outputFile"
                Overwrite
      (system $ "cmp -s " ++ toString tmpDir' ++ "inputFile" ++ " "
                          ++ toString tmpDir' ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "outputFile"


    -- posix failures --
    it "copyFile (Overwrite), input file does not exist" $
      copyFile' "noSuchFile"
                "outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile (Overwrite), no permission to write to output directory" $
      copyFile' "inputFile"
                "outputDirNoWrite/outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Overwrite), cannot open output directory" $
      copyFile' "inputFile"
                "noPerms/outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Overwrite), cannot open source directory" $
      copyFile' "noPerms/inputFile"
                "outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile (Overwrite), wrong input type (symlink)" $
      copyFile' "inputFileSymL"
                "outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile (Overwrite), wrong input type (directory)" $
      copyFile' "wrongInput"
                "outputFile"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile (Overwrite), output file already exists and is a dir" $
      copyFile' "inputFile"
                "alreadyExistsD"
                Overwrite
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    -- custom failures --
    it "copyFile (Overwrite), output and input are same file" $
      copyFile' "inputFile"
                "inputFile"
                Overwrite
        `shouldThrow` isSameFile
