{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CopyFileOverwriteSpec where


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
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.copyFileOverwrite" $ do

    -- successes --
    it "copyFileOverwrite, everything clear" $ do
      copyFileOverwrite' "inputFile"
                "outputFile"
      removeFileIfExists "outputFile"

    it "copyFileOverwrite, output file already exists, all clear" $ do
      copyFile' "alreadyExists" "alreadyExists.bak"
      copyFileOverwrite' "inputFile"
                         "alreadyExists"
      (system $ "cmp -s " ++ toString tmpDir ++ "inputFile" ++ " "
                          ++ toString tmpDir ++ "alreadyExists")
        `shouldReturn` ExitSuccess
      removeFileIfExists "alreadyExists"
      copyFile' "alreadyExists.bak" "alreadyExists"
      removeFileIfExists "alreadyExists.bak"

    it "copyFileOverwrite, and compare" $ do
      copyFileOverwrite' "inputFile"
                "outputFile"
      (system $ "cmp -s " ++ toString tmpDir ++ "inputFile" ++ " "
                          ++ toString tmpDir ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "outputFile"

    -- posix failures --
    it "copyFileOverwrite, input file does not exist" $
      copyFileOverwrite' "noSuchFile"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFileOverwrite, no permission to write to output directory" $
      copyFileOverwrite' "inputFile"
                "outputDirNoWrite/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open output directory" $
      copyFileOverwrite' "inputFile"
                "noPerms/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open source directory" $
      copyFileOverwrite' "noPerms/inputFile"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, wrong input type (symlink)" $
      copyFileOverwrite' "inputFileSymL"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFileOverwrite, wrong input type (directory)" $
      copyFileOverwrite' "wrongInput"
                "outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFileOverwrite, output file already exists and is a dir" $
      copyFileOverwrite' "inputFile"
                "alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    -- custom failures --
    it "copyFileOverwrite, output and input are same file" $
      copyFileOverwrite' "inputFile"
                "inputFile"
        `shouldThrow` isSameFile
