{-# LANGUAGE OverloadedStrings #-}

module System.Directory.AFP.CopyFileOverwriteSpec where


import Test.Hspec
import "hpath-directory" System.Directory.AFP
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
  setTmpDir "CopyFileOverwriteSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "inputFile"
  createRegularFile' "alreadyExists"
  createSymlink' "inputFileSymL" "inputFile" False
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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.copyFile" $ do

    -- successes --
    it "copyFile (Overwrite), everything clear" $ do
      copyFile' "inputFile"
                "outputFile"
                Overwrite
      removeFileIfExists "outputFile"

    it "copyFile (Overwrite), output file already exists, all clear" $ do
      tmpDir' <- getRawTmpDir
      tmpDirS <- fromAbstractFilePathIO tmpDir'
      copyFile' "alreadyExists" "alreadyExists.bak" Strict
      copyFile' "inputFile" "alreadyExists" Overwrite
      (system $ "cmp -s " ++ tmpDirS ++ "inputFile" ++ " "
                          ++ tmpDirS ++ "alreadyExists")
        `shouldReturn` ExitSuccess
      removeFileIfExists "alreadyExists"
      copyFile' "alreadyExists.bak" "alreadyExists" Strict
      removeFileIfExists "alreadyExists.bak"

    it "copyFile (Overwrite), and compare" $ do
      tmpDir' <- getRawTmpDir
      tmpDirS <- fromAbstractFilePathIO tmpDir'
      copyFile' "inputFile"
                "outputFile"
                Overwrite
      (system $ "cmp -s " ++ tmpDirS ++ "inputFile" ++ " "
                          ++ tmpDirS ++ "outputFile")
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
        `shouldThrow`
        (\e -> case e of
                SameFile{} -> True
                _          -> False)
