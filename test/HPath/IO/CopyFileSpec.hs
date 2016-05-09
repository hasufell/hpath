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


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/copyFileSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.copyFile" $ do

    -- successes --
    it "copyFile, everything clear" $ do
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "outputFile")
      removeFileIfExists (specDir `ba` "outputFile")

    it "copyFile, and compare" $ do
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "outputFile")
      (system $ "cmp -s " ++ specDir' ++ "inputFile" ++ " "
                          ++ specDir' ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists (specDir `ba` "outputFile")

    -- posix failures --
    it "copyFile, input file does not exist" $
      copyFile' (specDir `ba` "noSuchFile")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile, no permission to write to output directory" $
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "outputDirNoWrite/outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open output directory" $
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "noPerms/outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open source directory" $
      copyFile' (specDir `ba` "noPerms/inputFile")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, wrong input type (symlink)" $
      copyFile' (specDir `ba` "inputFileSymL")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile, wrong input type (directory)" $
      copyFile' (specDir `ba` "wrongInput")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile, output file already exists" $
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists and is a dir" $
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    -- custom failures --
    it "copyFile, output and input are same file" $
      copyFile' (specDir `ba` "inputFile")
                (specDir `ba` "inputFile")
        `shouldThrow`
        isSameFile
