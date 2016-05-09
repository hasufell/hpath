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


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/copyFileOverwriteSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.copyFileOverwrite" $ do

    -- successes --
    it "copyFileOverwrite, everything clear" $ do
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "outputFile")
      removeFileIfExists (specDir `ba` "outputFile")

    it "copyFileOverwrite, output file already exists, all clear" $ do
      copyFile' (specDir `ba` "alreadyExists") (specDir `ba` "alreadyExists.bak")
      copyFileOverwrite' (specDir `ba` "inputFile")
                         (specDir `ba` "alreadyExists")
      (system $ "cmp -s " ++ specDir' ++ "inputFile" ++ " "
                          ++ specDir' ++ "alreadyExists")
        `shouldReturn` ExitSuccess
      removeFileIfExists (specDir `ba` "alreadyExists")
      copyFile' (specDir `ba` "alreadyExists.bak") (specDir `ba` "alreadyExists")
      removeFileIfExists (specDir `ba` "alreadyExists.bak")

    it "copyFileOverwrite, and compare" $ do
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "outputFile")
      (system $ "cmp -s " ++ specDir' ++ "inputFile" ++ " "
                          ++ specDir' ++ "outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists (specDir `ba` "outputFile")

    -- posix failures --
    it "copyFileOverwrite, input file does not exist" $
      copyFileOverwrite' (specDir `ba` "noSuchFile")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFileOverwrite, no permission to write to output directory" $
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "outputDirNoWrite/outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open output directory" $
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "noPerms/outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open source directory" $
      copyFileOverwrite' (specDir `ba` "noPerms/inputFile")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, wrong input type (symlink)" $
      copyFileOverwrite' (specDir `ba` "inputFileSymL")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFileOverwrite, wrong input type (directory)" $
      copyFileOverwrite' (specDir `ba` "wrongInput")
                (specDir `ba` "outputFile")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFileOverwrite, output file already exists and is a dir" $
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    -- custom failures --
    it "copyFileOverwrite, output and input are same file" $
      copyFileOverwrite' (specDir `ba` "inputFile")
                (specDir `ba` "inputFile")
        `shouldThrow` isSameFile
