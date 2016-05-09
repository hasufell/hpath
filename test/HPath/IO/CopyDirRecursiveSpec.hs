{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CopyDirRecursiveSpec where


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
specDir = "test/HPath/IO/copyDirRecursiveSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive, all fine" $ do
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "outputDir")
      removeDirIfExists (specDir `ba` "outputDir")

    it "copyDirRecursive, all fine and compare" $ do
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "outputDir")
      (system $ "diff -r --no-dereference "
                          ++ specDir' ++ "inputDir" ++ " "
                          ++ specDir' ++ "outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists (specDir `ba` "outputDir")

    -- posix failures --
    it "copyDirRecursive, source directory does not exist" $
      copyDirRecursive' (specDir `ba` "doesNotExist")
                        (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive, no write permission on output dir" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "noWritePerm/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open output dir" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open source dir" $
      copyDirRecursive' (specDir `ba` "noPerms/inputDir")
                        (specDir `ba` "foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, destination dir already exists" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "alreadyExistsD")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination already exists and is a file" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, wrong input (regular file)" $
      copyDirRecursive' (specDir `ba` "wrongInput")
                        (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive, wrong input (symlink to directory)" $
      copyDirRecursive' (specDir `ba` "wrongInputSymL")
                        (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursive, destination in source" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "inputDir/foo")
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursive, destination and source same directory" $
      copyDirRecursive' (specDir `ba` "inputDir")
                        (specDir `ba` "inputDir")
        `shouldThrow`
        isSameFile
