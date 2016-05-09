{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CopyDirRecursiveOverwriteSpec where


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
specDir = "test/HPath/IO/copyDirRecursiveOverwriteSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.copyDirRecursiveOverwrite" $ do

    -- successes --
    it "copyDirRecursiveOverwrite, all fine" $ do
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "outputDir")
      removeDirIfExists $ specDir `ba` "outputDir"

    it "copyDirRecursiveOverwrite, all fine and compare" $ do
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "outputDir")
      (system $ "diff -r --no-dereference "
                          ++ specDir' ++ "inputDir" ++ " "
                          ++ specDir' ++ "outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists $ specDir `ba` "outputDir"

    it "copyDirRecursiveOverwrite, destination dir already exists" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "alreadyExistsD")

    -- posix failures --
    it "copyDirRecursiveOverwrite, source directory does not exist" $
      copyDirRecursiveOverwrite' (specDir `ba` "doesNotExist")
                                 (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursiveOverwrite, no write permission on output dir" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "noWritePerm/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open output dir" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "noPerms/foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open source dir" $
      copyDirRecursiveOverwrite' (specDir `ba` "noPerms/inputDir")
                                 (specDir `ba` "foo")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, destination already exists and is a file" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (regular file)" $
      copyDirRecursiveOverwrite' (specDir `ba` "wrongInput")
                                 (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (symlink to directory)" $
      copyDirRecursiveOverwrite' (specDir `ba` "wrongInputSymL")
                                 (specDir `ba` "outputDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursiveOverwrite, destination in source" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "inputDir/foo")
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursiveOverwrite, destination and source same directory" $
      copyDirRecursiveOverwrite' (specDir `ba` "inputDir")
                                 (specDir `ba` "inputDir")
        `shouldThrow`
        isSameFile
