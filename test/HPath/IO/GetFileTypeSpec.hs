{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.GetFileTypeSpec where


import HPath.IO
import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/getFileTypeSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.getFileType" $ do

    -- successes --
    it "getFileType, regular file" $
      getFileType' (specDir `ba` "regularfile")
        `shouldReturn` RegularFile

    it "getFileType, directory" $
      getFileType' (specDir `ba` "directory")
        `shouldReturn` Directory

    it "getFileType, directory with null permissions" $
      getFileType' (specDir `ba` "noPerms")
        `shouldReturn` Directory

    it "getFileType, symlink to file" $
      getFileType' (specDir `ba` "symlink")
        `shouldReturn` SymbolicLink

    it "getFileType, symlink to directory" $
      getFileType' (specDir `ba` "symlinkD")
        `shouldReturn` SymbolicLink

    it "getFileType, broken symlink" $
      getFileType' (specDir `ba` "brokenSymlink")
        `shouldReturn` SymbolicLink

    -- posix failures --
    it "getFileType, file does not exist" $
      getFileType' (specDir `ba` "nothingHere")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getFileType, can't open directory" $
      getFileType' (specDir `ba` "noPerms/forz")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

