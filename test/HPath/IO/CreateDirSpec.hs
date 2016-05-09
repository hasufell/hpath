{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CreateDirSpec where


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
specDir = "test/HPath/IO/createDirSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.createDir" $ do

    -- successes --
    it "createDir, all fine" $ do
      createDir' (specDir `ba` "newDir")
      removeDirIfExists (specDir `ba` "newDir")

    -- posix failures --
    it "createDir, can't write to output directory" $
      createDir' (specDir `ba` "noWritePerms/newDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, can't open output directory" $
      createDir' (specDir `ba` "noPerms/newDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, destination directory already exists" $
      createDir' (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

