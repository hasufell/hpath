{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CreateRegularFileSpec where


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
specDir = "test/HPath/IO/createRegularFileSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.createRegularFile" $ do

    -- successes --
    it "createRegularFile, all fine" $ do
      createRegularFile' (specDir `ba` "newDir")
      removeFileIfExists (specDir `ba` "newDir")

    -- posix failures --
    it "createRegularFile, can't write to destination directory" $
      createRegularFile' (specDir `ba` "noWritePerms/newDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, can't write to destination directory" $
      createRegularFile' (specDir `ba` "noPerms/newDir")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, destination file already exists" $
      createRegularFile' (specDir `ba` "alreadyExists")
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

