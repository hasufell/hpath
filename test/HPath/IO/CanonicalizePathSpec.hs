{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.CanonicalizePathSpec where


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
specDir = "test/HPath/IO/canonicalizePathSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.canonicalizePath" $ do

    -- successes --
    it "canonicalizePath, all fine" $ do
      path <- withPwd (specDir `ba` "file") return
      canonicalizePath' (specDir `ba` "file")
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withPwd (specDir `ba` "dir") return
      canonicalizePath' (specDir `ba` "dir")
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withPwd (specDir `ba` "file") return
      canonicalizePath' (specDir `ba` "fileSym")
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withPwd (specDir `ba` "dir") return
      canonicalizePath' (specDir `ba` "dirSym")
        `shouldReturn` path


    -- posix failures --
    it "canonicalizePath, broken symlink" $
      canonicalizePath' (specDir `ba` "brokenSym")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "canonicalizePath, file does not exist" $
      canonicalizePath' (specDir `ba` "nothingBlah")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

