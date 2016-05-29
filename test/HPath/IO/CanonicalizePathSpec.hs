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




setupFiles :: IO ()
setupFiles = do
  createRegularFile' "file"
  createDir' "dir"
  createSymlink' "dirSym" "dir/"
  createSymlink' "brokenSym" "nothing"
  createSymlink' "fileSym" "file"

cleanupFiles :: IO ()
cleanupFiles = do
  deleteFile' "file"
  deleteDir' "dir"
  deleteFile' "dirSym"
  deleteFile' "brokenSym"
  deleteFile' "fileSym"


spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.canonicalizePath" $ do

    -- successes --
    it "canonicalizePath, all fine" $ do
      path <- withTmpDir "file" return
      canonicalizePath' "file"
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withTmpDir "dir" return
      canonicalizePath' "dir"
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withTmpDir "file" return
      canonicalizePath' "fileSym"
        `shouldReturn` path

    it "canonicalizePath, all fine" $ do
      path <- withTmpDir "dir" return
      canonicalizePath' "dirSym"
        `shouldReturn` path


    -- posix failures --
    it "canonicalizePath, broken symlink" $
      canonicalizePath' "brokenSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "canonicalizePath, file does not exist" $
      canonicalizePath' "nothingBlah"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

