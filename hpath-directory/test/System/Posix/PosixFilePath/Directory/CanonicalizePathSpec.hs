{-# LANGUAGE OverloadedStrings #-}

module System.Posix.PosixFilePath.Directory.CanonicalizePathSpec where


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




upTmpDir :: IO ()
upTmpDir = do
  setTmpDir "CanonicalizePathSpec"
  createTmpDir

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
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.canonicalizePath" $ do

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

