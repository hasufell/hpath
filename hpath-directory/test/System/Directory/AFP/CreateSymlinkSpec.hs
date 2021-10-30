{-# LANGUAGE OverloadedStrings #-}

module System.Directory.AFP.CreateSymlinkSpec where


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
  setTmpDir "CreateSymlinkSpec"
  createTmpDir


setupFiles :: IO ()
setupFiles = do
  createRegularFile' "alreadyExists"
  createDir' "noPerms"
  createDir' "noWritePerms"
  noPerms "noPerms"
  noWritableDirPerms "noWritePerms"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerms"
  deleteFile' "alreadyExists"
  deleteDir' "noPerms"
  deleteDir' "noWritePerms"


spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.createSymlink" $ do

    -- successes --
    it "createSymlink, all fine" $ do
      createSymlink' "newSymL" "alreadyExists/"
      removeFileIfExists "newSymL"

    -- posix failures --
    it "createSymlink, parent directories do not exist" $
      createSymlink' "some/thing/dada" "lala"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "createSymlink, can't write to destination directory" $
      createSymlink' "noWritePerms/newDir" "lala"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createSymlink, can't write to destination directory" $
      createSymlink' "noPerms/newDir" "lala"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createSymlink, destination file already exists" $
      createSymlink' "alreadyExists" "lala"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

