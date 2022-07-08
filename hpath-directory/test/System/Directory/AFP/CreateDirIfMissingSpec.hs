{-# LANGUAGE OverloadedStrings #-}

module System.Directory.AFP.CreateDirIfMissingSpec where


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
  setTmpDir "CreateDirIfMissingSpec"
  createTmpDir

setupFiles :: IO ()
setupFiles = do
  createDir' "alreadyExists"
  createDir' "noPerms"
  createDir' "noWritePerms"
  noPerms "noPerms"
  noWritableDirPerms "noWritePerms"



cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  normalDirPerms "noWritePerms"
  deleteDir' "alreadyExists"
  deleteDir' "noPerms"
  deleteDir' "noWritePerms"


spec :: Spec
spec = beforeAll_ (upTmpDir >> setupFiles) $ afterAll_ cleanupFiles $
  describe "System.Posix.PosixFilePath.Directory.CreateDirIfMissing" $ do

    -- successes --
    it "createDirIfMissing, all fine" $ do
      createDirIfMissing' "newDir"
      removeDirIfExists "newDir"

    it "createDirIfMissing, destination directory already exists" $
      createDirIfMissing' "alreadyExists"

    -- posix failures --
    it "createDirIfMissing, parent directories do not exist" $
      createDirIfMissing' "some/thing/dada"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "createDirIfMissing, can't write to output directory" $
      createDirIfMissing' "noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDirIfMissing, can't open output directory" $
      createDirIfMissing' "noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)
