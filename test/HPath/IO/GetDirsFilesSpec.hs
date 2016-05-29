{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.GetDirsFilesSpec where


import Control.Applicative
  (
    (<$>)
  )
import Data.List
  (
    sort
  )
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
import HPath.IO
import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import System.Posix.Env.ByteString
  (
    getEnv
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
  createRegularFile' "Lala"
  createRegularFile' ".hidden"
  createSymlink' "syml" "Lala"
  createDir' "dir"
  createSymlink' "dirsym" "dir"
  createDir' "noPerms"
  noPerms "noPerms"


cleanupFiles :: IO ()
cleanupFiles = do
  normalDirPerms "noPerms"
  deleteFile' "file"
  deleteFile' "Lala"
  deleteFile' ".hidden"
  deleteFile' "syml"
  deleteDir' "dir"
  deleteFile' "dirsym"
  deleteDir' "noPerms"



spec :: Spec
spec = before_ setupFiles $ after_ cleanupFiles $
  describe "HPath.IO.getDirsFiles" $ do

    -- successes --
    it "getDirsFiles, all fine" $
      withRawTmpDir $ \p -> do
      expectedFiles <- mapM P.parseRel [".hidden"
                                       ,"Lala"
                                       ,"dir"
                                       ,"dirsym"
                                       ,"file"
                                       ,"noPerms"
                                       ,"syml"]
      (fmap sort $ getDirsFiles p)
        `shouldReturn` fmap (p P.</>) expectedFiles

    -- posix failures --
    it "getDirsFiles, nonexistent directory" $
      getDirsFiles' "nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getDirsFiles, wrong file type (file)" $
      getDirsFiles' "file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "getDirsFiles, wrong file type (symlink to file)" $
      getDirsFiles' "syml"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, wrong file type (symlink to dir)" $
      getDirsFiles' "dirsym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, can't open directory" $
      getDirsFiles' "noPerms"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)




