{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Utils where


import Control.Monad
  (
    forM_
  , void
  )
import Control.Monad.IfElse
  (
    whenM
  )
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
  (
    newIORef
  , readIORef
  , writeIORef
  , IORef
  )
import Prelude hiding (appendFile, readFile, writeFile)
import Data.Maybe
  (
    fromJust
  )
import System.IO.Unsafe
  (
    unsafePerformIO
  )
#ifdef WINDOWS
#else
import System.Posix.PosixPath.Directory
  (
    getFileType
  )
import System.OsPath.Posix (PosixPath)
#endif
import Data.ByteString
  (
    ByteString
  )
import System.OsPath
import System.OsString.Internal.Types
import qualified System.OsPath as AFP
import qualified System.OsPath.Posix as P

import System.Directory.OsPath hiding ( getFileType )
import System.File.OsPath
import Data.String (IsString (fromString))


instance IsString OsString where
  fromString = either (error . show) id . AFP.encodeUtf

instance IsString PosixString where
  fromString = either (error . show) id . P.encodeUtf

baseTmpDir :: IORef (Maybe OsPath)
{-# NOINLINE baseTmpDir #-}
baseTmpDir = unsafePerformIO (newIORef Nothing)


tmpDir :: IORef (Maybe OsPath)
{-# NOINLINE tmpDir #-}
tmpDir = unsafePerformIO (newIORef Nothing)



    -----------------
    --[ Utilities ]--
    -----------------


setTmpDir :: OsPath -> IO ()
{-# NOINLINE setTmpDir #-}
setTmpDir bs = do
  tmp <- fromJust <$> readIORef baseTmpDir
  writeIORef tmpDir (Just (tmp AFP.</> bs))


createTmpDir :: IO ()
{-# NOINLINE createTmpDir #-}
createTmpDir = do
  tmp <- fromJust <$> readIORef tmpDir
  void $ createDir tmp


deleteTmpDir :: IO ()
{-# NOINLINE deleteTmpDir #-}
deleteTmpDir = do
  tmp <- fromJust <$> readIORef tmpDir
  void $ deleteDir tmp


deleteBaseTmpDir :: IO ()
{-# NOINLINE deleteBaseTmpDir #-}
deleteBaseTmpDir = do
  tmp <- fromJust <$> readIORef baseTmpDir
  contents <- getDirsFiles tmp
  forM_ contents deleteDir
  void $ deleteDir tmp


withRawTmpDir :: (OsPath -> IO a) -> IO a
{-# NOINLINE withRawTmpDir #-}
withRawTmpDir f = do
  tmp <- fromJust <$> readIORef tmpDir
  f tmp


getRawTmpDir :: IO OsPath
{-# NOINLINE getRawTmpDir #-}
getRawTmpDir = withRawTmpDir (return . pack . (++ [unsafeFromChar '/']) . unpack)


withTmpDir :: OsPath -> (OsPath -> IO a) -> IO a
{-# NOINLINE withTmpDir #-}
withTmpDir ip f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p = tmp AFP.</> ip
  f p


withTmpDir' :: OsPath
            -> OsPath
            -> (OsPath -> OsPath -> IO a)
            -> IO a
{-# NOINLINE withTmpDir' #-}
withTmpDir' ip1 ip2 f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p1 = tmp </> ip1
  let p2 = tmp </> ip2
  f p1 p2


removeFileIfExists :: OsPath -> IO ()
{-# NOINLINE removeFileIfExists #-}
removeFileIfExists bs =
  withTmpDir bs $ \p -> whenM (doesFileExist p) (deleteFile p)


removeDirIfExists :: OsPath -> IO ()
{-# NOINLINE removeDirIfExists #-}
removeDirIfExists bs =
  withTmpDir bs $ \p -> whenM (doesDirectoryExist p) (deleteDirRecursive p)


copyFile' :: OsPath -> OsPath -> CopyMode -> IO ()
{-# NOINLINE copyFile' #-}
copyFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> copyFile p1 p2 cm)


copyDirRecursive' :: OsPath -> OsPath
                  -> CopyMode -> RecursiveErrorMode -> IO ()
{-# NOINLINE copyDirRecursive' #-}
copyDirRecursive' inputDirP outputDirP cm rm =
  withTmpDir' inputDirP outputDirP (\p1 p2 -> copyDirRecursive p1 p2 cm rm)


createDir' :: OsPath -> IO ()
{-# NOINLINE createDir' #-}
createDir' dest = withTmpDir dest createDir

createDirIfMissing' :: OsPath -> IO ()
{-# NOINLINE createDirIfMissing' #-}
createDirIfMissing' dest = withTmpDir dest createDirIfMissing

createDirRecursive' :: OsPath -> IO ()
{-# NOINLINE createDirRecursive' #-}
createDirRecursive' dest = withTmpDir dest createDirRecursive

createRegularFile' :: OsPath -> IO ()
{-# NOINLINE createRegularFile' #-}
createRegularFile' dest = withTmpDir dest createRegularFile


createSymlink' :: OsPath -> OsPath -> Bool -> IO ()
{-# NOINLINE createSymlink' #-}
createSymlink' dest sympoint b = withTmpDir dest
  (\x -> createSymlink x sympoint b)


renameFile' :: OsPath -> OsPath -> IO ()
{-# NOINLINE renameFile' #-}
renameFile' inputFileP outputFileP =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    renameFile i o
    renameFile o i


moveFile' :: OsPath -> OsPath -> CopyMode -> IO ()
{-# NOINLINE moveFile' #-}
moveFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    moveFile i o cm
    moveFile o i Strict


recreateSymlink' :: OsPath -> OsPath -> CopyMode -> IO ()
{-# NOINLINE recreateSymlink' #-}
recreateSymlink' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> recreateSymlink p1 p2 cm)


noWritableDirPerms :: OsPath -> IO ()
{-# NOINLINE noWritableDirPerms #-}
noWritableDirPerms path = withTmpDir path $ \p ->
  setPermissions p (setOwnerWritable False newDirPerms)


noPerms :: OsPath -> IO ()
{-# NOINLINE noPerms #-}
noPerms path = withTmpDir path $ \p ->
  setPermissions p emptyPermissions


normalDirPerms :: OsPath -> IO ()
{-# NOINLINE normalDirPerms #-}
normalDirPerms path =
  withTmpDir path $ \p ->
    setPermissions p newDirPerms


normalFilePerms :: OsPath -> IO ()
{-# NOINLINE normalFilePerms #-}
normalFilePerms path =
  withTmpDir path $ \p ->
    setPermissions p newFilePerms

#ifndef WINDOWS
getFileType' :: PosixPath -> IO FileType
{-# NOINLINE getFileType' #-}
getFileType' path = withTmpDir (OsString path) $ \(OsString p) -> getFileType p
#endif


getDirsFiles' :: OsPath -> IO [OsPath]
{-# NOINLINE getDirsFiles' #-}
getDirsFiles' path = withTmpDir path getDirsFiles


deleteFile' :: OsPath -> IO ()
{-# NOINLINE deleteFile' #-}
deleteFile' p = withTmpDir p deleteFile


deleteDir' :: OsPath -> IO ()
{-# NOINLINE deleteDir' #-}
deleteDir' p = withTmpDir p deleteDir


deleteDirRecursive' :: OsPath -> IO ()
{-# NOINLINE deleteDirRecursive' #-}
deleteDirRecursive' p = withTmpDir p deleteDirRecursive


canonicalizePath' :: OsPath -> IO OsPath
{-# NOINLINE canonicalizePath' #-}
canonicalizePath' p = withTmpDir p canonicalizePath


writeFile' :: OsPath -> ByteString -> IO ()
{-# NOINLINE writeFile' #-}
writeFile' ip bs =
  withTmpDir ip $ \p -> System.File.OsPath.writeFile' p bs

writeFileL' :: OsPath -> BSL.ByteString -> IO ()
{-# NOINLINE writeFileL' #-}
writeFileL' ip bs =
  withTmpDir ip $ \p -> writeFile p bs

writeExistingFile' :: OsPath -> ByteString -> IO ()
{-# NOINLINE writeExistingFile' #-}
writeExistingFile' ip bs =
  withTmpDir ip $ \p -> System.Directory.OsPath.writeExistingFile' p bs

writeExistingFileL' :: OsPath -> BSL.ByteString -> IO ()
{-# NOINLINE writeExistingFileL' #-}
writeExistingFileL' ip bs =
  withTmpDir ip $ \p -> writeExistingFile p bs

appendFile' :: OsPath -> ByteString -> IO ()
{-# NOINLINE appendFile' #-}
appendFile' ip bs =
  withTmpDir ip $ \p -> System.File.OsPath.appendFile' p bs

appendExistingFile' :: OsPath -> ByteString -> IO ()
{-# NOINLINE appendExistingFile' #-}
appendExistingFile' ip bs =
  withTmpDir ip $ \p -> System.Directory.OsPath.appendExistingFile' p bs


{-# NOINLINE allDirectoryContents' #-}
allDirectoryContents' :: OsPath -> IO [OsPath]
allDirectoryContents' ip =
  withTmpDir ip $ \p -> getDirsFilesRec p


readFile' :: OsPath -> IO ByteString
{-# NOINLINE readFile' #-}
readFile' p = withTmpDir p System.File.OsPath.readFile'

readExistingFile' :: OsPath -> IO ByteString
{-# NOINLINE readExistingFile' #-}
readExistingFile' p = withTmpDir p System.Directory.OsPath.readExistingFile'

readFileL :: OsPath -> IO BSL.ByteString
{-# NOINLINE readFileL #-}
readFileL p = withTmpDir p readFile

readExistingFileL :: OsPath -> IO BSL.ByteString
{-# NOINLINE readExistingFileL #-}
readExistingFileL p = withTmpDir p System.Directory.OsPath.readExistingFile

dirExists :: OsPath -> IO Bool
{-# NOINLINE dirExists #-}
dirExists fp = doesDirectoryExist fp

