{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


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
import System.Posix.PosixFilePath.Directory
  (
    getFileType
  )
import System.AbstractFilePath.Posix (PosixFilePath)
#endif
import Data.ByteString
  (
    ByteString
  )
import System.AbstractFilePath
import System.OsString.Internal.Types
import qualified System.AbstractFilePath as AFP

import System.Directory.AbstractFilePath hiding ( getFileType )
import System.File.AbstractFilePath




baseTmpDir :: IORef (Maybe AbstractFilePath)
{-# NOINLINE baseTmpDir #-}
baseTmpDir = unsafePerformIO (newIORef Nothing)


tmpDir :: IORef (Maybe AbstractFilePath)
{-# NOINLINE tmpDir #-}
tmpDir = unsafePerformIO (newIORef Nothing)



    -----------------
    --[ Utilities ]--
    -----------------


setTmpDir :: AbstractFilePath -> IO ()
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


withRawTmpDir :: (AbstractFilePath -> IO a) -> IO a
{-# NOINLINE withRawTmpDir #-}
withRawTmpDir f = do
  tmp <- fromJust <$> readIORef tmpDir
  f tmp


getRawTmpDir :: IO AbstractFilePath
{-# NOINLINE getRawTmpDir #-}
getRawTmpDir = withRawTmpDir (return . packAFP . (++ [unsafeFromChar '/']) . unpackAFP)


withTmpDir :: AbstractFilePath -> (AbstractFilePath -> IO a) -> IO a
{-# NOINLINE withTmpDir #-}
withTmpDir ip f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p = tmp AFP.</> ip
  f p


withTmpDir' :: AbstractFilePath
            -> AbstractFilePath
            -> (AbstractFilePath -> AbstractFilePath -> IO a)
            -> IO a
{-# NOINLINE withTmpDir' #-}
withTmpDir' ip1 ip2 f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p1 = tmp </> ip1
  let p2 = tmp </> ip2
  f p1 p2


removeFileIfExists :: AbstractFilePath -> IO ()
{-# NOINLINE removeFileIfExists #-}
removeFileIfExists bs =
  withTmpDir bs $ \p -> whenM (doesFileExist p) (deleteFile p)


removeDirIfExists :: AbstractFilePath -> IO ()
{-# NOINLINE removeDirIfExists #-}
removeDirIfExists bs =
  withTmpDir bs $ \p -> whenM (doesDirectoryExist p) (deleteDirRecursive p)


copyFile' :: AbstractFilePath -> AbstractFilePath -> CopyMode -> IO ()
{-# NOINLINE copyFile' #-}
copyFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> copyFile p1 p2 cm)


copyDirRecursive' :: AbstractFilePath -> AbstractFilePath
                  -> CopyMode -> RecursiveErrorMode -> IO ()
{-# NOINLINE copyDirRecursive' #-}
copyDirRecursive' inputDirP outputDirP cm rm =
  withTmpDir' inputDirP outputDirP (\p1 p2 -> copyDirRecursive p1 p2 cm rm)


createDir' :: AbstractFilePath -> IO ()
{-# NOINLINE createDir' #-}
createDir' dest = withTmpDir dest createDir

createDirIfMissing' :: AbstractFilePath -> IO ()
{-# NOINLINE createDirIfMissing' #-}
createDirIfMissing' dest = withTmpDir dest createDirIfMissing

createDirRecursive' :: AbstractFilePath -> IO ()
{-# NOINLINE createDirRecursive' #-}
createDirRecursive' dest = withTmpDir dest createDirRecursive

createRegularFile' :: AbstractFilePath -> IO ()
{-# NOINLINE createRegularFile' #-}
createRegularFile' dest = withTmpDir dest createRegularFile


createSymlink' :: AbstractFilePath -> AbstractFilePath -> Bool -> IO ()
{-# NOINLINE createSymlink' #-}
createSymlink' dest sympoint b = withTmpDir dest
  (\x -> createSymlink x sympoint b)


renameFile' :: AbstractFilePath -> AbstractFilePath -> IO ()
{-# NOINLINE renameFile' #-}
renameFile' inputFileP outputFileP =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    renameFile i o
    renameFile o i


moveFile' :: AbstractFilePath -> AbstractFilePath -> CopyMode -> IO ()
{-# NOINLINE moveFile' #-}
moveFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    moveFile i o cm
    moveFile o i Strict


recreateSymlink' :: AbstractFilePath -> AbstractFilePath -> CopyMode -> IO ()
{-# NOINLINE recreateSymlink' #-}
recreateSymlink' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> recreateSymlink p1 p2 cm)


noWritableDirPerms :: AbstractFilePath -> IO ()
{-# NOINLINE noWritableDirPerms #-}
noWritableDirPerms path = withTmpDir path $ \p ->
  setPermissions p (setOwnerWritable False newDirPerms)


noPerms :: AbstractFilePath -> IO ()
{-# NOINLINE noPerms #-}
noPerms path = withTmpDir path $ \p ->
  setPermissions p emptyPermissions


normalDirPerms :: AbstractFilePath -> IO ()
{-# NOINLINE normalDirPerms #-}
normalDirPerms path =
  withTmpDir path $ \p ->
    setPermissions p newDirPerms


normalFilePerms :: AbstractFilePath -> IO ()
{-# NOINLINE normalFilePerms #-}
normalFilePerms path =
  withTmpDir path $ \p ->
    setPermissions p newFilePerms

#ifndef WINDOWS
getFileType' :: PosixFilePath -> IO FileType
{-# NOINLINE getFileType' #-}
getFileType' path = withTmpDir (OsString path) $ \(OsString p) -> getFileType p
#endif


getDirsFiles' :: AbstractFilePath -> IO [AbstractFilePath]
{-# NOINLINE getDirsFiles' #-}
getDirsFiles' path = withTmpDir path getDirsFiles


deleteFile' :: AbstractFilePath -> IO ()
{-# NOINLINE deleteFile' #-}
deleteFile' p = withTmpDir p deleteFile


deleteDir' :: AbstractFilePath -> IO ()
{-# NOINLINE deleteDir' #-}
deleteDir' p = withTmpDir p deleteDir


deleteDirRecursive' :: AbstractFilePath -> IO ()
{-# NOINLINE deleteDirRecursive' #-}
deleteDirRecursive' p = withTmpDir p deleteDirRecursive


canonicalizePath' :: AbstractFilePath -> IO AbstractFilePath
{-# NOINLINE canonicalizePath' #-}
canonicalizePath' p = withTmpDir p canonicalizePath


writeFile' :: AbstractFilePath -> ByteString -> IO ()
{-# NOINLINE writeFile' #-}
writeFile' ip bs =
  withTmpDir ip $ \p -> System.File.AbstractFilePath.writeFile' p bs

writeFileL' :: AbstractFilePath -> BSL.ByteString -> IO ()
{-# NOINLINE writeFileL' #-}
writeFileL' ip bs =
  withTmpDir ip $ \p -> writeFile p bs

writeExistingFile' :: AbstractFilePath -> ByteString -> IO ()
{-# NOINLINE writeExistingFile' #-}
writeExistingFile' ip bs =
  withTmpDir ip $ \p -> System.Directory.AbstractFilePath.writeExistingFile' p bs

writeExistingFileL' :: AbstractFilePath -> BSL.ByteString -> IO ()
{-# NOINLINE writeExistingFileL' #-}
writeExistingFileL' ip bs =
  withTmpDir ip $ \p -> writeExistingFile p bs

appendFile' :: AbstractFilePath -> ByteString -> IO ()
{-# NOINLINE appendFile' #-}
appendFile' ip bs =
  withTmpDir ip $ \p -> System.File.AbstractFilePath.appendFile' p bs

appendExistingFile' :: AbstractFilePath -> ByteString -> IO ()
{-# NOINLINE appendExistingFile' #-}
appendExistingFile' ip bs =
  withTmpDir ip $ \p -> System.Directory.AbstractFilePath.appendExistingFile' p bs


{-# NOINLINE allDirectoryContents' #-}
allDirectoryContents' :: AbstractFilePath -> IO [AbstractFilePath]
allDirectoryContents' ip =
  withTmpDir ip $ \p -> getDirsFilesRec p


readFile' :: AbstractFilePath -> IO ByteString
{-# NOINLINE readFile' #-}
readFile' p = withTmpDir p System.File.AbstractFilePath.readFile'

readExistingFile' :: AbstractFilePath -> IO ByteString
{-# NOINLINE readExistingFile' #-}
readExistingFile' p = withTmpDir p System.Directory.AbstractFilePath.readExistingFile'

readFileL :: AbstractFilePath -> IO BSL.ByteString
{-# NOINLINE readFileL #-}
readFileL p = withTmpDir p readFile

readExistingFileL :: AbstractFilePath -> IO BSL.ByteString
{-# NOINLINE readExistingFileL #-}
readExistingFileL p = withTmpDir p System.Directory.AbstractFilePath.readExistingFile

dirExists :: AbstractFilePath -> IO Bool
{-# NOINLINE dirExists #-}
dirExists fp = doesDirectoryExist fp

