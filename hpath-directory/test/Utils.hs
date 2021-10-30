{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Utils where


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception
import Data.Either
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
import qualified System.Posix.PosixFilePath.Directory.Traversals as DT
import System.Posix.Files.ByteString
  (
    getSymbolicLinkStatus
  )
#endif
import Data.ByteString
  (
    ByteString
  )
import AFP.AbstractFilePath
import AFP.OsString.Internal.Types
import qualified AFP.AbstractFilePath as AFP
import qualified Data.ByteString.Short as SBS

import System.Directory.AFP




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
getRawTmpDir = withRawTmpDir (return . packAFP . (++ [fromChar '/']) . unpackAFP)


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


createSymlink' :: AbstractFilePath -> AbstractFilePath -> IO ()
{-# NOINLINE createSymlink' #-}
createSymlink' dest sympoint = withTmpDir dest
  (\x -> createSymlink x sympoint)


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


getFileType' :: AbstractFilePath -> IO FileType
{-# NOINLINE getFileType' #-}
getFileType' path = withTmpDir path getFileType


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
  withTmpDir ip $ \p -> writeFile p True bs

writeFileL' :: AbstractFilePath -> BSL.ByteString -> IO ()
{-# NOINLINE writeFileL' #-}
writeFileL' ip bs =
  withTmpDir ip $ \p -> writeFileL p True bs


appendFile' :: AbstractFilePath -> ByteString -> IO ()
{-# NOINLINE appendFile' #-}
appendFile' ip bs =
  withTmpDir ip $ \p -> appendFile p bs


allDirectoryContents' :: AbstractFilePath -> IO [AbstractFilePath]
{-# NOINLINE allDirectoryContents' #-}
allDirectoryContents' ip =
#ifdef WINDOWS
  -- TODO
  undefined
#else
  withTmpDir ip $ \(OsString p) -> fmap OsString <$> DT.allDirectoryContents' p
#endif


readFile' :: AbstractFilePath -> IO ByteString
{-# NOINLINE readFile' #-}
readFile' p = withTmpDir p readFileStrict


readFileL :: AbstractFilePath -> IO BSL.ByteString
{-# NOINLINE readFileL #-}
readFileL p = withTmpDir p readFile

dirExists :: AbstractFilePath -> IO Bool
{-# NOINLINE dirExists #-}
#ifdef WINDOWS
dirExists fp =
  -- TODO
  undefined
#else
dirExists (OsString (PS fp)) =
  fmap isRight $ try @SomeException $ getSymbolicLinkStatus (SBS.fromShort fp)
#endif

