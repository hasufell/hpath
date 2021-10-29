{-# LANGUAGE OverloadedStrings #-}


module Utils where


import Control.Applicative
  (
    (<$>)
  )
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
import "hpath-directory" System.Posix.PosixFilePath.Directory
import Prelude hiding (appendFile, readFile, writeFile)
import Data.Maybe
  (
    fromJust
  )
import System.IO.Unsafe
  (
    unsafePerformIO
  )
import qualified System.Posix.PosixFilePath.Directory.Traversals as DT
import Data.ByteString
  (
    ByteString
  )
import System.Posix.Files.PosixString
  (
    groupExecuteMode
  , groupReadMode
  , nullFileMode
  , otherExecuteMode
  , otherReadMode
  , ownerExecuteMode
  , ownerReadMode
  , setFileMode
  , unionFileModes
  )
import AFP.AbstractFilePath.Posix
import qualified AFP.AbstractFilePath.Posix as AFP

baseTmpDir :: IORef (Maybe PosixFilePath)
{-# NOINLINE baseTmpDir #-}
baseTmpDir = unsafePerformIO (newIORef Nothing)


tmpDir :: IORef (Maybe PosixFilePath)
{-# NOINLINE tmpDir #-}
tmpDir = unsafePerformIO (newIORef Nothing)



    -----------------
    --[ Utilities ]--
    -----------------


setTmpDir :: PosixFilePath -> IO ()
{-# NOINLINE setTmpDir #-}
setTmpDir bs = do
  tmp <- fromJust <$> readIORef baseTmpDir
  writeIORef tmpDir (Just (tmp AFP.</> bs))


createTmpDir :: IO ()
{-# NOINLINE createTmpDir #-}
createTmpDir = do
  tmp <- fromJust <$> readIORef tmpDir
  void $ createDir newDirPerms tmp


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


withRawTmpDir :: (PosixFilePath -> IO a) -> IO a
{-# NOINLINE withRawTmpDir #-}
withRawTmpDir f = do
  tmp <- fromJust <$> readIORef tmpDir
  f tmp


getRawTmpDir :: IO PosixFilePath
{-# NOINLINE getRawTmpDir #-}
getRawTmpDir = withRawTmpDir (return . packPlatformString . (++ [fromChar '/']) . unpackPlatformString)


withTmpDir :: PosixFilePath -> (PosixFilePath -> IO a) -> IO a
{-# NOINLINE withTmpDir #-}
withTmpDir ip f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p = tmp AFP.</> ip
  f p


withTmpDir' :: PosixFilePath
            -> PosixFilePath
            -> (PosixFilePath -> PosixFilePath -> IO a)
            -> IO a
{-# NOINLINE withTmpDir' #-}
withTmpDir' ip1 ip2 f = do
  tmp <- fromJust <$> readIORef tmpDir
  let p1 = tmp </> ip1
  let p2 = tmp </> ip2
  f p1 p2


removeFileIfExists :: PosixFilePath -> IO ()
{-# NOINLINE removeFileIfExists #-}
removeFileIfExists bs =
  withTmpDir bs $ \p -> whenM (doesFileExist p) (deleteFile p)


removeDirIfExists :: PosixFilePath -> IO ()
{-# NOINLINE removeDirIfExists #-}
removeDirIfExists bs =
  withTmpDir bs $ \p -> whenM (doesDirectoryExist p) (deleteDirRecursive p)


copyFile' :: PosixFilePath -> PosixFilePath -> CopyMode -> IO ()
{-# NOINLINE copyFile' #-}
copyFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> copyFile p1 p2 cm)


copyDirRecursive' :: PosixFilePath -> PosixFilePath
                  -> CopyMode -> RecursiveErrorMode -> IO ()
{-# NOINLINE copyDirRecursive' #-}
copyDirRecursive' inputDirP outputDirP cm rm =
  withTmpDir' inputDirP outputDirP (\p1 p2 -> copyDirRecursive p1 p2 cm rm)


createDir' :: PosixFilePath -> IO ()
{-# NOINLINE createDir' #-}
createDir' dest = withTmpDir dest (createDir newDirPerms)

createDirIfMissing' :: PosixFilePath -> IO ()
{-# NOINLINE createDirIfMissing' #-}
createDirIfMissing' dest = withTmpDir dest (createDirIfMissing newDirPerms)

createDirRecursive' :: PosixFilePath -> IO ()
{-# NOINLINE createDirRecursive' #-}
createDirRecursive' dest = withTmpDir dest (createDirRecursive newDirPerms)

createRegularFile' :: PosixFilePath -> IO ()
{-# NOINLINE createRegularFile' #-}
createRegularFile' dest = withTmpDir dest (createRegularFile newFilePerms)


createSymlink' :: PosixFilePath -> PosixFilePath -> IO ()
{-# NOINLINE createSymlink' #-}
createSymlink' dest sympoint = withTmpDir dest
  (\x -> createSymlink x sympoint)


renameFile' :: PosixFilePath -> PosixFilePath -> IO ()
{-# NOINLINE renameFile' #-}
renameFile' inputFileP outputFileP =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    renameFile i o
    renameFile o i


moveFile' :: PosixFilePath -> PosixFilePath -> CopyMode -> IO ()
{-# NOINLINE moveFile' #-}
moveFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    moveFile i o cm
    moveFile o i Strict


recreateSymlink' :: PosixFilePath -> PosixFilePath -> CopyMode -> IO ()
{-# NOINLINE recreateSymlink' #-}
recreateSymlink' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> recreateSymlink p1 p2 cm)


noWritableDirPerms :: PosixFilePath -> IO ()
{-# NOINLINE noWritableDirPerms #-}
noWritableDirPerms path = withTmpDir path $ \p ->
  setFileMode p perms
  where
    perms =            ownerReadMode
      `unionFileModes` ownerExecuteMode
      `unionFileModes` groupReadMode
      `unionFileModes` groupExecuteMode
      `unionFileModes` otherReadMode
      `unionFileModes` otherExecuteMode


noPerms :: PosixFilePath -> IO ()
{-# NOINLINE noPerms #-}
noPerms path = withTmpDir path $ \p -> setFileMode p nullFileMode


normalDirPerms :: PosixFilePath -> IO ()
{-# NOINLINE normalDirPerms #-}
normalDirPerms path =
  withTmpDir path $ \p -> setFileMode p newDirPerms


normalFilePerms :: PosixFilePath -> IO ()
{-# NOINLINE normalFilePerms #-}
normalFilePerms path =
  withTmpDir path $ \p -> setFileMode p newFilePerms


getFileType' :: PosixFilePath -> IO FileType
{-# NOINLINE getFileType' #-}
getFileType' path = withTmpDir path getFileType


getDirsFiles' :: PosixFilePath -> IO [PosixFilePath]
{-# NOINLINE getDirsFiles' #-}
getDirsFiles' path = withTmpDir path getDirsFiles


deleteFile' :: PosixFilePath -> IO ()
{-# NOINLINE deleteFile' #-}
deleteFile' p = withTmpDir p deleteFile


deleteDir' :: PosixFilePath -> IO ()
{-# NOINLINE deleteDir' #-}
deleteDir' p = withTmpDir p deleteDir


deleteDirRecursive' :: PosixFilePath -> IO ()
{-# NOINLINE deleteDirRecursive' #-}
deleteDirRecursive' p = withTmpDir p deleteDirRecursive


canonicalizePath' :: PosixFilePath -> IO PosixFilePath
{-# NOINLINE canonicalizePath' #-}
canonicalizePath' p = withTmpDir p canonicalizePath


writeFile' :: PosixFilePath -> ByteString -> IO ()
{-# NOINLINE writeFile' #-}
writeFile' ip bs =
  withTmpDir ip $ \p -> writeFile p Nothing bs

writeFileL' :: PosixFilePath -> BSL.ByteString -> IO ()
{-# NOINLINE writeFileL' #-}
writeFileL' ip bs =
  withTmpDir ip $ \p -> writeFileL p Nothing bs


appendFile' :: PosixFilePath -> ByteString -> IO ()
{-# NOINLINE appendFile' #-}
appendFile' ip bs =
  withTmpDir ip $ \p -> appendFile p bs


allDirectoryContents' :: PosixFilePath -> IO [PosixFilePath]
{-# NOINLINE allDirectoryContents' #-}
allDirectoryContents' ip =
  withTmpDir ip $ \p -> DT.allDirectoryContents' p


readFile' :: PosixFilePath -> IO ByteString
{-# NOINLINE readFile' #-}
readFile' p = withTmpDir p readFileStrict


readFileL :: PosixFilePath -> IO BSL.ByteString
{-# NOINLINE readFileL #-}
readFileL p = withTmpDir p readFile
