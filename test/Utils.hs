{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}


module Utils where


import Control.Applicative
  (
    (<$>)
  )
import Control.Monad
  (
    void
  )
import HPath.IO
import HPath.IO.Errors
import HPath.IO.Utils
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
import System.Posix.Directory.Traversals
  (
    allDirectoryContents
  )
import System.Posix.Env.ByteString
  (
    getEnv
  )
import Data.ByteString
  (
    ByteString
  )
import System.Posix.Files.ByteString
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

import qualified "unix" System.Posix.IO.ByteString as SPI
import qualified "unix-bytestring" System.Posix.IO.ByteString as SPB



tmpDir :: ByteString
tmpDir = "test/HPath/IO/tmp/"



    -----------------
    --[ Utilities ]--
    -----------------


createTmpDir :: IO ()
createTmpDir = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  tmp <- P.parseRel tmpDir
  void $ createDir (pwd P.</> tmp)


deleteTmpDir :: IO ()
deleteTmpDir = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  tmp <- P.parseRel tmpDir
  void $ deleteDir (pwd P.</> tmp)


withRawTmpDir :: (P.Path P.Abs -> IO a) -> IO a
withRawTmpDir f = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  tmp <- P.parseRel tmpDir
  f (pwd P.</> tmp)


withTmpDir :: ByteString -> (P.Path P.Abs -> IO a) -> IO a
withTmpDir ip f = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  tmp <- P.parseRel tmpDir
  p <- (pwd P.</> tmp P.</>) <$> P.parseRel ip
  f p


withTmpDir' :: ByteString
            -> ByteString
            -> (P.Path P.Abs -> P.Path P.Abs -> IO a)
            -> IO a
withTmpDir' ip1 ip2 f = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  tmp <- P.parseRel tmpDir
  p1 <- (pwd P.</> tmp P.</>) <$> P.parseRel ip1
  p2 <- (pwd P.</> tmp P.</>) <$> P.parseRel ip2
  f p1 p2


removeFileIfExists :: ByteString -> IO ()
removeFileIfExists bs =
  withTmpDir bs $ \p -> whenM (doesFileExist p) (deleteFile p)


removeDirIfExists :: ByteString -> IO ()
removeDirIfExists bs =
  withTmpDir bs $ \p -> whenM (doesDirectoryExist p) (deleteDirRecursive p)


copyFile' :: ByteString -> ByteString -> CopyMode -> IO ()
copyFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> copyFile p1 p2 cm)


copyDirRecursive' :: ByteString -> ByteString
                  -> CopyMode -> RecursiveMode -> IO ()
copyDirRecursive' inputDirP outputDirP cm rm =
  withTmpDir' inputDirP outputDirP (\p1 p2 -> copyDirRecursive p1 p2 cm rm)


createDir' :: ByteString -> IO ()
createDir' dest = withTmpDir dest createDir


createRegularFile' :: ByteString -> IO ()
createRegularFile' dest = withTmpDir dest createRegularFile


createSymlink' :: ByteString -> ByteString -> IO ()
createSymlink' dest sympoint = withTmpDir dest
  (\x -> createSymlink x sympoint)


renameFile' :: ByteString -> ByteString -> IO ()
renameFile' inputFileP outputFileP =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    renameFile i o
    renameFile o i


moveFile' :: ByteString -> ByteString -> CopyMode -> IO ()
moveFile' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP $ \i o -> do
    moveFile i o cm
    moveFile o i Strict


recreateSymlink' :: ByteString -> ByteString -> CopyMode -> IO ()
recreateSymlink' inputFileP outputFileP cm =
  withTmpDir' inputFileP outputFileP (\p1 p2 -> recreateSymlink p1 p2 cm)


noWritableDirPerms :: ByteString -> IO ()
noWritableDirPerms path = withTmpDir path $ \p ->
  setFileMode (P.fromAbs p) perms
  where
    perms =            ownerReadMode
      `unionFileModes` ownerExecuteMode
      `unionFileModes` groupReadMode
      `unionFileModes` groupExecuteMode
      `unionFileModes` otherReadMode
      `unionFileModes` otherExecuteMode


noPerms :: ByteString -> IO ()
noPerms path = withTmpDir path $ \p -> setFileMode (P.fromAbs p) nullFileMode


normalDirPerms :: ByteString -> IO ()
normalDirPerms path =
  withTmpDir path $ \p -> setFileMode (P.fromAbs p) newDirPerms


getFileType' :: ByteString -> IO FileType
getFileType' path = withTmpDir path getFileType


getDirsFiles' :: ByteString -> IO [P.Path P.Abs]
getDirsFiles' path = withTmpDir path getDirsFiles


deleteFile' :: ByteString -> IO ()
deleteFile' p = withTmpDir p deleteFile


deleteDir' :: ByteString -> IO ()
deleteDir' p = withTmpDir p deleteDir


deleteDirRecursive' :: ByteString -> IO ()
deleteDirRecursive' p = withTmpDir p deleteDirRecursive


canonicalizePath' :: ByteString -> IO (P.Path P.Abs)
canonicalizePath' p = withTmpDir p canonicalizePath


writeFile' :: ByteString -> ByteString -> IO ()
writeFile' ip bs = 
  withTmpDir ip $ \p -> do
    fd <- SPI.openFd (P.fromAbs p) SPI.WriteOnly Nothing
                                   SPI.defaultFileFlags
    _ <- SPB.fdWrite fd bs
    SPI.closeFd fd


allDirectoryContents' :: ByteString -> IO [ByteString]
allDirectoryContents' ip =
  withTmpDir ip $ \p -> allDirectoryContents (P.fromAbs p)

