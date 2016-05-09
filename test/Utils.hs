{-# LANGUAGE OverloadedStrings #-}

module Utils where


import HPath.IO
import HPath.IO.Errors
import HPath.IO.Utils
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
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



    -----------------
    --[ Utilities ]--
    -----------------


withPwd :: ByteString -> (P.Path P.Abs -> IO a) -> IO a
withPwd ip f = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  p <- (pwd P.</>) <$> P.parseRel ip
  f p


withPwd' :: ByteString
         -> ByteString
         -> (P.Path P.Abs -> P.Path P.Abs -> IO a)
         -> IO a
withPwd' ip1 ip2 f = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  p1 <- (pwd P.</>) <$> P.parseRel ip1
  p2 <- (pwd P.</>) <$> P.parseRel ip2
  f p1 p2


removeFileIfExists :: ByteString -> IO ()
removeFileIfExists bs =
  withPwd bs $ \p -> whenM (doesFileExist p) (deleteFile p)


removeDirIfExists :: ByteString -> IO ()
removeDirIfExists bs =
  withPwd bs $ \p -> whenM (doesDirectoryExist p) (deleteDirRecursive p)


copyFile' :: ByteString -> ByteString -> IO ()
copyFile' inputFileP outputFileP =
  withPwd' inputFileP outputFileP copyFile


copyFileOverwrite' :: ByteString -> ByteString -> IO ()
copyFileOverwrite' inputFileP outputFileP =
  withPwd' inputFileP outputFileP copyFileOverwrite


copyDirRecursive' :: ByteString -> ByteString -> IO ()
copyDirRecursive' inputDirP outputDirP =
  withPwd' inputDirP outputDirP copyDirRecursive


copyDirRecursiveOverwrite' :: ByteString -> ByteString -> IO ()
copyDirRecursiveOverwrite' inputDirP outputDirP =
  withPwd' inputDirP outputDirP copyDirRecursiveOverwrite


createDir' :: ByteString -> IO ()
createDir' dest = withPwd dest createDir


createRegularFile' :: ByteString -> IO ()
createRegularFile' dest = withPwd dest createRegularFile


renameFile' :: ByteString -> ByteString -> IO ()
renameFile' inputFileP outputFileP =
  withPwd' inputFileP outputFileP $ \i o -> do
    renameFile i o
    renameFile o i


moveFile' :: ByteString -> ByteString -> IO ()
moveFile' inputFileP outputFileP =
  withPwd' inputFileP outputFileP $ \i o -> do
    moveFile i o
    moveFile o i


moveFileOverwrite' :: ByteString -> ByteString -> IO ()
moveFileOverwrite' inputFileP outputFileP =
  withPwd' inputFileP outputFileP $ \i o -> do
    moveFileOverwrite i o
    moveFile o i


recreateSymlink' :: ByteString -> ByteString -> IO ()
recreateSymlink' inputFileP outputFileP =
  withPwd' inputFileP outputFileP recreateSymlink


noWritableDirPerms :: ByteString -> IO ()
noWritableDirPerms path = withPwd path $ \p ->
  setFileMode (P.fromAbs p) perms
  where
    perms =            ownerReadMode
      `unionFileModes` ownerExecuteMode
      `unionFileModes` groupReadMode
      `unionFileModes` groupExecuteMode
      `unionFileModes` otherReadMode
      `unionFileModes` otherExecuteMode


noPerms :: ByteString -> IO ()
noPerms path = withPwd path $ \p -> setFileMode (P.fromAbs p) nullFileMode


normalDirPerms :: ByteString -> IO ()
normalDirPerms path =
  withPwd path $ \p -> setFileMode (P.fromAbs p) newDirPerms


getFileType' :: ByteString -> IO FileType
getFileType' path = withPwd path getFileType


getDirsFiles' :: ByteString -> IO [P.Path P.Abs]
getDirsFiles' path = withPwd path getDirsFiles


deleteFile' :: ByteString -> IO ()
deleteFile' p = withPwd p deleteFile


deleteDir' :: ByteString -> IO ()
deleteDir' p = withPwd p deleteDir


deleteDirRecursive' :: ByteString -> IO ()
deleteDirRecursive' p = withPwd p deleteDirRecursive


canonicalizePath' :: ByteString -> IO (P.Path P.Abs)
canonicalizePath' p = withPwd p canonicalizePath

