-- |
-- Module      :  HPath.IO
-- Copyright   :  © 2016 Julian Ospald
-- License     :  GPL-2
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides high-level IO related file operations like
-- copy, delete, move and so on. It only operates on /Path Abs/ which
-- guarantees us well-typed paths which are absolute.
--
-- Some functions are just path-safe wrappers around
-- unix functions, others have stricter exception handling
-- and some implement functionality that doesn't have a unix
-- counterpart (like `copyDirRecursive`).
--
-- Some of these operations are due to their nature __not atomic__, which
-- means they may do multiple syscalls which form one context. Some
-- of them also have to examine the filetypes explicitly before the
-- syscalls, so a reasonable decision can be made. That means
-- the result is undefined if another process changes that context
-- while the non-atomic operation is still happening. However, where
-- possible, as few syscalls as possible are used and the underlying
-- exception handling is kept.
--
-- Note: `BlockDevice`, `CharacterDevice`, `NamedPipe` and `Socket`
-- are not explicitly supported right now. Calling any of these
-- functions on such a file may throw an exception.

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module HPath.IO
  (
  -- * Types
    FileType(..)
  -- * File copying
  , copyDirRecursive
  , copyDirRecursiveOverwrite
  , recreateSymlink
  , copyFile
  , copyFileOverwrite
  , easyCopy
  , easyCopyOverwrite
  -- * File deletion
  , deleteFile
  , deleteDir
  , deleteDirRecursive
  , easyDelete
  -- * File opening
  , openFile
  , executeFile
  -- * File creation
  , createRegularFile
  , createDir
  -- * File renaming/moving
  , renameFile
  , moveFile
  , moveFileOverwrite
  -- * File permissions
  , newFilePerms
  , newDirPerms
  -- * Directory reading
  , getDirsFiles
  -- * Filetype operations
  , getFileType
  -- * Others
  , canonicalizePath
  )
  where


import Control.Exception
  (
    bracket
  , bracketOnError
  , throwIO
  )
import Control.Monad
  (
    void
  , when
  )
import Data.ByteString
  (
    ByteString
  )
import Data.Foldable
  (
    for_
  )
import Data.Maybe
  (
    catMaybes
  )
import Data.Word
  (
    Word8
  )
import Foreign.C.Error
  (
    eEXIST
  , eNOTEMPTY
  , eXDEV
  )
import Foreign.C.Types
  (
    CSize
  )
import Foreign.Marshal.Alloc
  (
    allocaBytes
  )
import Foreign.Ptr
  (
    Ptr
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import HPath
import HPath.Internal
import HPath.IO.Errors
import HPath.IO.Utils
import Prelude hiding (readFile)
import System.IO.Error
  (
    catchIOError
  , ioeGetErrorType
  )
import System.Posix.ByteString
  (
    exclusive
  )
import System.Posix.Directory.ByteString
  (
    createDirectory
  , removeDirectory
  )
import System.Posix.Directory.Traversals
  (
    getDirectoryContents'
  )
import System.Posix.Files.ByteString
  (
    createSymbolicLink
  , fileMode
  , getFdStatus
  , groupExecuteMode
  , groupReadMode
  , groupWriteMode
  , otherExecuteMode
  , otherReadMode
  , otherWriteMode
  , ownerModes
  , ownerReadMode
  , ownerWriteMode
  , readSymbolicLink
  , removeLink
  , rename
  , setFileMode
  , unionFileModes
  )
import qualified System.Posix.Files.ByteString as PF
import qualified "unix" System.Posix.IO.ByteString as SPI
import qualified "unix-bytestring" System.Posix.IO.ByteString as SPB
import qualified System.Posix.Directory.Traversals as SPDT
import qualified System.Posix.Directory.Foreign as SPDF
import qualified System.Posix.Process.ByteString as SPP
import System.Posix.Types
  (
    FileMode
  , ProcessID
  , Fd
  )





data FileType = Directory
              | RegularFile
              | SymbolicLink
              | BlockDevice
              | CharacterDevice
              | NamedPipe
              | Socket
  deriving (Eq, Show)





    --------------------
    --[ File Copying ]--
    --------------------



-- |Copies a directory recursively to the given destination.
-- Does not follow symbolic links.
--
-- Safety/reliability concerns:
--
--    * not atomic
--    * examines filetypes explicitly
--    * an explicit check `throwDestinationInSource` is carried out for the
--      top directory for basic sanity, because otherwise we might end up
--      with an infinite copy loop... however, this operation is not
--      carried out recursively (because it's slow)
--
-- Throws:
--
--    - `NoSuchThing` if source directory does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source directory is wrong type (symlink)
--    - `InvalidArgument` if source directory is wrong type (regular file)
--    - `SameFile` if source and destination are the same file (`HPathIOException`)
--    - `AlreadyExists` if destination already exists
--    - `DestinationInSource` if destination is contained in source (`HPathIOException`)
copyDirRecursive :: Path Abs  -- ^ source dir
                 -> Path Abs  -- ^ full destination
                 -> IO ()
copyDirRecursive fromp destdirp
  = do
    -- for performance, sanity checks are only done for the top dir
    throwSameFile fromp destdirp
    throwDestinationInSource fromp destdirp
    go fromp destdirp
  where
    go :: Path Abs -> Path Abs -> IO ()
    go fromp' destdirp' = do
      -- order is important here, so we don't get empty directories
      -- on failure
      contents <- getDirsFiles fromp'

      fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus (fromAbs fromp')
      createDirectory (fromAbs destdirp') fmode'

      for_ contents $ \f -> do
        ftype <- getFileType f
        newdest <- (destdirp' </>) <$> basename f
        case ftype of
          SymbolicLink -> recreateSymlink f newdest
          Directory    -> go f newdest
          RegularFile  -> copyFile f newdest
          _            -> ioError $ userError $ "No idea what to do with the" ++
                                                "given filetype: " ++ show ftype


-- |Like `copyDirRecursive` except it overwrites contents of directories
-- if any.
--
-- Throws:
--
--    - `NoSuchThing` if source directory does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source directory is wrong type (symlink)
--    - `InvalidArgument` if source directory is wrong type (regular file)
--    - `SameFile` if source and destination are the same file (`HPathIOException`)
--    - `DestinationInSource` if destination is contained in source (`HPathIOException`)
copyDirRecursiveOverwrite :: Path Abs  -- ^ source dir
                          -> Path Abs  -- ^ full destination
                          -> IO ()
copyDirRecursiveOverwrite fromp destdirp
  = do
    -- for performance, sanity checks are only done for the top dir
    throwSameFile fromp destdirp
    throwDestinationInSource fromp destdirp
    go fromp destdirp
  where
    go :: Path Abs -> Path Abs -> IO ()
    go fromp' destdirp' = do
      -- order is important here, so we don't get empty directories
      -- on failure
      contents <- getDirsFiles fromp'

      fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus (fromAbs fromp')
      catchIOError (createDirectory (fromAbs destdirp') fmode') $ \e ->
        case ioeGetErrorType e of
          AlreadyExists -> setFileMode (fromAbs destdirp') fmode'
          _             -> ioError e

      for_ contents $ \f -> do
        ftype <- getFileType f
        newdest <- (destdirp' </>) <$> basename f
        case ftype of
          SymbolicLink -> whenM (doesFileExist newdest) (deleteFile newdest)
                          >> recreateSymlink f newdest
          Directory    -> go f newdest
          RegularFile  -> copyFileOverwrite f newdest
          _            -> ioError $ userError $ "No idea what to do with the" ++
                                                "given filetype: " ++ show ftype


-- |Recreate a symlink.
--
-- Throws:
--
--    - `InvalidArgument` if symlink file is wrong type (file)
--    - `InvalidArgument` if symlink file is wrong type (directory)
--    - `PermissionDenied` if output directory cannot be written to
--    - `PermissionDenied` if source directory cannot be opened
--    - `AlreadyExists` if destination file already exists
--    - `SameFile` if source and destination are the same file (`HPathIOException`)
--
-- Note: calls `symlink`
recreateSymlink :: Path Abs  -- ^ the old symlink file
                -> Path Abs  -- ^ destination file
                -> IO ()
recreateSymlink symsource newsym
  = do
    throwSameFile symsource newsym
    sympoint <- readSymbolicLink (fromAbs symsource)
    createSymbolicLink sympoint (fromAbs newsym)


-- |Copies the given regular file to the given destination.
-- Neither follows symbolic links, nor accepts them.
-- For "copying" symbolic links, use `recreateSymlink` instead.
--
-- Throws:
--
--    - `NoSuchThing` if source file does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source file is wrong type (symlink)
--    - `InvalidArgument` if source file is wrong type (directory)
--    - `SameFile` if source and destination are the same file (`HPathIOException`)
--    - `AlreadyExists` if destination already exists
--
-- Note: calls `sendfile`
copyFile :: Path Abs  -- ^ source file
         -> Path Abs  -- ^ destination file
         -> IO ()
copyFile from to = do
  throwSameFile from to
  _copyFile [SPDF.oNofollow]
            [SPDF.oNofollow, SPDF.oExcl]
            from to


-- |Like `copyFile` except it overwrites the destination if it already
-- exists.
-- This also works if source and destination are the same file.
--
-- Safety/reliability concerns:
--
--    * not atomic
--    * falls back to delete-copy method with explicit checks
--
-- Throws:
--
--    - `NoSuchThing` if source file does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source file is wrong type (symlink)
--    - `InvalidArgument` if source file is wrong type (directory)
--    - `SameFile` if source and destination are the same file (`HPathIOException`)
--
-- Note: calls `sendfile`
copyFileOverwrite :: Path Abs  -- ^ source file
                  -> Path Abs  -- ^ destination file
                  -> IO ()
copyFileOverwrite from to = do
  throwSameFile from to
  catchIOError (_copyFile [SPDF.oNofollow]
                          [SPDF.oNofollow, SPDF.oTrunc]
                          from to) $ \e ->
    case ioeGetErrorType e of
      -- if the destination file is not writable, we need to
      -- figure out if we can still copy by deleting it first
      PermissionDenied -> do
        exists   <- doesFileExist to
        writable <- isWritable (dirname to)
        if exists && writable
          then deleteFile to >> copyFile from to
          else ioError e
      _ -> ioError e


_copyFile :: [SPDF.Flags]
          -> [SPDF.Flags]
          -> Path Abs  -- ^ source file
          -> Path Abs  -- ^ destination file
          -> IO ()
_copyFile sflags dflags from to
  =
    -- from sendfile(2) manpage:
    --   Applications  may  wish  to  fall back to read(2)/write(2) in the case
    --   where sendfile() fails with EINVAL or ENOSYS.
    withAbsPath to $ \to' -> withAbsPath from $ \from' ->
      void $ fallbackCopy from' to'
  where
    -- low-level copy operation utilizing read(2)/write(2)
    -- in case `sendFileCopy` fails/is unsupported
    fallbackCopy source dest =
      bracket (SPDT.openFd source SPI.ReadOnly sflags Nothing)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracketeer (SPDT.openFd dest SPI.WriteOnly
                             dflags $ Just fileM)
                           SPI.closeFd
                           (\fd -> SPI.closeFd fd >> deleteFile to)
                            $ \dfd -> allocaBytes (fromIntegral bufSize) $ \buf ->
                                        write' sfd dfd buf 0
      where
        bufSize :: CSize
        bufSize = 8192
        write' :: Fd -> Fd -> Ptr Word8 -> Int -> IO Int
        write' sfd dfd buf totalsize = do
            size <- SPB.fdReadBuf sfd buf bufSize
            if size == 0
              then return $ fromIntegral totalsize
              else do rsize <- SPB.fdWriteBuf dfd buf size
                      when (rsize /= size) (throwIO . CopyFailed $ "wrong size!")
                      write' sfd dfd buf (totalsize + fromIntegral size)


-- |Copies anything. In case of a symlink,
-- it is just recreated, even if it points to a directory.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: Path Abs
         -> Path Abs
         -> IO ()
easyCopy from to = do
  ftype <- getFileType from
  case ftype of
       SymbolicLink -> recreateSymlink from to
       RegularFile  -> copyFile from to
       Directory    -> copyDirRecursive from to
       _            -> ioError $ userError $ "No idea what to do with the" ++
                                             "given filetype: " ++ show ftype


-- |Like `easyCopy` except it overwrites the destination if it already exists.
-- For directories, this overwrites contents without pruning them, so the resulting
-- directory may have more files than have been copied.
easyCopyOverwrite :: Path Abs
                  -> Path Abs
                  -> IO ()
easyCopyOverwrite from to = do
  ftype <- getFileType from
  case ftype of
       SymbolicLink -> whenM (doesFileExist to) (deleteFile to)
                       >> recreateSymlink from to
       RegularFile  -> copyFileOverwrite from to
       Directory    -> copyDirRecursiveOverwrite from to
       _            -> ioError $ userError $ "No idea what to do with the" ++
                                             "given filetype: " ++ show ftype






    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes the given file, does not follow symlinks. Raises `eISDIR`
-- if run on a directory. Does not follow symbolic links.
--
-- Throws:
--
--    - `InappropriateType` for wrong file type (directory)
--    - `NoSuchThing` if the file does not exist
--    - `PermissionDenied` if the directory cannot be read
deleteFile :: Path Abs -> IO ()
deleteFile p = withAbsPath p removeLink


-- |Deletes the given directory, which must be empty, never symlinks.
--
-- Throws:
--
--    - `InappropriateType` for wrong file type (symlink to directory)
--    - `InappropriateType` for wrong file type (regular file)
--    - `NoSuchThing` if directory does not exist
--    - `UnsatisfiedConstraints` if directory is not empty
--    - `PermissionDenied` if we can't open or write to parent directory
--
-- Notes: calls `rmdir`
deleteDir :: Path Abs -> IO ()
deleteDir p = withAbsPath p removeDirectory


-- |Deletes the given directory recursively. Does not follow symbolic
-- links. Tries `deleteDir` first before attemtping a recursive
-- deletion.
--
-- Safety/reliability concerns:
--
--    * not atomic
--    * examines filetypes explicitly
--
-- Throws:
--
--    - `InappropriateType` for wrong file type (symlink to directory)
--    - `InappropriateType` for wrong file type (regular file)
--    - `NoSuchThing` if directory does not exist
--    - `PermissionDenied` if we can't open or write to parent directory
deleteDirRecursive :: Path Abs -> IO ()
deleteDirRecursive p =
  catchErrno [eNOTEMPTY, eEXIST]
             (deleteDir p)
    $ do
      files <- getDirsFiles p
      for_ files $ \file -> do
        ftype <- getFileType file
        case ftype of
          SymbolicLink -> deleteFile file
          Directory    -> deleteDirRecursive file
          RegularFile  -> deleteFile file
          _            -> ioError $ userError $ "No idea what to do with the" ++
                                                "given filetype: " ++ show ftype
      removeDirectory . toFilePath $ p


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: Path Abs -> IO ()
easyDelete p = do
  ftype <- getFileType p
  case ftype of
    SymbolicLink -> deleteFile p
    Directory    -> deleteDirRecursive p
    RegularFile  -> deleteFile p
    _            -> ioError $ userError $ "No idea what to do with the" ++
                                          "given filetype: " ++ show ftype




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open. The file type
-- is not checked. This forks a process.
openFile :: Path Abs
         -> IO ProcessID
openFile p =
  withAbsPath p $ \fp ->
    SPP.forkProcess $ SPP.executeFile "xdg-open" True [fp] Nothing


-- |Executes a program with the given arguments. This forks a process.
executeFile :: Path Abs        -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile fp args
  = withAbsPath fp $ \fpb ->
      SPP.forkProcess
      $ SPP.executeFile fpb True args Nothing




    ---------------------
    --[ File Creation ]--
    ---------------------


-- |Create an empty regular file at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination file already exists
createRegularFile :: Path Abs -> IO ()
createRegularFile dest =
  bracket (SPI.openFd (fromAbs dest) SPI.WriteOnly (Just newFilePerms)
                      (SPI.defaultFileFlags { exclusive = True }))
          SPI.closeFd
          (\_ -> return ())


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination directory already exists
createDir :: Path Abs -> IO ()
createDir dest = createDirectory (fromAbs dest) newDirPerms




    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


-- |Rename a given file with the provided filename. Destination and source
-- must be on the same device, otherwise `eXDEV` will be raised.
--
-- Does not follow symbolic links, but renames the symbolic link file.
--
-- Safety/reliability concerns:
--
--    * has a separate set of exception handling, apart from the syscall
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `UnsupportedOperation` if source and destination are on different devices
--     - `FileDoesExist` if destination file already exists
--     - `DirDoesExist` if destination directory already exists
--     - `SameFile` if destination and source are the same file (`HPathIOException`)
--
-- Note: calls `rename` (but does not allow to rename over existing files)
renameFile :: Path Abs -> Path Abs -> IO ()
renameFile fromf tof = do
  throwSameFile fromf tof
  throwFileDoesExist tof
  throwDirDoesExist tof
  rename (fromAbs fromf) (fromAbs tof)


-- |Move a file. This also works across devices by copy-delete fallback.
-- And also works on directories.
--
-- Does not follow symbolic links, but renames the symbolic link file.
--
-- Safety/reliability concerns:
--
--    * copy-delete fallback is inherently non-atomic
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `FileDoesExist` if destination file already exists
--     - `DirDoesExist` if destination directory already exists
--     - `SameFile` if destination and source are the same file (`HPathIOException`)
--
-- Note: calls `rename` (but does not allow to rename over existing files)
moveFile :: Path Abs  -- ^ file to move
         -> Path Abs  -- ^ destination
         -> IO ()
moveFile from to = do
  throwSameFile from to
  catchErrno [eXDEV] (renameFile from to) $ do
    easyCopy from to
    easyDelete from


-- |Like `moveFile`, but overwrites the destination if it exists.
--
-- Does not follow symbolic links, but renames the symbolic link file.
--
-- Safety/reliability concerns:
--
--    * copy-delete fallback is inherently non-atomic
--    * checks for file types and destination file existence explicitly
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `SameFile` if destination and source are the same file (`HPathIOException`)
--
-- Note: calls `rename` (but does not allow to rename over existing files)
moveFileOverwrite :: Path Abs  -- ^ file to move
                  -> Path Abs  -- ^ destination
                  -> IO ()
moveFileOverwrite from to = do
  throwSameFile from to
  ft <- getFileType from
  writable <- isWritable $ dirname to
  case ft of
    RegularFile -> do
      exists <- doesFileExist to
      when (exists && writable) (deleteFile to)
    SymbolicLink -> do
      exists <- doesFileExist to
      when (exists && writable) (deleteFile to)
    Directory -> do
      exists <- doesDirectoryExist to
      when (exists && writable) (deleteDir to)
    _ -> ioError $ userError $ "Don't know how to handle filetype " ++
                               show ft
  moveFile from to




    -----------------------
    --[ File Permissions]--
    -----------------------


-- |Default permissions for a new file.
newFilePerms :: FileMode
newFilePerms
  =                  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode


-- |Default permissions for a new directory.
newDirPerms :: FileMode
newDirPerms
  =                  ownerModes
    `unionFileModes` groupExecuteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherExecuteMode
    `unionFileModes` otherReadMode



    -------------------------
    --[ Directory reading ]--
    -------------------------


-- |Gets all filenames of the given directory. This excludes "." and "..".
-- This version does not follow symbolic links.
--
-- Throws:
--
--     - `NoSuchThing` if directory does not exist
--     - `InappropriateType` if file type is wrong (file)
--     - `InappropriateType` if file type is wrong (symlink to file)
--     - `InappropriateType` if file type is wrong (symlink to dir)
--     - `PermissionDenied` if directory cannot be opened
getDirsFiles :: Path Abs        -- ^ dir to read
             -> IO [Path Abs]
getDirsFiles p =
  withAbsPath p $ \fp ->
    bracketOnError (SPDT.openFd fp SPI.ReadOnly [SPDF.oNofollow] Nothing)
            SPI.closeFd
            $ \fd ->
              return
                . catMaybes
                .   fmap (\x -> (</>) p <$> (parseMaybe . snd $ x))
                =<< getDirectoryContents' fd
  where
    parseMaybe :: ByteString -> Maybe (Path Fn)
    parseMaybe = parseFn




    ---------------------------
    --[ FileType operations ]--
    ---------------------------


-- |Get the file type of the file located at the given path. Does
-- not follow symbolic links.
--
-- Throws:
--
--    - `NoSuchThing` if the file does not exist
--    - `PermissionDenied` if any part of the path is not accessible
getFileType :: Path Abs -> IO FileType
getFileType p = do
  fs <- PF.getSymbolicLinkStatus (fromAbs p)
  decide fs
  where
    decide fs
      | PF.isDirectory fs       = return Directory
      | PF.isRegularFile fs     = return RegularFile
      | PF.isSymbolicLink fs    = return SymbolicLink
      | PF.isBlockDevice fs     = return BlockDevice
      | PF.isCharacterDevice fs = return CharacterDevice
      | PF.isNamedPipe fs       = return NamedPipe
      | PF.isSocket fs          = return Socket
      | otherwise               = ioError $ userError "No filetype?!"



    --------------
    --[ Others ]--
    --------------



-- |Applies `realpath` on the given absolute path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
canonicalizePath :: Path Abs -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- SPDT.realpath l
  return $ MkPath nl
