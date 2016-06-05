-- |
-- Module      :  HPath.IO
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
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
-- are ignored by some of the more high-level functions (like `easyCopy`).
-- For other functions (like `copyFile`), the behavior on these file types is
-- unreliable/unsafe. Check the documentation of those functions for details.

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module HPath.IO
  (
  -- * Types
    FileType(..)
  , RecursiveErrorMode(..)
  , CopyMode(..)
  -- * File copying
  , copyDirRecursive
  , recreateSymlink
  , copyFile
  , easyCopy
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
  , createSymlink
  -- * File renaming/moving
  , renameFile
  , moveFile
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


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception
  (
    IOException
  , bracket
  , throwIO
  )
import Control.Monad
  (
    unless
  , void
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
import Data.IORef
  (
    IORef
  , modifyIORef
  , newIORef
  , readIORef
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
  , eINVAL
  , eNOSYS
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
import Prelude hiding (readFile)
import System.IO.Error
  (
    catchIOError
  , ioeGetErrorType
  )
import System.Linux.Sendfile
  (
    sendfileFd
  , FileRange(..)
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
import System.Posix.FD
  (
    openFd
  )
import qualified System.Posix.Directory.Traversals as SPDT
import qualified System.Posix.Directory.Foreign as SPDF
import qualified System.Posix.Process.ByteString as SPP
import System.Posix.Types
  (
    FileMode
  , ProcessID
  , Fd
  )





    -------------
    --[ Types ]--
    -------------


data FileType = Directory
              | RegularFile
              | SymbolicLink
              | BlockDevice
              | CharacterDevice
              | NamedPipe
              | Socket
  deriving (Eq, Show)



-- |The error mode for any recursive operation.
--
-- On `FailEarly` the whole operation fails immediately if any of the
-- recursive sub-operations fail, which is sort of the default
-- for IO operations.
--
-- On `CollectFailures` skips errors in the recursion and keeps on recursing.
-- However all errors are collected in the `RecursiveFailure` error type,
-- which is raised finally if there was any error. Also note that
-- `RecursiveFailure` does not give any guarantees on the ordering
-- of the collected exceptions.
data RecursiveErrorMode = FailEarly
                        | CollectFailures


-- |The mode for copy and file moves.
-- Overwrite mode is usually not very well defined, but is a convenience
-- shortcut.
data CopyMode = Strict    -- ^ fail if any target exists
              | Overwrite -- ^ overwrite targets




    --------------------
    --[ File Copying ]--
    --------------------



-- |Copies the contents of a directory recursively to the given destination, while preserving permissions.
-- Does not follow symbolic links. This behaves more or less like
-- the following, without descending into the destination if it
-- already exists:
--
-- @
--   cp -a \/source\/dir \/destination\/somedir
-- @
--
-- For directory contents, this will ignore any file type that is not
-- `RegularFile`, `SymbolicLink` or `Directory`.
--
-- For `Overwrite` copy mode this does not prune destination directory
-- contents, so the destination might contain more files than the source after
-- the operation has completed. Permissions of existing directories are
-- fixed.
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
--    - `PermissionDenied` if source directory can't be opened
--    - `SameFile` if source and destination are the same file
--      (`HPathIOException`)
--    - `DestinationInSource` if destination is contained in source
--      (`HPathIOException`)
--
-- Throws in `FailEarly` RecursiveErrorMode only:
--
--    - `PermissionDenied` if output directory is not writable
--    - `InvalidArgument` if source directory is wrong type (symlink)
--    - `InappropriateType` if source directory is wrong type (regular file)
--
-- Throws in `CollectFailures` RecursiveErrorMode only:
--
--    - `RecursiveFailure` if any of the recursive operations that are not
--      part of the top-directory sanity-checks fail (`HPathIOException`)
--
-- Throws in `Strict` CopyMode only:
--
--    - `AlreadyExists` if destination already exists
copyDirRecursive :: Path Abs  -- ^ source dir
                 -> Path Abs  -- ^ destination (parent dirs
                              --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive fromp destdirp cm rm
  = do
    ce <- newIORef []
    -- for performance, sanity checks are only done for the top dir
    throwSameFile fromp destdirp
    throwDestinationInSource fromp destdirp
    go ce fromp destdirp
    collectedExceptions <- readIORef ce
    unless (null collectedExceptions)
           (throwIO . RecursiveFailure $ collectedExceptions)
  where
    go :: IORef [IOException] -> Path Abs -> Path Abs -> IO ()
    go ce fromp' destdirp' = do

      -- order is important here, so we don't get empty directories
      -- on failure
      contents <- handleIOE ce [] $ do
        contents <- getDirsFiles fromp'

        fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus (fromAbs fromp')
        case cm of
          Strict    -> createDirectory (fromAbs destdirp') fmode'
          Overwrite -> catchIOError (createDirectory (fromAbs destdirp')
                                                     fmode')
                         $ \e ->
                           case ioeGetErrorType e of
                             AlreadyExists -> setFileMode (fromAbs destdirp')
                                                          fmode'
                             _             -> ioError e
        return contents

      -- we can't use `easyCopy` here, because we want to call `go`
      -- recursively to skip the top-level sanity checks
      for_ contents $ \f -> do
        ftype <- getFileType f
        newdest <- (destdirp' </>) <$> basename f
        case ftype of
          SymbolicLink -> handleIOE ce ()
                            $ recreateSymlink f newdest cm
          Directory    -> go ce f newdest
          RegularFile  -> handleIOE ce () $ copyFile f newdest cm
          _            -> return ()
    handleIOE :: IORef [IOException] -> a -> IO a -> IO a
    handleIOE ce def = case rm of
      FailEarly -> handleIOError throwIO
      CollectFailures -> handleIOError (\e -> modifyIORef ce (e:)
                                         >> return def)


-- |Recreate a symlink.
--
-- In `Overwrite` copy mode only files and empty directories are deleted.
--
-- Safety/reliability concerns:
--
--    * `Overwrite` mode is inherently non-atomic
--
-- Throws:
--
--    - `InvalidArgument` if source file is wrong type (not a symlink)
--    - `PermissionDenied` if output directory cannot be written to
--    - `PermissionDenied` if source directory cannot be opened
--    - `SameFile` if source and destination are the same file
--      (`HPathIOException`)
--
--
-- Throws in `Strict` mode only:
--
--    - `AlreadyExists` if destination file already exists
--
-- Throws in `Overwrite` mode only:
--
--    - `UnsatisfiedConstraints` if destination file is non-empty directory
--
-- Note: calls `symlink`
recreateSymlink :: Path Abs  -- ^ the old symlink file
                -> Path Abs  -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink symsource newsym cm
  = do
    throwSameFile symsource newsym
    sympoint <- readSymbolicLink (fromAbs symsource)
    case cm of
      Strict -> return ()
      Overwrite -> do
        writable <- isWritable (dirname newsym)
        isfile   <- doesFileExist newsym
        isdir    <- doesDirectoryExist newsym
        when (writable && isfile) (deleteFile newsym)
        when (writable && isdir)  (deleteDir newsym)
    createSymbolicLink sympoint (fromAbs newsym)


-- |Copies the given regular file to the given destination.
-- Neither follows symbolic links, nor accepts them.
-- For "copying" symbolic links, use `recreateSymlink` instead.
--
-- Note that this is still sort of a low-level function and doesn't
-- examine file types. For a more high-level version, use `easyCopy`
-- instead.
--
-- In `Overwrite` copy mode only overwrites actual files, not directories.
--
-- Safety/reliability concerns:
--
--    * `Overwrite` mode is not atomic
--    * when used on `CharacterDevice`, reads the "contents" and copies
--      them to a regular file, which might take indefinitely
--    * when used on `BlockDevice`, may either read the "contents"
--      and copy them to a regular file (potentially hanging indefinitely)
--      or may create a regular empty destination file
--    * when used on `NamedPipe`, will hang indefinitely
--
-- Throws:
--
--    - `NoSuchThing` if source file does not exist
--    - `NoSuchThing` if source file is a a `Socket`
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source file is wrong type (symlink or directory)
--    - `SameFile` if source and destination are the same file
--      (`HPathIOException`)
--
-- Throws in `Strict` mode only:
--
--    - `AlreadyExists` if destination already exists
--
-- Note: calls `sendfile` and possibly `read`/`write` as fallback
copyFile :: Path Abs  -- ^ source file
         -> Path Abs  -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile from to cm = do
  throwSameFile from to
  
  case cm of
    Strict -> _copyFile [SPDF.oNofollow]
                        [SPDF.oNofollow, SPDF.oExcl]
                        from to
    Overwrite -> 
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
              then deleteFile to >> copyFile from to Strict
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
    --   Applications  may  wish  to  fall back to read(2)/write(2) in
    --   the case where sendfile() fails with EINVAL or ENOSYS.
    withAbsPath to $ \to' -> withAbsPath from $ \from' ->
      catchErrno [eINVAL, eNOSYS]
                 (sendFileCopy from' to')
                 (void $ readWriteCopy from' to')
  where
    copyWith copyAction source dest =
      bracket (openFd source SPI.ReadOnly sflags Nothing)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracketeer (openFd dest SPI.WriteOnly
                             dflags $ Just fileM)
                           SPI.closeFd
                           (\fd -> SPI.closeFd fd >> deleteFile to)
                           $ \dfd -> copyAction sfd dfd
    -- this is low-level stuff utilizing sendfile(2) for speed
    sendFileCopy :: ByteString -> ByteString -> IO ()
    sendFileCopy = copyWith
      (\sfd dfd -> sendfileFd dfd sfd EntireFile $ return ())
    -- low-level copy operation utilizing read(2)/write(2)
    -- in case `sendFileCopy` fails/is unsupported
    readWriteCopy :: ByteString -> ByteString -> IO Int
    readWriteCopy = copyWith
      (\sfd dfd -> allocaBytes (fromIntegral bufSize)
                     $ \buf -> write' sfd dfd buf 0)
      where
        bufSize :: CSize
        bufSize = 8192
        write' :: Fd -> Fd -> Ptr Word8 -> Int -> IO Int
        write' sfd dfd buf totalsize = do
            size <- SPB.fdReadBuf sfd buf bufSize
            if size == 0
              then return $ fromIntegral totalsize
              else do rsize <- SPB.fdWriteBuf dfd buf size
                      when (rsize /= size) (throwIO . CopyFailed
                                            $ "wrong size!")
                      write' sfd dfd buf (totalsize + fromIntegral size)


-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: Path Abs
         -> Path Abs
         -> CopyMode
         -> RecursiveErrorMode
         -> IO ()
easyCopy from to cm rm = do
  ftype <- getFileType from
  case ftype of
       SymbolicLink -> recreateSymlink from to cm
       RegularFile  -> copyFile from to cm
       Directory    -> copyDirRecursive from to cm rm
       _            -> return ()





    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes the given file. Raises `eISDIR`
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
-- On directory contents this behaves like `easyDelete`
-- and thus will ignore any file type that is not `RegularFile`,
-- `SymbolicLink` or `Directory`.
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
          _            -> return ()
      removeDirectory . toFilePath $ p


-- |Deletes a file, directory or symlink.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
-- Any other file type is ignored.
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
    _            -> return ()




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


-- |Create an empty regular file at the given directory with the given
-- filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination file already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createRegularFile :: FileMode -> Path Abs -> IO ()
createRegularFile fm dest =
  bracket (SPI.openFd (fromAbs dest) SPI.WriteOnly (Just fm)
                      (SPI.defaultFileFlags { exclusive = True }))
          SPI.closeFd
          (\_ -> return ())


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination directory already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: FileMode -> Path Abs -> IO ()
createDir fm dest = createDirectory (fromAbs dest) fm


-- |Create a symlink.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination file already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
--
-- Note: calls `symlink`
createSymlink :: Path Abs   -- ^ destination file
              -> ByteString -- ^ path the symlink points to
              -> IO ()
createSymlink dest sympoint
  = createSymbolicLink sympoint (fromAbs dest)



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
--     - `UnsupportedOperation` if source and destination are on different
--       devices
--     - `FileDoesExist` if destination file already exists
--       (`HPathIOException`)
--     - `DirDoesExist` if destination directory already exists
--       (`HPathIOException`)
--     - `SameFile` if destination and source are the same file
--       (`HPathIOException`)
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
--
-- Safety/reliability concerns:
--
--    * `Overwrite` mode is not atomic
--    * copy-delete fallback is inherently non-atomic
--    * since this function calls `easyCopy` and `easyDelete` as a fallback
--      to `renameFile`, file types that are not `RegularFile`, `SymbolicLink`
--      or `Directory` may be ignored
--    * for `Overwrite` mode, the destination will be deleted (not recursively)
--      before moving
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `SameFile` if destination and source are the same file
--       (`HPathIOException`)
--
-- Throws in `Strict` mode only:
--
--    - `FileDoesExist` if destination file already exists (`HPathIOException`)
--    - `DirDoesExist` if destination directory already exists
--      (`HPathIOException`)
--
-- Note: calls `rename` (but does not allow to rename over existing files)
moveFile :: Path Abs  -- ^ file to move
         -> Path Abs  -- ^ destination
         -> CopyMode
         -> IO ()
moveFile from to cm = do
  throwSameFile from to
  case cm of
    Strict -> catchErrno [eXDEV] (renameFile from to) $ do
                easyCopy from to Strict FailEarly
                easyDelete from
    Overwrite -> do
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
        _ -> return ()
      moveFile from to Strict






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
-- The contents are not sorted and there is no guarantee on the ordering.
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
  withAbsPath p $ \fp -> do
    fd <- openFd fp SPI.ReadOnly [SPDF.oNofollow] Nothing
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
