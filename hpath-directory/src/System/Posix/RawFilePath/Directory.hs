-- |
-- Module      :  System.Posix.RawFilePath.Directory
-- Copyright   :  © 2020 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides IO related file operations like
-- copy, delete, move and so on, similar to the 'directory' package.
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
--
-- Import as:
-- > import System.Posix.RawFilePath.Directory

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Posix.RawFilePath.Directory
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
  , createDirIfMissing
  , createDirRecursive
  , createSymlink
  -- * File renaming/moving
  , renameFile
  , moveFile
  -- * File reading
  , readFile
  , readFileStream
  -- * File writing
  , writeFile
  , writeFileL
  , appendFile
  -- * File permissions
  , newFilePerms
  , newDirPerms
  -- * File checks
  , doesExist
  , doesFileExist
  , doesDirectoryExist
  , isReadable
  , isWritable
  , isExecutable
  , canOpenDirectory
  -- * File times
  , getModificationTime
  , setModificationTime
  , setModificationTimeHiRes
  -- * Directory reading
  , getDirsFiles
  , getDirsFiles'
  -- * Filetype operations
  , getFileType
  -- * Others
  , canonicalizePath
  , toAbs
  )
  where


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception.Safe
  (
    IOException
  , bracket
  , bracketOnError
  , throwIO
  , finally
  )
import Control.Monad
  (
    unless
  , void
  , when
  )
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.IfElse
  (
    unlessM
  )
import qualified Data.ByteString as BS
import Data.ByteString
  (
    ByteString
  )
import           Data.Traversable               ( for )
import           Data.Functor                   ( ($>) )
#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder
#else
import Data.ByteString.Lazy.Builder
#endif
  (
    Builder
  , byteString
  , toLazyByteString
  )
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe
  (
    unsafePackCStringFinalizer
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
import Data.Monoid
  (
    (<>)
  , mempty
  )
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, POSIXTime)
import Data.Word
  (
    Word8
  )
import Foreign.C.Error
  (
    eEXIST
  , eNOENT
  , eNOTEMPTY
  , eXDEV
  , getErrno
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
import Prelude hiding (appendFile, readFile, writeFile)
import Streamly
import Streamly.External.ByteString
import qualified Streamly.External.ByteString.Lazy as SL
import qualified Streamly.Data.Fold as FL
import Streamly.Memory.Array
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as SU
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Prelude as S
import qualified System.IO as SIO
import System.IO.Error
  (
    catchIOError
  , ioeGetErrorType
  )
import System.Posix.FilePath
import System.Posix.ByteString
  (
    exclusive
  )
import System.Posix.RawFilePath.Directory.Errors
import System.Posix.Directory.ByteString
  (
    createDirectory
  , closeDirStream
  , getWorkingDirectory
  , openDirStream
  , removeDirectory
  )
import System.Posix.RawFilePath.Directory.Traversals
  (
    getDirectoryContents'
  )
import System.Posix.Files.ByteString
  (
    createSymbolicLink
  , fileAccess
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
import qualified System.Posix.FilePath as FP
import qualified System.Posix.Files.ByteString as PF
import qualified "unix" System.Posix.IO.ByteString as SPI
import qualified "unix-bytestring" System.Posix.IO.ByteString as SPB
import System.Posix.FD
  (
    openFd
  )
import qualified System.Posix.RawFilePath.Directory.Traversals as SPDT
import qualified System.Posix.Foreign as SPDF
import qualified System.Posix.Process.ByteString as SPP
import System.Posix.Types
  (
    FileMode
  , ProcessID
  , Fd
  , EpochTime
  )
import System.Posix.Time





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



-- |The error mode for recursive operations.
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
--
-- Note: may call `getcwd` (only if destination is a relative path)
copyDirRecursive :: RawFilePath  -- ^ source dir
                 -> RawFilePath  -- ^ destination (parent dirs
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
    basename :: MonadFail m => RawFilePath -> m RawFilePath
    basename x = let b = takeBaseName x
                 in if BS.null b then fail ("No base name" :: String) else pure b

    go :: IORef [(RecursiveFailureHint, IOException)]
       -> RawFilePath -> RawFilePath -> IO ()
    go ce from destdir = do

      -- NOTE: order is important here, so we don't get empty directories
      -- on failure

      -- get the contents of the source dir
      contents <- handleIOE (ReadContentsFailed from destdir) ce [] $ do
        contents <- getDirsFiles from

        -- create the destination dir and
        -- only return contents if we succeed
        handleIOE (CreateDirFailed from destdir) ce [] $ do
          fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus from
          case cm of
            Strict    -> createDirectory destdir fmode'
            Overwrite -> catchIOError (createDirectory destdir
                                                       fmode')
                           $ \e ->
                             case ioeGetErrorType e of
                               AlreadyExists -> setFileMode destdir
                                                            fmode'
                               _             -> ioError e
          return contents

      -- NOTE: we can't use `easyCopy` here, because we want to call `go`
      -- recursively to skip the top-level sanity checks

      -- if reading the contents and creating the destination dir worked,
      -- then copy the contents to the destination too
      for_ contents $ \f -> do
        ftype <- getFileType f
        newdest <- (destdir </>) <$> basename f
        case ftype of
          SymbolicLink -> handleIOE (RecreateSymlinkFailed f newdest) ce ()
                            $ recreateSymlink f newdest cm
          Directory    -> go ce f newdest
          RegularFile  -> handleIOE (CopyFileFailed f newdest) ce ()
                            $ copyFile f newdest cm
          _            -> return ()

    -- helper to handle errors for both RecursiveErrorModes and return a
    -- default value
    handleIOE :: RecursiveFailureHint
              -> IORef [(RecursiveFailureHint, IOException)]
              -> a -> IO a -> IO a
    handleIOE hint ce def = case rm of
      FailEarly       -> handleIOError throwIO
      CollectFailures -> handleIOError (\e -> modifyIORef ce ((hint, e):)
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
--    - `AlreadyExists` if destination already exists
--
-- Throws in `Overwrite` mode only:
--
--    - `UnsatisfiedConstraints` if destination file is non-empty directory
--
-- Notes:
--
--    - calls `symlink`
--    - calls `getcwd` in Overwrite mode (if destination is a relative path)
recreateSymlink :: RawFilePath   -- ^ the old symlink file
                -> RawFilePath   -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink symsource newsym cm
  = do
    throwSameFile symsource newsym
    sympoint <- readSymbolicLink symsource
    case cm of
      Strict -> return ()
      Overwrite -> do
        writable <- do
                      e <- doesExist newsym
                      if e then isWritable newsym else pure False
        isfile   <- doesFileExist newsym
        isdir    <- doesDirectoryExist newsym
        when (writable && isfile) (deleteFile newsym)
        when (writable && isdir)  (deleteDir newsym)
    createSymbolicLink sympoint newsym


-- |Copies the given regular file to the given destination.
-- Neither follows symbolic links, nor accepts them.
-- For "copying" symbolic links, use `recreateSymlink` instead.
--
-- Note that this is still sort of a low-level function and doesn't
-- examine file types. For a more high-level version, use `easyCopy`
-- instead.
--
-- In `Overwrite` copy mode only overwrites actual files, not directories.
-- In `Strict` mode the destination file must not exist.
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
-- Notes:
--
--    - may call `getcwd` in Overwrite mode (if destination is a relative path)
copyFile :: RawFilePath   -- ^ source file
         -> RawFilePath   -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile from to cm = do
  throwSameFile from to
  bracket (do
            fd <- openFd from SPI.ReadOnly [SPDF.oNofollow] Nothing
            handle <- SPI.fdToHandle fd
            pure (fd, handle))
    (\(_, handle) -> SIO.hClose handle)
    $ \(fromFd, fH) -> do
                   sourceFileMode <- System.Posix.Files.ByteString.fileMode <$> getFdStatus fromFd
                   let dflags = [SPDF.oNofollow, case cm of
                                                      Strict    -> SPDF.oExcl
                                                      Overwrite -> SPDF.oTrunc]
                   bracketeer (do
                                fd <- openFd to SPI.WriteOnly dflags $ Just sourceFileMode
                                handle <- SPI.fdToHandle fd
                                pure (fd, handle))
                              (\(_, handle) -> SIO.hClose handle)
                              (\(_, handle) -> do
                                                 SIO.hClose handle
                                                 case cm of
                                                      -- if we created the file and copying failed, it's
                                                      -- safe to clean up
                                                      Strict -> deleteFile to
                                                      Overwrite -> pure ())
                              $ \(_, tH) -> do
                                           SIO.hSetBinaryMode fH True
                                           SIO.hSetBinaryMode tH True
                                           streamlyCopy (fH, tH)
  where
    streamlyCopy (fH, tH) = S.fold (FH.writeChunks tH) $ IFH.toChunksWithBufferOf (256*1024) fH

-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
--
-- Note: may call `getcwd` in Overwrite mode (if destination is a relative path)
easyCopy :: RawFilePath
         -> RawFilePath
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
--
-- Notes: calls `unlink`
deleteFile :: RawFilePath -> IO ()
deleteFile = removeLink


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
deleteDir :: RawFilePath -> IO ()
deleteDir = removeDirectory


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
deleteDirRecursive :: RawFilePath -> IO ()
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
      removeDirectory p


-- |Deletes a file, directory or symlink.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: RawFilePath -> IO ()
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
openFile :: RawFilePath
         -> IO ProcessID
openFile fp =
  SPP.forkProcess $ SPP.executeFile "xdg-open" True [fp] Nothing


-- |Executes a program with the given arguments. This forks a process.
executeFile :: RawFilePath     -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile fp args =
  SPP.forkProcess $ SPP.executeFile fp True args Nothing




    ---------------------
    --[ File Creation ]--
    ---------------------


-- |Create an empty regular file at the given directory with the given
-- filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createRegularFile :: FileMode -> RawFilePath -> IO ()
createRegularFile fm destBS =
  bracket (SPI.openFd destBS SPI.WriteOnly (Just fm)
                      (SPI.defaultFileFlags { exclusive = True }))
          SPI.closeFd
          (\_ -> return ())


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: FileMode -> RawFilePath -> IO ()
createDir fm destBS = createDirectory destBS fm

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: FileMode -> RawFilePath -> IO ()
createDirIfMissing fm destBS =
  hideError AlreadyExists $ createDirectory destBS fm


-- |Create an empty directory at the given directory with the given filename.
-- All parent directories are created with the same filemode. This
-- basically behaves like:
--
-- @
--   mkdir -p \/some\/dir
-- @
--
-- Safety/reliability concerns:
--
--    * not atomic
--
-- Throws:
--
--    - `PermissionDenied` if any part of the path components do not
--      exist and cannot be written to
--    - `AlreadyExists` if destination already exists and
--      is *not* a directory
--
-- Note: calls `getcwd` if the input path is a relative path
createDirRecursive :: FileMode -> RawFilePath -> IO ()
createDirRecursive fm p =
  go p
  where
    go :: RawFilePath -> IO ()
    go dest = do
      catchIOError (createDirectory dest fm) $ \e -> do
        errno <- getErrno
        case errno of
             en | en == eEXIST -> unlessM (doesDirectoryExist dest) (ioError e)
                | en == eNOENT -> createDirRecursive fm (takeDirectory dest)
                                  >> createDirectory dest fm
                | otherwise    -> ioError e


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
createSymlink :: RawFilePath     -- ^ destination file
              -> RawFilePath     -- ^ path the symlink points to
              -> IO ()
createSymlink destBS sympoint
  = createSymbolicLink sympoint destBS



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
--     - `AlreadyExists` if destination already exists
--     - `SameFile` if destination and source are the same file
--       (`HPathIOException`)
--
-- Note: calls `rename` (but does not allow to rename over existing files)
renameFile :: RawFilePath -> RawFilePath -> IO ()
renameFile fromf tof = do
  throwSameFile fromf tof
  throwFileDoesExist tof
  throwDirDoesExist tof
  rename fromf tof


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
--    - `AlreadyExists` if destination already exists
--
-- Notes:
--
--    - calls `rename` (but does not allow to rename over existing files)
--    - calls `getcwd` in Overwrite mode if destination is a relative path
moveFile :: RawFilePath   -- ^ file to move
         -> RawFilePath   -- ^ destination
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
      writable <- do
                    e <- doesFileExist to
                    if e then isWritable to else pure False

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





    --------------------
    --[ File Reading ]--
    --------------------


-- |Read the given file *at once* into memory as a lazy ByteString.
-- Symbolic links are followed, no sanity checks on file size
-- or file type. File must exist. Uses Builders under the hood
-- (hence lazy ByteString).
--
-- Safety/reliability concerns:
--
--    * the whole file is read into memory, this doesn't read lazily
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFile :: RawFilePath -> IO L.ByteString
readFile path = do
  stream <- readFileStream path
  toLazyByteString <$> S.fold FL.mconcat (fmap byteString stream)



-- | Open the given file as a filestream. Once the filestream is
-- exits, the filehandle is cleaned up.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStream :: RawFilePath
               -> IO (SerialT IO ByteString)
readFileStream fp = do
  fd <- openFd fp SPI.ReadOnly [] Nothing
  handle <- SPI.fdToHandle fd
  let stream = fmap fromArray (S.unfold (SU.finally SIO.hClose FH.readChunks) handle)
  pure stream




    --------------------
    --[ File Writing ]--
    --------------------


-- |Write a given ByteString to a file, truncating the file beforehand.
-- Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
writeFile :: RawFilePath
          -> Maybe FileMode  -- ^ if Nothing, file must exist
          -> ByteString
          -> IO ()
writeFile fp fmode bs =
  bracket (openFd fp SPI.WriteOnly [SPDF.oTrunc] fmode) (SPI.closeFd) $ \fd -> void $ SPB.fdWrite fd bs


-- |Write a given lazy ByteString to a file, truncating the file beforehand.
-- Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
--
-- Note: uses streamly under the hood
writeFileL :: RawFilePath
           -> Maybe FileMode  -- ^ if Nothing, file must exist
           -> L.ByteString
           -> IO ()
writeFileL fp fmode lbs = do
  handle <- bracketOnError (openFd fp SPI.WriteOnly [SPDF.oTrunc] fmode) (SPI.closeFd) $ SPI.fdToHandle
  finally (streamlyCopy handle) (SIO.hClose handle)
  where
    streamlyCopy tH = S.fold (FH.writeChunks tH) $ SL.toChunks lbs


-- |Append a given ByteString to a file.
-- The file must exist. Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
appendFile :: RawFilePath -> ByteString -> IO ()
appendFile fp bs =
  bracket (openFd fp SPI.WriteOnly [SPDF.oAppend] Nothing)
          (SPI.closeFd) $ \fd -> void $ SPB.fdWrite fd bs




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




    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesExist :: RawFilePath -> IO Bool
doesExist bs =
  catchErrno [eNOENT] (do
      _  <- PF.getSymbolicLinkStatus bs
      return $ True)
    $ return False


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: RawFilePath -> IO Bool
doesFileExist bs =
  catchErrno [eNOENT] (do
      fs  <- PF.getSymbolicLinkStatus bs
      return $ not . PF.isDirectory $ fs)
    $ return False


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: RawFilePath -> IO Bool
doesDirectoryExist bs =
  catchErrno [eNOENT] (do
      fs  <- PF.getSymbolicLinkStatus bs
      return $ PF.isDirectory fs)
    $ return False


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: RawFilePath -> IO Bool
isReadable bs = fileAccess bs True False False

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: RawFilePath -> IO Bool
isWritable bs = fileAccess bs False True False


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: RawFilePath -> IO Bool
isExecutable bs = fileAccess bs False False True



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: RawFilePath -> IO Bool
canOpenDirectory bs =
  handleIOError (\_ -> return False) $ do
    bracket (openDirStream bs)
            closeDirStream
            (\_ -> return ())
    return True




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: RawFilePath -> IO UTCTime
getModificationTime bs = do
  fs <- PF.getFileStatus bs
  pure $ posixSecondsToUTCTime $ PF.modificationTimeHiRes fs

setModificationTime :: RawFilePath -> EpochTime -> IO ()
setModificationTime bs t = do
  -- TODO: setFileTimes doesn't allow to pass NULL to utime
  ctime <- epochTime
  PF.setFileTimes bs ctime t

setModificationTimeHiRes :: RawFilePath -> POSIXTime -> IO ()
setModificationTimeHiRes bs t = do
  -- TODO: setFileTimesHiRes doesn't allow to pass NULL to utimes
  ctime <- getPOSIXTime
  PF.setFileTimesHiRes bs ctime t



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
getDirsFiles :: RawFilePath        -- ^ dir to read
             -> IO [RawFilePath]
getDirsFiles p = do
  contents <- getDirsFiles' p
  pure $ fmap (p </>) contents


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: RawFilePath        -- ^ dir to read
              -> IO [RawFilePath]
getDirsFiles' fp = do
  fd <- openFd fp SPI.ReadOnly [SPDF.oNofollow] Nothing
  rawContents <- getDirectoryContents' fd
  fmap catMaybes $ for rawContents $ \(_, f) ->
      if FP.isSpecialDirectoryEntry f
      then pure Nothing
      else pure $ Just f




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
getFileType :: RawFilePath -> IO FileType
getFileType fp = do
  fs <- PF.getSymbolicLinkStatus fp
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



-- |Applies `realpath` on the given path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
canonicalizePath :: RawFilePath -> IO RawFilePath
canonicalizePath = SPDT.realpath


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: RawFilePath -> IO RawFilePath
toAbs bs = do
  case isAbsolute bs of
    True  -> return bs
    False -> do
      cwd <- getWorkingDirectory
      return $ cwd </> bs

