-- |
-- Module      :  System.Posix.PosixFilePath.Directory
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
-- > import System.Posix.PosixFilePath.Directory

{-# LANGUAGE CPP              #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE FlexibleContexts #-} -- streamly

module System.Posix.PosixFilePath.Directory
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
  -- * File creation
  , createRegularFile
  , createDir
  , createDirIfMissing
  , createDirRecursive
  , createSymlink
  -- * File renaming/moving
  , renameFile
  , moveFile
  -- * File opening
  , openFile
  , openBinaryFile
  , withFile
  , withBinaryFile
  , withFile'
  , withBinaryFile'
  -- * File writing
  , writeFile
  , writeFile'
  , writeExistingFile
  , writeExistingFile'
  , appendFile
  , appendFile'
  , appendExistingFile
  , appendExistingFile'
  -- * File reading
  , readFile
  , readFile'
  , readExistingFile
  , readExistingFile'
  , readSymbolicLink
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
  , getDirsFilesRec
  , getDirsFiles'
  , getDirsFilesRec'
  , getDirsFilesStream
  , getDirsFilesStreamRec
  -- * CWD
  , getCurrentDirectory
  , setCurrentDirectory
  -- * Filetype operations
  , getFileType
  -- * Others
  , canonicalizePath
  , toAbs
  )
where


import           System.File.PlatformFilePath
import           Control.Exception.Safe         ( IOException
                                                , MonadCatch
                                                , MonadMask
                                                , bracket
                                                , onException
                                                , throwIO
                                                )
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail             as Fail
#else
import qualified Control.Monad                  as Fail
#endif
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.IfElse           ( unlessM )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import           Data.Foldable                  ( for_ )
import           Data.String
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.Time.Clock
import           Data.Time.Clock.POSIX          ( getPOSIXTime
                                                , posixSecondsToUTCTime
                                                , utcTimeToPOSIXSeconds
                                                , POSIXTime
                                                )
import           Foreign.C.Error                ( eEXIST
                                                , eNOENT
                                                , eNOTEMPTY
                                                , eXDEV
                                                , getErrno
                                                )
import           GHC.IO.Exception               ( IOErrorType(..) )
import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import           Streamly.Prelude               ( SerialT, MonadAsync )
import qualified Streamly.External.Posix.DirStream
                                               as SD
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.FileSystem.Handle
                                               as IFH
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as SE
import qualified Streamly.Prelude              as S
import           Control.Monad.IO.Class         ( liftIO
                                                )
import qualified System.IO                     as SIO
import           System.IO.Error                ( catchIOError
                                                , ioeGetErrorType
                                                )
import           System.Posix.ByteString        ( exclusive )
import           System.Posix.PosixFilePath.Directory.Errors
import           System.Posix.Directory.PosixFilePath
                                                ( createDirectory
                                                , closeDirStream
                                                , getWorkingDirectory
                                                , openDirStream
                                                , removeDirectory
                                                , changeWorkingDirectory
                                                )
import           System.Posix.Files.PosixString  ( createSymbolicLink
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
                                                , removeLink
                                                , rename
                                                , setFileMode
                                                , unionFileModes
                                                )
import qualified System.Posix.Files.PosixString as PF
import qualified System.Posix.IO.PosixString
                                               as SPI
import           System.Posix.FD                ( openFd )
import qualified System.Posix.PosixFilePath.Directory.Traversals
                                               as SPDT
import qualified System.Posix.Foreign          as SPDF
import           System.Posix.Types             ( FileMode
                                                )
import           System.Posix.Time

import System.AbstractFilePath.Posix
import System.OsString.Internal.Types
import System.Directory.Types
import System.Directory.Errors




    ----------------------------
    --[ Posix specific types ]--
    ----------------------------


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
copyDirRecursive :: PosixFilePath  -- ^ source dir
                 -> PosixFilePath  -- ^ destination (parent dirs
                                 --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive fromp destdirp cm rm = do
  ce <- newIORef []
  -- for performance, sanity checks are only done for the top dir
  throwSameFile fromp destdirp
  throwDestinationInSource fromp destdirp
  go ce fromp destdirp
  collectedExceptions <- readIORef ce
  unless (null collectedExceptions)
         (throwIO . RecursiveFailure $ collectedExceptions)
 where
#if MIN_VERSION_base(4,9,0)
  basename :: Fail.MonadFail m => PosixFilePath -> m PosixFilePath
#else
  basename :: Fail.Monad m => PosixFilePath -> m PosixFilePath
#endif
  basename x =
    let b = takeBaseName x
    in  if b == mempty then Fail.fail ("No base name" :: String) else pure b

  go :: IORef [(RecursiveFailureHint, IOException)]
     -> PosixFilePath
     -> PosixFilePath
     -> IO ()
  go ce from destdir = do

    -- NOTE: order is important here, so we don't get empty directories
    -- on failure

    -- get the contents of the source dir
    contents <- handleIOE (ReadContentsFailed (OsString from) (OsString destdir)) ce [] $ do
      contents <- getDirsFiles from

      -- create the destination dir and
      -- only return contents if we succeed
      handleIOE (CreateDirFailed (OsString from) (OsString destdir)) ce [] $ do
        fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus from
        case cm of
          Strict    -> createDirectory destdir fmode'
          Overwrite -> catchIOError (createDirectory destdir fmode') $ \e ->
            case ioeGetErrorType e of
              AlreadyExists -> setFileMode destdir fmode'
              _             -> ioError e
        return contents

    -- NOTE: we can't use `easyCopy` here, because we want to call `go`
    -- recursively to skip the top-level sanity checks

    -- if reading the contents and creating the destination dir worked,
    -- then copy the contents to the destination too
    for_ contents $ \f -> do
      ftype   <- getFileType f
      newdest <- (destdir </>) <$> basename f
      case ftype of
        SymbolicLink ->
          handleIOE (RecreateSymlinkFailed (OsString f) (OsString newdest)) ce ()
            $ recreateSymlink f newdest cm
        Directory -> go ce f newdest
        RegularFile ->
          handleIOE (CopyFileFailed (OsString f) (OsString newdest)) ce () $ copyFile f newdest cm
        _ -> return ()

  -- helper to handle errors for both RecursiveErrorModes and return a
  -- default value
  handleIOE :: RecursiveFailureHint
            -> IORef [(RecursiveFailureHint, IOException)]
            -> a
            -> IO a
            -> IO a
  handleIOE hint ce def = case rm of
    FailEarly -> handleIOError throwIO
    CollectFailures ->
      handleIOError (\e -> modifyIORef ce ((hint, e) :) >> return def)


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
recreateSymlink :: PosixFilePath   -- ^ the old symlink file
                -> PosixFilePath   -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink symsource newsym cm = do
  throwSameFile symsource newsym
  sympoint <- readSymbolicLink symsource
  case cm of
    Strict    -> return ()
    Overwrite -> do
      writable <- do
        e <- doesExist newsym
        if e then isWritable newsym else pure False
      isfile <- doesFileExist newsym
      isdir  <- doesDirectoryExist newsym
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
copyFile :: PosixFilePath   -- ^ source file
         -> PosixFilePath   -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile from to cm = do
  throwSameFile from to
  bracket
      (do
        fd     <- openFd from SPI.ReadOnly [SPDF.oNofollow] Nothing
        handle <- SPI.fdToHandle fd
        pure (fd, handle)
      )
      (\(_, handle) -> SIO.hClose handle)
    $ \(fromFd, fH) -> do
        sourceFileMode <- System.Posix.Files.PosixString.fileMode
          <$> getFdStatus fromFd
        let dflags =
              [ SPDF.oNofollow
              , case cm of
                Strict    -> SPDF.oExcl
                Overwrite -> SPDF.oTrunc
              ]
        bracketeer
            (do
              fd     <- openFd to SPI.WriteOnly dflags $ Just sourceFileMode
              handle <- SPI.fdToHandle fd
              pure (fd, handle)
            )
            (\(_, handle) -> SIO.hClose handle)
            (\(_, handle) -> do
              SIO.hClose handle
              case cm of
                   -- if we created the file and copying failed, it's
                   -- safe to clean up
                Strict    -> deleteFile to
                Overwrite -> pure ()
            )
          $ \(_, tH) -> do
              SIO.hSetBinaryMode fH True
              SIO.hSetBinaryMode tH True
              streamlyCopy (fH, tH)
 where
  streamlyCopy (fH, tH) =
    S.fold (FH.writeChunks tH) $ IFH.toChunksWithBufferOf (256 * 1024) fH

-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: PosixFilePath
         -> PosixFilePath
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
deleteFile :: PosixFilePath -> IO ()
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
deleteDir :: PosixFilePath -> IO ()
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
deleteDirRecursive :: PosixFilePath -> IO ()
deleteDirRecursive p = catchErrno [eNOTEMPTY, eEXIST] (deleteDir p) $ do
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
easyDelete :: PosixFilePath -> IO ()
easyDelete p = do
  ftype <- getFileType p
  case ftype of
    SymbolicLink -> deleteFile p
    Directory    -> deleteDirRecursive p
    RegularFile  -> deleteFile p
    _            -> return ()


    ------------------
    --[ File Write ]--
    ------------------

appendExistingFile :: PosixFilePath -> L.ByteString -> IO ()
appendExistingFile fp contents = withExistingFile fp SIO.AppendMode (`L.hPut` contents)

appendExistingFile' :: PosixFilePath -> BS.ByteString -> IO ()
appendExistingFile' fp contents = withExistingFile fp SIO.AppendMode (`BS.hPut` contents)


writeExistingFile :: PosixFilePath -> L.ByteString -> IO ()
writeExistingFile fp contents = withExistingFile fp SIO.WriteMode (`L.hPut` contents)

writeExistingFile' :: PosixFilePath -> BS.ByteString -> IO ()
writeExistingFile' fp contents = withExistingFile fp SIO.WriteMode (`BS.hPut` contents)

    --------------------
    --[ File Reading ]--
    --------------------

readExistingFile :: PosixFilePath -> IO L.ByteString
readExistingFile fp = withExistingFile' fp SIO.ReadMode L.hGetContents

readExistingFile' :: PosixFilePath -> IO BS.ByteString
readExistingFile' fp = withExistingFile fp SIO.ReadMode BS.hGetContents

-- | Read the target of a symbolic link.
readSymbolicLink :: PosixFilePath -> IO PosixString
readSymbolicLink = PF.readSymbolicLink



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
createRegularFile :: FileMode -> PosixFilePath -> IO ()
createRegularFile fm destBS = bracket
  (SPI.openFd destBS
              SPI.WriteOnly
              (SPI.defaultFileFlags { exclusive = True, SPI.creat = Just fm })
  )
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
createDir :: FileMode -> PosixFilePath -> IO ()
createDir fm destBS = createDirectory destBS fm

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: FileMode -> PosixFilePath -> IO ()
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
createDirRecursive :: FileMode -> PosixFilePath -> IO ()
createDirRecursive fm p = go p
 where
  go :: PosixFilePath -> IO ()
  go dest = do
    catchIOError (createDirectory dest fm) $ \e -> do
      errno <- getErrno
      case errno of
        en
          | en == eEXIST
          -> unlessM (doesDirectoryExist dest) (ioError e)
          | en == eNOENT
          -> go (takeDirectory $ dropTrailingPathSeparator dest)
            >> createDir fm dest
          | otherwise
          -> ioError e


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
createSymlink :: PosixFilePath     -- ^ destination file
              -> PosixFilePath     -- ^ path the symlink points to
              -> IO ()
createSymlink destBS sympoint = createSymbolicLink sympoint destBS



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
renameFile :: PosixFilePath -> PosixFilePath -> IO ()
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
moveFile :: PosixFilePath   -- ^ file to move
         -> PosixFilePath   -- ^ destination
         -> CopyMode
         -> IO ()
moveFile from to cm = do
  throwSameFile from to
  case cm of
    Strict -> catchErrno [eXDEV] (renameFile from to) $ do
      easyCopy from to Strict FailEarly
      easyDelete from
    Overwrite -> do
      ft       <- getFileType from
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







    -----------------------
    --[ File Permissions]--
    -----------------------


-- |Default permissions for a new file.
newFilePerms :: FileMode
newFilePerms =
                     ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode


-- |Default permissions for a new directory.
newDirPerms :: FileMode
newDirPerms =
                     ownerModes
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
doesExist :: PosixFilePath -> IO Bool
doesExist bs =
  catchErrno
      [eNOENT]
      (do
        _ <- PF.getSymbolicLinkStatus bs
        return $ True
      )
    $ return False


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: PosixFilePath -> IO Bool
doesFileExist bs =
  catchErrno
      [eNOENT]
      (do
        fs <- PF.getSymbolicLinkStatus bs
        return $ not . PF.isDirectory $ fs
      )
    $ return False


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: PosixFilePath -> IO Bool
doesDirectoryExist bs =
  catchErrno
      [eNOENT]
      (do
        fs <- PF.getSymbolicLinkStatus bs
        return $ PF.isDirectory fs
      )
    $ return False


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: PosixFilePath -> IO Bool
isReadable bs = fileAccess bs True False False

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: PosixFilePath -> IO Bool
isWritable bs = fileAccess bs False True False


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: PosixFilePath -> IO Bool
isExecutable bs = fileAccess bs False False True



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: PosixFilePath -> IO Bool
canOpenDirectory bs = handleIOError (\_ -> return False) $ do
  bracket (openDirStream bs) closeDirStream (\_ -> return ())
  return True




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: PosixFilePath -> IO UTCTime
getModificationTime bs = do
  fs <- PF.getFileStatus bs
  pure $ posixSecondsToUTCTime $ PF.modificationTimeHiRes fs

setModificationTime :: PosixFilePath -> UTCTime -> IO ()
setModificationTime bs t = do
  -- TODO: setFileTimes doesn't allow to pass NULL to utime
  ctime <- epochTime
  PF.setFileTimes bs ctime (fromInteger . floor . utcTimeToPOSIXSeconds $ t)

setModificationTimeHiRes :: PosixFilePath -> POSIXTime -> IO ()
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
getDirsFiles :: PosixFilePath        -- ^ dir to read
             -> IO [PosixFilePath]
getDirsFiles p = do
  contents <- getDirsFiles' p
  pure $ fmap (p </>) contents


getDirsFilesRec :: PosixFilePath        -- ^ dir to read
                -> IO [PosixFilePath]
getDirsFilesRec p = do
  contents <- getDirsFilesRec' p
  pure $ fmap (p </>) contents


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: PosixFilePath        -- ^ dir to read
              -> IO [PosixFilePath]
getDirsFiles' fp = getDirsFilesStream fp >>= S.toList


getDirsFilesRec' :: PosixFilePath        -- ^ dir to read
                       -> IO [PosixFilePath]
getDirsFilesRec' fp = getDirsFilesStreamRec fp >>= S.toList


getDirsFilesStreamRec :: (MonadCatch m, MonadAsync m, MonadMask m)
                      => PosixFilePath
                      -> IO (SerialT m PosixFilePath)
getDirsFilesStreamRec fp = do
  stream <- getDirsFilesStream fp
  pure $ S.concatMapM inner stream
 where
  inner f = do
    let nextFile = fp </> f
    isdir <- liftIO $ doesDirectoryExist nextFile
    if isdir
    then do
      stream <- liftIO (getDirsFilesStreamRec nextFile)
      pure $ SE.append (pure f) (fmap (f </>) stream)
    else pure (pure f)


-- | Like 'getDirsFiles'', except returning a Stream.
getDirsFilesStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                   => PosixFilePath
                   -> IO (SerialT m PosixFilePath)
getDirsFilesStream fp = do
  fd <- openFd fp SPI.ReadOnly [SPDF.oNofollow] Nothing
  ds <- SPDT.fdOpendir fd `onException` SPI.closeFd fd
  pure $ fmap snd $ SD.dirContentsStream ds


    -----------
    --[ CWD ]--
    -----------

getCurrentDirectory :: IO PosixFilePath
getCurrentDirectory = getWorkingDirectory

setCurrentDirectory :: PosixFilePath -> IO ()
setCurrentDirectory = changeWorkingDirectory



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
getFileType :: PosixFilePath -> IO FileType
getFileType fp = do
  fs <- PF.getSymbolicLinkStatus fp
  decide fs
 where
  decide fs | PF.isDirectory fs       = return Directory
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
canonicalizePath :: PosixFilePath -> IO PosixFilePath
canonicalizePath = SPDT.realpath


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: PosixFilePath -> IO PosixFilePath
toAbs bs = do
  case isAbsolute bs of
    True  -> return bs
    False -> do
      cwd <- getWorkingDirectory
      return $ cwd </> bs


withExistingFile :: PosixFilePath -> SIO.IOMode -> (SIO.Handle -> IO r) -> IO r
withExistingFile fp iomode = bracket
  (openExistingFile fp iomode)
  SIO.hClose

withExistingFile' :: PosixFilePath -> SIO.IOMode -> (SIO.Handle -> IO r) -> IO r
withExistingFile' fp iomode action = do
  h <- openExistingFile fp iomode
  action h

