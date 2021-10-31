-- |
-- Module      :  System.Win32.WindowsFilePath.Directory
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
-- > import System.Win32.WindowsFilePath.Directory

{-# LANGUAGE CPP              #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-} -- streamly

module System.Win32.WindowsFilePath.Directory
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
  -- * File reading
  , readFile
  , readFileStrict
  , readFileStream
  , readSymbolicLink
  -- * File writing
  , writeFile
  , writeFileL
  , appendFile
  -- * File permissions
  , setWriteMode
  , setFilePermissions
  , newFilePerms
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
  , windowsToPosixTime
  , posixToWindowsTime
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

#include <HsDirectoryConfig.h>
#if defined(mingw32_HOST_OS)
##if defined(i386_HOST_ARCH)
## define WINAPI stdcall
##elif defined(x86_64_HOST_ARCH)
## define WINAPI ccall
##else
## error unknown architecture
##endif
#include <shlobj.h>
#include <windows.h>
#include <System/Win32/WindowsFilePath/utility.h>
#include <System/Win32/WindowsFilePath/windows_ext.h>

import           Control.Exception.Safe         ( IOException
                                                , MonadCatch
                                                , MonadMask
                                                , bracket
                                                , bracketOnError
                                                , throwIO
                                                , finally
                                                , handleIO
                                                )
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail             as Fail
#else
import qualified Control.Monad                  as Fail
#endif
import           Control.Monad                  ( when
                                                )
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Control.Monad.IfElse           ( unlessM )
import qualified Data.ByteString               as BS
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as L
import           Data.Foldable                  ( for_ )
import           Data.String
import           Data.List.Split
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                )
import           Data.Time.Clock
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime
                                                , utcTimeToPOSIXSeconds
                                                , POSIXTime
                                                )
import           Data.Word                      ( Word8 )
import           GHC.IO.Exception               ( IOErrorType(..) )
import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import           Streamly.Prelude               ( SerialT, MonadAsync )
import           Streamly.Data.Array.Foreign
import qualified Streamly.External.ByteString as SB
import qualified Streamly.External.ByteString.Lazy
                                               as SL
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.Data.Unfold as SU
import qualified Streamly.Internal.Data.Array.Stream.Foreign
                                               as AS
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
import           Streamly.Internal.Data.Unfold.Type
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as SE
import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Array.Stream.Foreign (arraysOf)
import Streamly.Internal.Data.Array.Foreign.Mut.Type (defaultChunkSize)
import qualified Streamly.Prelude              as S
import qualified System.IO                     as SIO

import AFP.AbstractFilePath.Windows
import AFP.OsString.Internal.Types
import System.Directory.Types
import System.Directory.Errors
import Data.Bits
import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
import qualified System.Win32.WindowsString.Info as WS
import qualified System.Win32.WindowsString.SymbolicLink as WS
import Data.Maybe
import System.Environment
import Data.Char
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.Types
import Data.ByteString.Short.Word16 (packCWStringLen, ShortByteString)
import qualified Data.ByteString.Short.Word16 as W16
import System.IO.Error
import Data.Void




    ------------------------------
    --[ Windows specific types ]--
    ------------------------------


data FileType = Directory
              | DirectoryLink
              | SymbolicLink
              | File
  deriving (Eq, Show)



maxShareMode :: Win32.ShareMode
maxShareMode =
  Win32.fILE_SHARE_DELETE .|.
  Win32.fILE_SHARE_READ   .|.
  Win32.fILE_SHARE_WRITE

writeShareMode :: Win32.ShareMode
writeShareMode =
  Win32.fILE_SHARE_DELETE .|.
  Win32.fILE_SHARE_READ


data Win32_REPARSE_DATA_BUFFER
  = Win32_MOUNT_POINT_REPARSE_DATA_BUFFER ShortByteString ShortByteString
    -- ^ substituteName printName
  | Win32_SYMLINK_REPARSE_DATA_BUFFER ShortByteString ShortByteString Bool
    -- ^ substituteName printName isRelative
  | Win32_GENERIC_REPARSE_DATA_BUFFER


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
copyDirRecursive :: WindowsFilePath  -- ^ source dir
                 -> WindowsFilePath  -- ^ destination (parent dirs
                                 --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive fromp destdirp cm rm = do
  ce <- newIORef []
  -- for performance, sanity checks are only done for the top dir
  -- TODO
  -- throwSameFile fromp destdirp
  -- throwDestinationInSource fromp destdirp
  go ce fromp destdirp
  -- collectedExceptions <- readIORef ce
  -- unless (null collectedExceptions)
  --       (throwIO . RecursiveFailure $ collectedExceptions)
 where
#if MIN_VERSION_base(4,9,0)
  basename :: Fail.MonadFail m => WindowsFilePath -> m WindowsFilePath
#else
  basename :: Fail.Monad m => WindowsFilePath -> m WindowsFilePath
#endif
  basename x =
    let b = takeFileName x
    in  if b == mempty then Fail.fail ("No base name" :: String) else pure b

  go :: IORef [(RecursiveFailureHint, IOException)]
     -> WindowsFilePath
     -> WindowsFilePath
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
        fmode' <- WS.getFileAttributes from
        case cm of
          Strict    -> createDir destdir
          Overwrite -> catchIOError (createDir destdir) $ \e ->
            case ioeGetErrorType e of
              AlreadyExists -> pure ()
              _             -> ioError e
        WS.setFileAttributes destdir fmode'
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
        DirectoryLink ->
          handleIOE (RecreateSymlinkFailed (OsString f) (OsString newdest)) ce ()
            $ recreateSymlink f newdest cm
        Directory -> go ce f newdest
        File ->
          handleIOE (CopyFileFailed (OsString f) (OsString newdest)) ce () $ copyFile f newdest cm

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
recreateSymlink :: WindowsFilePath   -- ^ the old symlink file
                -> WindowsFilePath   -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink symsource newsym cm = do
  isdirSource  <- doesDirectoryExist symsource
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
  createSymlink newsym sympoint isdirSource


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
copyFile :: WindowsFilePath   -- ^ source file
         -> WindowsFilePath   -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile from to cm = WS.copyFile from to (cm == Strict)


-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: WindowsFilePath
         -> WindowsFilePath
         -> CopyMode
         -> RecursiveErrorMode
         -> IO ()
easyCopy from to cm rm = do
  ftype <- getFileType from
  case ftype of
    SymbolicLink  -> recreateSymlink from to cm
    Directory     -> copyDirRecursive from to cm rm
    DirectoryLink -> recreateSymlink from to cm
    File          -> copyFile from to cm





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
deleteFile :: WindowsFilePath -> IO ()
deleteFile = WS.deleteFile


-- |Deletes the given directory, which must be empty, never symlinks.
--
-- Throws:
--
--    - `InappropriateType` for wrong file type (symlink to directory)
--    - `InappropriateType` for wrong file type (regular file)
--    - `NoSuchThing` if directory does not exist
--    - `UnsatisfiedConstraints` if directory is not empty
--    - `PermissionDenied` if we can't open or write to parent directory
deleteDir :: WindowsFilePath -> IO ()
deleteDir = WS.removeDirectory


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
deleteDirRecursive :: WindowsFilePath -> IO ()
deleteDirRecursive p = catchIOError (deleteDir p) $ \e ->
  case ioeGetErrorType e of
    NoSuchThing -> rmRecursive p
    UnsatisfiedConstraints -> rmRecursive p
    _ -> throwIO e
 where
  rmRecursive fp = do
    files <- getDirsFiles fp
    for_ files $ \file -> do
      ftype <- getFileType file
      case ftype of
        SymbolicLink  -> deleteFile file
        Directory     -> deleteDirRecursive file
        DirectoryLink -> deleteDirRecursive file
        File          -> deleteFile file
    deleteDir fp
    

-- |Deletes a file, directory or symlink.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: WindowsFilePath -> IO ()
easyDelete p = do
  ftype <- getFileType p
  case ftype of
    SymbolicLink  -> deleteFile p
    Directory     -> deleteDirRecursive p
    DirectoryLink -> deleteDirRecursive p
    File          -> deleteFile p








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
createRegularFile :: Win32.AccessMode -> WindowsFilePath -> IO ()
createRegularFile mode fp = bracket open close (\_ -> return ())
 where
   open = WS.createFile
            fp
            mode
            maxShareMode
            Nothing
            Win32.cREATE_NEW
            Win32.fILE_ATTRIBUTE_NORMAL
            Nothing
   close = Win32.closeHandle


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: WindowsFilePath -> IO ()
createDir = flip WS.createDirectory Nothing


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: WindowsFilePath -> IO ()
createDirIfMissing = hideError AlreadyExists . createDir



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
createDirRecursive :: WindowsFilePath -> IO ()
createDirRecursive p = go p
 where
  go :: WindowsFilePath -> IO ()
  go dest = do
    catchIOError (createDir dest) $ \e -> do
      case ioeGetErrorType e of
        en
          | en == alreadyExistsErrorType
          -> unlessM (doesDirectoryExist dest) (ioError e)
          | en == doesNotExistErrorType
          -> go (takeDirectory $ dropTrailingPathSeparator dest)
            >> createDir dest
          | otherwise
          -> ioError e


-- |Create a symlink. And tries to do so in unprivileged mode (needs developer mode activated).
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination file already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createSymlink :: WindowsFilePath     -- ^ destination file
              -> WindowsFilePath     -- ^ path the symlink points to
              -> Bool                -- ^ whether this is a directory
              -> IO ()
createSymlink destBS sympoint dir =
  WS.createSymbolicLink' destBS sympoint ((if dir then Win32.sYMBOLIC_LINK_FLAG_DIRECTORY else Win32.sYMBOLIC_LINK_FLAG_FILE) .|. Win32.sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE)



    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


-- |Rename a given file with the provided filename. Destination and source
-- must be on the same device.
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `UnsupportedOperation` if source and destination are on different
--       devices
--     - `AlreadyExists` if destination already exists
renameFile :: WindowsFilePath -> WindowsFilePath -> IO ()
renameFile from to =
  WS.moveFileEx from (Just to) 0


-- |Move a file. This also works across devices by copy-delete fallback.
-- And also works on directories.
--
-- Safety/reliability concerns:
--
--    * `Overwrite` mode is not atomic
--    * copy-delete fallback is inherently non-atomic
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source directory cannot be opened
--     - `PermissionDenied` when moving one directory over another (even in Overwrite mode)
--
-- Throws in `Strict` mode only:
--
--    - `AlreadyExists` if destination already exists
moveFile :: WindowsFilePath   -- ^ file to move
         -> WindowsFilePath   -- ^ destination
         -> CopyMode
         -> IO ()
moveFile from to cm = do
  let flag = case cm of
        Strict -> Win32.mOVEFILE_COPY_ALLOWED
        Overwrite -> Win32.mOVEFILE_COPY_ALLOWED .|. Win32.mOVEFILE_REPLACE_EXISTING
  WS.moveFileEx from (Just to) flag




    --------------------
    --[ File Reading ]--
    --------------------


-- |Read the given file lazily.
--
-- Symbolic links are followed. File must exist.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFile :: WindowsFilePath -> IO L.ByteString
readFile path = do
  stream <- readFileStream path
  SL.fromChunksIO stream


-- |Read the given file strictly into memory.
--
-- Symbolic links are followed. File must exist.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStrict :: WindowsFilePath -> IO BS.ByteString
readFileStrict path = do
  stream <- readFileStream path
  SB.fromArray <$> AS.toArray stream


-- | Open the given file as a filestream. Once the filestream
-- exits, the filehandle is cleaned up.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStream :: WindowsFilePath -> IO (SerialT IO (Array Word8))
readFileStream fp = do
  handle <- bracketOnError
    (WS.createFile
      fp
      Win32.gENERIC_READ
      maxShareMode
      Nothing
      Win32.oPEN_EXISTING
      Win32.fILE_ATTRIBUTE_NORMAL
      Nothing)
    Win32.closeHandle
    Win32.hANDLEToHandle
  let stream = S.unfold (SU.finally SIO.hClose FH.readChunks) handle
  pure stream


foreign import WINAPI unsafe "windows.h DeviceIoControl"
  c_DeviceIoControl
    :: Win32.HANDLE
    -> Win32.DWORD
    -> Ptr a
    -> Win32.DWORD
    -> Ptr b
    -> Win32.DWORD
    -> Ptr Win32.DWORD
    -> Ptr Void
    -> IO Win32.BOOL


-- | Read the target of a symbolic link.
--
-- This is mostly stolen from 'directory' package.
readSymbolicLink :: WindowsFilePath -> IO WindowsFilePath
readSymbolicLink path = WS <$> do
  let open = WS.createFile path 0 maxShareMode Nothing Win32.oPEN_EXISTING
                              (Win32.fILE_FLAG_BACKUP_SEMANTICS .|.
                              win32_fILE_FLAG_OPEN_REPARSE_POINT) Nothing
  bracket open Win32.closeHandle $ \ h -> do
    win32_alloca_REPARSE_DATA_BUFFER $ \ ptrAndSize@(ptr, _) -> do
      result <- deviceIoControl h win32_fSCTL_GET_REPARSE_POINT
                                (nullPtr, 0) ptrAndSize Nothing
      case result of
        Left e | e == (#const ERROR_INVALID_FUNCTION) -> do
                   let msg = "Incorrect function. The file system " <>
                             "might not support symbolic links."
                   throwIO (mkIOError illegalOperationErrorType
                                      "DeviceIoControl" Nothing Nothing
                            `ioeSetErrorString` msg)
               | otherwise -> Win32.failWith "DeviceIoControl" e
        Right _ -> pure ()
      rData <- win32_peek_REPARSE_DATA_BUFFER ptr
      strip <$> case rData of
        Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn _ -> pure sn
        Win32_SYMLINK_REPARSE_DATA_BUFFER sn _ _ -> pure sn
        _ -> throwIO (mkIOError InappropriateType
                                "readSymbolicLink" Nothing Nothing)
 where
  strip sn = fromMaybe sn (W16.stripPrefix (unWFP $ fromString "\\??\\") sn)

  win32_iO_REPARSE_TAG_MOUNT_POINT, win32_iO_REPARSE_TAG_SYMLINK :: CULong
  win32_iO_REPARSE_TAG_MOUNT_POINT = (#const IO_REPARSE_TAG_MOUNT_POINT)
  win32_iO_REPARSE_TAG_SYMLINK = (#const IO_REPARSE_TAG_SYMLINK)

  win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE :: Win32.DWORD
  win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE =
    (#const MAXIMUM_REPARSE_DATA_BUFFER_SIZE)

  win32_sYMLINK_FLAG_RELATIVE :: CULong
  win32_sYMLINK_FLAG_RELATIVE = 0x00000001


  win32_fILE_FLAG_OPEN_REPARSE_POINT :: Win32.FileAttributeOrFlag
  win32_fILE_FLAG_OPEN_REPARSE_POINT = 0x00200000

  win32_fSCTL_GET_REPARSE_POINT :: Win32.DWORD
  win32_fSCTL_GET_REPARSE_POINT = 0x900a8

  deviceIoControl
    :: Win32.HANDLE
    -> Win32.DWORD
    -> (Ptr a, Int)
    -> (Ptr b, Int)
    -> Maybe Void
    -> IO (Either Win32.ErrCode Int)
  deviceIoControl h code (inPtr, inSize) (outPtr, outSize) _ = do
    with 0 $ \ lenPtr -> do
      ok <- c_DeviceIoControl h code inPtr (fromIntegral inSize) outPtr
                              (fromIntegral outSize) lenPtr nullPtr
      if ok
        then Right . fromIntegral <$> peek lenPtr
        else Left <$> Win32.getLastError

  win32_alloca_REPARSE_DATA_BUFFER
    :: ((Ptr Win32_REPARSE_DATA_BUFFER, Int) -> IO a) -> IO a
  win32_alloca_REPARSE_DATA_BUFFER action =
    allocaBytesAligned size align $ \ ptr ->
      action (ptr, size)
    where size = fromIntegral win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE
          -- workaround (hsc2hs for GHC < 8.0 don't support #{alignment ...})
          align = #{size char[alignof(HsDirectory_REPARSE_DATA_BUFFER)]}

  win32_peek_REPARSE_DATA_BUFFER
    :: Ptr Win32_REPARSE_DATA_BUFFER -> IO Win32_REPARSE_DATA_BUFFER
  win32_peek_REPARSE_DATA_BUFFER p = do
    tag <- #{peek HsDirectory_REPARSE_DATA_BUFFER, ReparseTag} p
    case () of
      _ | tag == win32_iO_REPARSE_TAG_MOUNT_POINT -> do
            let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                            MountPointReparseBuffer.PathBuffer} p
            sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.SubstituteNameOffset} p
            sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.SubstituteNameLength} p
            sn <- peekName buf sni sns
            pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.PrintNameOffset} p
            pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.PrintNameLength} p
            pn <- peekName buf pni pns
            pure (Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn pn)
        | tag == win32_iO_REPARSE_TAG_SYMLINK -> do
            let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                            SymbolicLinkReparseBuffer.PathBuffer} p
            sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.SubstituteNameOffset} p
            sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.SubstituteNameLength} p
            sn <- peekName buf sni sns
            pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.PrintNameOffset} p
            pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.PrintNameLength} p
            pn <- peekName buf pni pns
            flags <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                            SymbolicLinkReparseBuffer.Flags} p
            pure (Win32_SYMLINK_REPARSE_DATA_BUFFER sn pn
                  (flags .&. win32_sYMLINK_FLAG_RELATIVE /= 0))
        | otherwise -> pure Win32_GENERIC_REPARSE_DATA_BUFFER
    where
      peekName :: Ptr CWchar -> CUShort -> CUShort -> IO ShortByteString
      peekName buf offset size =
        packCWStringLen ( buf `plusPtr` fromIntegral offset
                        , fromIntegral size `div` sizeOf (0 :: CWchar) )




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
writeFile :: WindowsFilePath
          -> Bool             -- ^ True if file must exist
          -> ByteString
          -> IO ()
writeFile fp fmode bs =
  writeFileStream
    fp
    Win32.gENERIC_WRITE
    (if fmode then Win32.tRUNCATE_EXISTING else Win32.cREATE_ALWAYS)
    FH.writeChunks
    (arraysOf defaultChunkSize $ S.unfold SB.read bs)


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
writeFileL :: WindowsFilePath
           -> Bool             -- ^ True if file must exist
           -> L.ByteString
           -> IO ()
writeFileL fp fmode lbs =
  writeFileStream
    fp
    Win32.gENERIC_WRITE
    (if fmode then Win32.tRUNCATE_EXISTING else Win32.cREATE_ALWAYS)
    FH.writeChunks
    (SL.toChunks lbs)


writeFileStream :: WindowsFilePath
                -> Win32.AccessMode
                -> Win32.CreateMode
                -> (SIO.Handle -> Fold IO a ())   -- ^ writer
                -> SerialT IO a                   -- ^ stream
                -> IO ()
writeFileStream fp am cm writer stream = do
  handle <- bracketOnError
    (WS.createFile
      fp
      am
      writeShareMode
      Nothing
      cm
      Win32.fILE_ATTRIBUTE_NORMAL
      Nothing)
    Win32.closeHandle
    Win32.hANDLEToHandle
  finally (streamlyCopy handle) (SIO.hClose handle)
  where streamlyCopy tH = S.fold (writer tH) stream


-- |Append a given ByteString to a file.
-- The file must exist. Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
appendFile :: WindowsFilePath -> ByteString -> IO ()
appendFile fp bs = writeFileStream fp Win32.fILE_APPEND_DATA Win32.oPEN_ALWAYS FH.writeChunks
  (arraysOf defaultChunkSize $ S.unfold SB.read bs)



    -----------------------
    --[ File Permissions]--
    -----------------------


setWriteMode :: Bool -> Win32.FileAttributeOrFlag -> Win32.FileAttributeOrFlag
setWriteMode False m = m .|. Win32.fILE_ATTRIBUTE_READONLY
setWriteMode True  m = m .&. complement Win32.fILE_ATTRIBUTE_READONLY


-- | A restricted form of 'setFileMode' that only sets the permission bits.
-- For Windows, this means only the "read-only" attribute is affected.
setFilePermissions :: WindowsFilePath -> Win32.FileAttributeOrFlag -> IO ()
setFilePermissions path m = do
  m' <- Win32.bhfiFileAttributes <$> getFileMetadata path
  WS.setFileAttributes path ((m' .&. complement Win32.fILE_ATTRIBUTE_READONLY) .|.
                             (m  .&. Win32.fILE_ATTRIBUTE_READONLY))


-- |Default permissions for a new file.
newFilePerms :: Win32.AccessMode
newFilePerms = Win32.gENERIC_READ .|. Win32.gENERIC_WRITE




    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
--
-- Only NoSuchThing is catched (and returns False).
doesExist :: WindowsFilePath -> IO Bool
doesExist bs =
  handleIO (\e -> if NoSuchThing == ioeGetErrorType e then pure False else ioError e) $
    (const True) <$> getFileType bs


-- |Checks if the given file exists and is not a directory.
-- Does follow symlinks.
--
-- Only NoSuchThing is catched (and returns False).
doesFileExist :: WindowsFilePath -> IO Bool
doesFileExist bs =
  handleIO (\e -> if NoSuchThing == ioeGetErrorType e then pure False else ioError e) $
    (\ft -> ft == File || ft == SymbolicLink) <$> getFileType bs



-- |Checks if the given file exists and is a directory.
-- Does follow reparse points.
--
-- Only NoSuchThing is catched (and returns False).
doesDirectoryExist :: WindowsFilePath -> IO Bool
doesDirectoryExist bs = 
  handleIO (\e -> if NoSuchThing == ioeGetErrorType e then pure False else ioError e) $
    (\ft -> ft == Directory || ft == DirectoryLink) <$> getFileType bs



-- |Checks whether a file or folder is readable.
--
-- Throws:
--
--     - `NoSuchThing` if the file or folder does not exist
isReadable :: WindowsFilePath -> IO Bool
isReadable bs = (const True) <$> getFileType bs

-- |Checks whether a file or folder is writable.
--
-- Throws:
--
--     - `NoSuchThing` if the file or folder does not exist
isWritable :: WindowsFilePath -> IO Bool
isWritable bs = do
  fi <- getFileMetadata bs
  pure (hasWriteMode (Win32.bhfiFileAttributes fi))
 where
  hasWriteMode m = m .&. Win32.fILE_ATTRIBUTE_READONLY == 0


-- |Checks whether a file is executable. Returns 'False' on directories.
--
-- This looks up PATHEXT and compares the files extension with the list.
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: WindowsFilePath -> IO Bool
isExecutable bs = do
  getFileType bs >>= \case
    Directory -> pure False
    DirectoryLink -> pure False
    _ -> do
      let ext = takeExtension bs
      exeExts <- fmap toPlatformString
        . (fmap . fmap) toLower
        . (wordsBy (==';'))
        . fromMaybe ""
       <$> lookupEnv "PATHEXT"
      pure $ ext `elem` exeExts


-- |Checks whether the directory at the given path exists and can be
-- opened. Returns 'False' on non-directories.
canOpenDirectory :: WindowsFilePath -> IO Bool
canOpenDirectory bs = handleIOError (\_ -> return False) $ do
  let query = bs </> fromString "*"
  bracket
    (WS.findFirstFile query)
    (\(h, _) -> Win32.findClose h)
    (\_ -> return True)




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: WindowsFilePath -> IO UTCTime
getModificationTime bs = do
  m <- getFileMetadata bs
  pure $ posixSecondsToUTCTime $ windowsToPosixTime $ Win32.bhfiLastWriteTime m

setModificationTime :: WindowsFilePath -> UTCTime -> IO ()
setModificationTime fp t =
  bracket (WS.createFile fp Win32.fILE_WRITE_ATTRIBUTES maxShareMode Nothing Win32.oPEN_EXISTING Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing) Win32.closeHandle $ \h -> do
    Win32.setFileTime h Nothing Nothing (Just . posixToWindowsTime . utcTimeToPOSIXSeconds $ t)


setModificationTimeHiRes :: WindowsFilePath -> Win32.FILETIME -> IO ()
setModificationTimeHiRes fp t =
  bracket (WS.createFile fp Win32.fILE_WRITE_ATTRIBUTES maxShareMode Nothing Win32.oPEN_EXISTING Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing) Win32.closeHandle $ \h -> do
    Win32.setFileTime h Nothing Nothing (Just t)

-- https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-filetime
windowsToPosixTime :: Win32.FILETIME -> POSIXTime
windowsToPosixTime (Win32.FILETIME t) =
  (fromIntegral t - 116444736000000000) / 10000000

posixToWindowsTime :: POSIXTime -> Win32.FILETIME
posixToWindowsTime t = Win32.FILETIME $
  truncate (t * 10000000 + 116444736000000000)



    -------------------------
    --[ Directory reading ]--
    -------------------------


-- |Gets all filenames of the given directory.
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
getDirsFiles :: WindowsFilePath        -- ^ dir to read
             -> IO [WindowsFilePath]
getDirsFiles p = do
  contents <- getDirsFiles' p
  pure $ fmap (p </>) contents


getDirsFilesRec :: WindowsFilePath        -- ^ dir to read
                -> IO [WindowsFilePath]
getDirsFilesRec p = do
  contents <- getDirsFilesRec' p
  pure $ fmap (p </>) contents


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: WindowsFilePath        -- ^ dir to read
              -> IO [WindowsFilePath]
getDirsFiles' fp = getDirsFilesStream fp >>= S.toList


getDirsFilesRec' :: WindowsFilePath        -- ^ dir to read
                       -> IO [WindowsFilePath]
getDirsFilesRec' fp = getDirsFilesStreamRec fp >>= S.toList


getDirsFilesStreamRec :: (MonadCatch m, MonadAsync m, MonadMask m)
                      => WindowsFilePath
                      -> IO (SerialT m WindowsFilePath)
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
                   => WindowsFilePath
                   -> IO (SerialT m WindowsFilePath)
getDirsFilesStream fp = do
  let query = fp </> fromString "*"
  t <- WS.findFirstFile query
  let stream = S.unfold (SU.finally (liftIO . Win32.findClose . fst) unfoldDirContents) $ (fmap Just t)
  pure stream
 where
  unfoldDirContents :: MonadIO m => Unfold m (Win32.HANDLE, Maybe Win32.FindData) WindowsFilePath
  unfoldDirContents = Unfold step return
   where
    {-# INLINE [0] step #-}
    step (_, Nothing) = pure D.Stop
    step (handle, Just fd) = do
      filename <- liftIO $ WS.getFindDataFileName fd
      more <- liftIO $ Win32.findNextFile handle fd
      pure $ case () of
                 _
                  | [fromChar '.'] == unpackPlatformString filename -> D.Skip (handle, if more then Just fd else Nothing)
                  | [fromChar '.', fromChar '.'] == unpackPlatformString filename -> D.Skip (handle, if more then Just fd else Nothing)
                  | otherwise -> D.Yield filename (handle, if more then Just fd else Nothing)



    -----------
    --[ CWD ]--
    -----------

getCurrentDirectory :: IO WindowsFilePath
getCurrentDirectory = WS.getCurrentDirectory

setCurrentDirectory :: WindowsFilePath -> IO ()
setCurrentDirectory = WS.setCurrentDirectory



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
getFileType :: WindowsFilePath -> IO FileType
getFileType fp = do
      fi <- getFileMetadata fp
      pure $ decide fi
 where
  attrs  fi = Win32.bhfiFileAttributes fi
  isLink fi = attrs fi .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT /= 0
  isDir  fi = attrs fi .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
  decide fi | isLink fi && isDir fi  = DirectoryLink
            | isLink fi              = SymbolicLink
            | isDir fi               = Directory
            | otherwise              = File


getFileMetadata :: WindowsFilePath -> IO Win32.BY_HANDLE_FILE_INFORMATION
getFileMetadata fp = do
    bracket (WS.createFile fp 0 maxShareMode Nothing Win32.oPEN_EXISTING Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing)
      Win32.closeHandle $ \h -> Win32.getFileInformationByHandle h



    --------------
    --[ Others ]--
    --------------



-- |Applies `GetFullPathName` on the given path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
canonicalizePath :: WindowsFilePath -> IO WindowsFilePath
canonicalizePath = WS.getFullPathName


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: WindowsFilePath -> IO WindowsFilePath
toAbs bs = do
  case isAbsolute bs of
    True  -> return bs
    False -> do
      cwd <- getCurrentDirectory
      return $ cwd </> bs



#endif
