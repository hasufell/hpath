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
-- copy, delete, move and so on. It only operates on /Path x/ which
-- guarantees us well-typed paths. This is a thin wrapper over
-- System.Posix.RawFilePath.Directory in 'hpath-directory'. It's
-- encouraged to use this module.
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

{-# LANGUAGE FlexibleContexts #-} -- streamly
{-# LANGUAGE PackageImports   #-}

module HPath.IO
  (
  -- * Types
    module System.Directory.Types
  , Permissions
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
  -- * File writing
  , writeFile
  , writeFileL
  , appendFile
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
  , getDirsFilesStream
  -- * Filetype operations
  , getFileType
  -- * Permissions
  , getPermissions
  , setPermissions
  , emptyPermissions
  , setOwnerReadable
  , setOwnerWritable
  , setOwnerExecutable
  , setOwnerSearchable
  , newFilePerms
  , newDirPerms
  -- * Others
  , canonicalizePath
  , toAbs
  )
where

import           HPath
import           HPath.Internal
import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import AFP.AbstractFilePath.Types
import AFP.OsString.Internal.Types
import Control.Exception.Safe ( MonadCatch, MonadMask)
import Control.Monad.Catch
import Data.Bits
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Traversable
import Data.Word                      ( Word8 )
import Streamly.Data.Array.Foreign
import Streamly.Prelude               ( SerialT, MonadAsync )
import System.Directory.Types
import System.Directory.AFP (
    Permissions
  , emptyPermissions
  , setOwnerReadable
  , setOwnerWritable
  , setOwnerExecutable
  , setOwnerSearchable
  , newFilePerms
  , newDirPerms
  )

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as BS
import qualified System.Directory.AFP as Dir



    ------------------------
    --[ File Permissions ]--
    ------------------------


-- | Get the permissions of a file or directory.
--
-- On Windows, the 'writable' permission corresponds to the "read-only"
-- attribute.  The 'executable' permission is set if the file extension is of
-- an executable file type.  The 'readable' permission is always set.
--
-- On POSIX systems, this returns the result of @access@.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to access the
--   permissions, or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
getPermissions :: Path b -> IO Permissions
getPermissions (MkPath b) = Dir.getPermissions b


setPermissions :: Path b -> Permissions -> IO ()
setPermissions (MkPath b) perms = Dir.setPermissions b perms



    --------------------
    --[ File Copying ]--
    --------------------


copyDirRecursive :: Path b1 -- ^ source dir
                 -> Path b2 -- ^ destination (parent dirs
                                 --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive (MkPath fromp) (MkPath destdirp) cm rm =
  Dir.copyDirRecursive fromp destdirp cm rm


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
recreateSymlink :: Path b1  -- ^ the old symlink file
                -> Path b2  -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink (MkPath symsource) (MkPath newsym) cm =
  Dir.recreateSymlink symsource newsym cm


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
copyFile :: Path b1  -- ^ source file
         -> Path b2  -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile (MkPath from) (MkPath to) cm =
  Dir.copyFile from to cm


-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: Path b1
         -> Path b2
         -> CopyMode
         -> RecursiveErrorMode
         -> IO ()
easyCopy (MkPath from) (MkPath to) cm rm =
  Dir.easyCopy from to cm rm





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
deleteFile :: Path b -> IO ()
deleteFile (MkPath fp) =
  Dir.deleteFile fp


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
deleteDir :: Path b -> IO ()
deleteDir (MkPath fp) = Dir.deleteDir fp


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
deleteDirRecursive :: Path b -> IO ()
deleteDirRecursive (MkPath p) = Dir.deleteDirRecursive p


-- |Deletes a file, directory or symlink.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: Path b -> IO ()
easyDelete (MkPath p) = Dir.easyDelete p




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
createRegularFile :: Path b -> IO ()
createRegularFile (MkPath destBS) = Dir.createRegularFile destBS


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: Path b -> IO ()
createDir (MkPath destBS) = Dir.createDir destBS

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: Path b -> IO ()
createDirIfMissing (MkPath destBS) = Dir.createDirIfMissing destBS


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
createDirRecursive :: Path b -> IO ()
createDirRecursive (MkPath p) = Dir.createDirRecursive p


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
createSymlink :: Path b1     -- ^ destination file
              -> Path b2     -- ^ path the symlink points to
              -> IO ()
createSymlink (MkPath destBS) (MkPath sympoint) = Dir.createSymlink destBS sympoint



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
renameFile :: Path b1 -> Path b2 -> IO ()
renameFile (MkPath fromf) (MkPath tof) = Dir.renameFile fromf tof


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
moveFile :: Path b1   -- ^ file to move
         -> Path b2   -- ^ destination
         -> CopyMode
         -> IO ()
moveFile (MkPath from) (MkPath to) cm = Dir.moveFile from to cm





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
readFile :: Path b -> IO L.ByteString
readFile (MkPath path) = Dir.readFile path


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
readFileStrict :: Path b -> IO BS.ByteString
readFileStrict (MkPath path) = Dir.readFileStrict path


-- | Open the given file as a filestream. Once the filestream
-- exits, the filehandle is cleaned up.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStream :: Path b -> IO (SerialT IO (Array Word8))
readFileStream (MkPath fp) = Dir.readFileStream fp




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
writeFile :: Path b
          -> Bool             -- ^ True if file must exist
          -> BS.ByteString
          -> IO ()
writeFile (MkPath fp) nocreat bs = Dir.writeFile fp nocreat bs


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
writeFileL :: Path b
           -> Bool             -- ^ True if file must exist
           -> L.ByteString
           -> IO ()
writeFileL (MkPath fp) nocreat lbs = Dir.writeFileL fp nocreat lbs


-- |Append a given ByteString to a file.
-- The file must exist. Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
appendFile :: Path b -> BS.ByteString -> IO ()
appendFile (MkPath fp) bs = Dir.appendFile fp bs



    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesExist :: Path b -> IO Bool
doesExist (MkPath bs) = Dir.doesExist bs


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: Path b -> IO Bool
doesFileExist (MkPath bs) = Dir.doesFileExist bs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: Path b -> IO Bool
doesDirectoryExist (MkPath bs) = Dir.doesDirectoryExist bs


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: Path b -> IO Bool
isReadable (MkPath bs) = Dir.isReadable bs

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: Path b -> IO Bool
isWritable (MkPath bs) = Dir.isWritable bs


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: Path b -> IO Bool
isExecutable (MkPath bs) = Dir.isExecutable bs



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: Path b -> IO Bool
canOpenDirectory (MkPath bs) = Dir.canOpenDirectory bs




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: Path b -> IO UTCTime
getModificationTime (MkPath bs) = Dir.getModificationTime bs

setModificationTime :: Path b -> UTCTime -> IO ()
setModificationTime (MkPath bs) t = Dir.setModificationTime bs t

setModificationTimeHiRes :: Path b -> POSIXTime -> IO ()
setModificationTimeHiRes (MkPath bs) t = Dir.setModificationTimeHiRes bs t



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
getDirsFiles :: Path b        -- ^ dir to read
             -> IO [Path b]
getDirsFiles (MkPath p) = fmap MkPath <$> Dir.getDirsFiles p


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: Path b        -- ^ dir to read
              -> IO [Path Rel]
getDirsFiles' (MkPath fp) = do
  rawContents <- Dir.getDirsFiles' fp
  for rawContents $ \r -> parseRel r


-- | Like 'getDirsFiles'', except returning a Stream.
getDirsFilesStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                   => Path b
                   -> IO (SerialT m (Path Rel))
getDirsFilesStream (MkPath fp) = do
  s <- Dir.getDirsFilesStream fp
  pure (s >>= parseRel)




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
getFileType :: Path b -> IO FileType
getFileType (MkPath fp) = Dir.getFileType fp



    --------------
    --[ Others ]--
    --------------



-- |Applies `realpath` on the given path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
canonicalizePath :: Path b -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- Dir.canonicalizePath l
  parseAbs nl


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: Path b -> IO (Path Abs)
toAbs (MkPath bs) = MkPath <$> Dir.toAbs bs


-- | Helper function to use the Path library without
-- buying into the Path type too much. This uses 'parseAny'
-- under the hood and may throw `PathParseException`.
--
-- Throws:
--
--    - `PathParseException` if the bytestring could neither be parsed as
--      relative or absolute Path
withRawFilePath :: MonadThrow m
                => AbstractFilePath
                -> (Either (Path Abs) (Path Rel) -> m b)
                -> m b
withRawFilePath bs action = do
  path <- parseAny bs
  action path

