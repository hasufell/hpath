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
  , readFileStrict
  , readFileStream
  -- * File writing
  , writeFile
  , writeFileL
  , appendFile
  -- * File permissions
  , RD.newFilePerms
  , RD.newDirPerms
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
  -- * Others
  , canonicalizePath
  , toAbs
  , withRawFilePath
  , withHandle
  , module System.Posix.RawFilePath.Directory.Errors
  )
where


import           Control.Exception.Safe         ( MonadMask
                                                , MonadCatch
                                                , bracketOnError
                                                , finally
                                                )
import           Control.Monad.Catch            ( MonadThrow(..) )

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Traversable               ( for )
import qualified Data.ByteString.Lazy          as L
import           Data.Time.Clock
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           Data.Word                      ( Word8 )
import           HPath
import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import           Streamly
import           Streamly.Memory.Array
import qualified System.IO                     as SIO
import           System.Posix.Directory.ByteString
                                                ( getWorkingDirectory )
import qualified "unix" System.Posix.IO.ByteString
                                               as SPI
import           System.Posix.FD                ( openFd )
import           System.Posix.RawFilePath.Directory.Errors
import           System.Posix.Types             ( FileMode
                                                , ProcessID
                                                , EpochTime
                                                )
import qualified System.Posix.RawFilePath.Directory
                                               as RD
import           System.Posix.RawFilePath.Directory
                                                ( FileType
                                                , RecursiveErrorMode
                                                , CopyMode
                                                )






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
copyDirRecursive :: Path b1  -- ^ source dir
                 -> Path b2  -- ^ destination (parent dirs
                             --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive (Path fromp) (Path destdirp) cm rm =
  RD.copyDirRecursive fromp destdirp cm rm


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
recreateSymlink :: Path b1   -- ^ the old symlink file
                -> Path b2   -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink (Path symsourceBS) (Path newsymBS) cm =
  RD.recreateSymlink symsourceBS newsymBS cm


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
copyFile :: Path b1   -- ^ source file
         -> Path b2   -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile (Path from) (Path to) cm = RD.copyFile from to cm

-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: Path b1 -> Path b2 -> CopyMode -> RecursiveErrorMode -> IO ()
easyCopy (Path from) (Path to) cm rm = RD.easyCopy from to cm rm






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
deleteFile (Path p) = RD.deleteFile p


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
deleteDir (Path p) = RD.deleteDir p


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
deleteDirRecursive (Path p) = RD.deleteDirRecursive p



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
easyDelete (Path p) = RD.easyDelete p




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open. The file type
-- is not checked. This forks a process.
openFile :: Path b -> IO ProcessID
openFile (Path fp) = RD.openFile fp


-- |Executes a program with the given arguments. This forks a process.
executeFile :: Path b          -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile (Path fp) args = RD.executeFile fp args




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
createRegularFile :: FileMode -> Path b -> IO ()
createRegularFile fm (Path destBS) = RD.createRegularFile fm destBS


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: FileMode -> Path b -> IO ()
createDir fm (Path destBS) = RD.createDir fm destBS

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: FileMode -> Path b -> IO ()
createDirIfMissing fm (Path destBS) = RD.createDirIfMissing fm destBS


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
createDirRecursive :: FileMode -> Path b -> IO ()
createDirRecursive fm (Path p) = RD.createDirRecursive fm p



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
createSymlink :: Path b     -- ^ destination file
              -> ByteString -- ^ path the symlink points to
              -> IO ()
createSymlink (Path destBS) sympoint = RD.createSymlink destBS sympoint



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
renameFile (Path from) (Path to) = RD.renameFile from to



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
moveFile (Path from) (Path to) cm = RD.moveFile from to cm





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
readFile (Path path) = RD.readFile path


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
readFileStrict (Path path) = RD.readFileStrict path


-- | Open the given file as a filestream. Once the filestream is
-- exits, the filehandle is cleaned up.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStream :: Path b -> IO (SerialT IO (Array Word8))
readFileStream (Path fp) = RD.readFileStream fp




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
          -> Maybe FileMode  -- ^ if Nothing, file must exist
          -> ByteString
          -> IO ()
writeFile (Path fp) fmode bs = RD.writeFile fp fmode bs


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
           -> Maybe FileMode  -- ^ if Nothing, file must exist
           -> L.ByteString
           -> IO ()
writeFileL (Path fp) fmode lbs = RD.writeFileL fp fmode lbs


-- |Append a given ByteString to a file.
-- The file must exist. Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
appendFile :: Path b -> ByteString -> IO ()
appendFile (Path fp) bs = RD.appendFile fp bs





    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesExist :: Path b -> IO Bool
doesExist (Path bs) = RD.doesExist bs


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: Path b -> IO Bool
doesFileExist (Path bs) = RD.doesFileExist bs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: Path b -> IO Bool
doesDirectoryExist (Path bs) = RD.doesDirectoryExist bs


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: Path b -> IO Bool
isReadable (Path bs) = RD.isReadable bs

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: Path b -> IO Bool
isWritable (Path bs) = RD.isWritable bs


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: Path b -> IO Bool
isExecutable (Path bs) = RD.isExecutable bs



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: Path b -> IO Bool
canOpenDirectory (Path bs) = RD.canOpenDirectory bs




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: Path b -> IO UTCTime
getModificationTime (Path bs) = RD.getModificationTime bs

setModificationTime :: Path b -> EpochTime -> IO ()
setModificationTime (Path bs) t = RD.setModificationTime bs t

setModificationTimeHiRes :: Path b -> POSIXTime -> IO ()
setModificationTimeHiRes (Path bs) t = RD.setModificationTimeHiRes bs t



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
--     - `PathParseException` if a filename could not be parsed (should never happen)
getDirsFiles :: Path b        -- ^ dir to read
             -> IO [Path b]
getDirsFiles p = do
  contents <- getDirsFiles' p
  pure $ fmap (p </>) contents


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: Path b        -- ^ dir to read
              -> IO [Path Rel]
getDirsFiles' (Path fp) = do
  rawContents <- RD.getDirsFiles' fp
  for rawContents $ \r -> parseRel r


-- | Like 'getDirsFiles'', except returning a Stream.
getDirsFilesStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                   => Path b
                   -> IO (SerialT m (Path Rel))
getDirsFilesStream (Path fp) = do
  s <- RD.getDirsFilesStream fp
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
getFileType (Path fp) = RD.getFileType fp



    --------------
    --[ Others ]--
    --------------



-- |Applies `realpath` on the given path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
--    - `PathParseException` if realpath does not return an absolute path
canonicalizePath :: Path b -> IO (Path Abs)
canonicalizePath (Path l) = do
  nl <- RD.canonicalizePath l
  parseAbs nl


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: Path b -> IO (Path Abs)
toAbs (Path bs) = do
  let mabs = parseAbs bs :: Maybe (Path Abs)
  case mabs of
    Just a  -> return a
    Nothing -> do
      cwd <- getWorkingDirectory >>= parseAbs
      r   <- parseRel bs -- we know it must be relative now
      return $ cwd </> r


-- | Helper function to use the Path library without
-- buying into the Path type too much. This uses 'parseAny'
-- under the hood and may throw `PathParseException`.
--
-- Throws:
--
--    - `PathParseException` if the bytestring could neither be parsed as
--      relative or absolute Path
withRawFilePath :: MonadThrow m
                => ByteString
                -> (Either (Path Abs) (Path Rel) -> m b)
                -> m b
withRawFilePath bs action = do
  path <- parseAny bs
  action path


-- | Convenience function to open the path as a handle.
--
-- If the file does not exist, it will be created with 'newFilePerms'.
--
-- Throws:
--
--    - `PathParseException` if the bytestring could neither be parsed as
--      relative or absolute Path
withHandle :: ByteString
           -> SPI.OpenMode
           -> ((SIO.Handle, Either (Path Abs) (Path Rel)) -> IO a)
           -> IO a
withHandle bs mode action = do
  path   <- parseAny bs
  handle <-
    bracketOnError (openFd bs mode [] (Just RD.newFilePerms)) (SPI.closeFd)
      $ SPI.fdToHandle
  finally (action (handle, path)) (SIO.hClose handle)
