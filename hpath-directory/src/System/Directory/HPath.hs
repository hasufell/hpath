{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Directory.HPath
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
  , appendFile
  , appendFile'
  -- * File reading
  , readFile
  , readFile'
  , readSymbolicLink
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
  -- * Permissions
  , getPermissions
  , setPermissions
  , AFP.emptyPermissions
  , AFP.setOwnerReadable
  , AFP.setOwnerWritable
  , AFP.setOwnerExecutable
  , AFP.setOwnerSearchable
  , AFP.newFilePerms
  , AFP.newDirPerms
  -- * Others
  , canonicalizePath
  , toAbs
  , getFileType
  , AFP.FileType
  )
  where

import           System.Directory.OsPath (Permissions)
import qualified System.Directory.OsPath as AFP
import           System.File.OsPath
import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )

import HPath
import HPath.Internal
import System.Directory.Types
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Streamly.Prelude               ( SerialT, MonadAsync )

import           Control.Exception.Safe         ( MonadCatch
                                                , MonadMask
                                                )







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
getPermissions (MkPath path) = AFP.getPermissions path

getFileType :: Path b -> IO AFP.FileType
getFileType (MkPath path) = AFP.getFileType path

setPermissions :: Path b -> Permissions -> IO ()
setPermissions (MkPath path) = AFP.setPermissions path



    --------------------
    --[ File Copying ]--
    --------------------


copyDirRecursive :: Path b1  -- ^ source dir
                 -> Path b2  -- ^ destination (parent dirs
                                 --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive (MkPath fromp) (MkPath destdirp) cm rm =
  AFP.copyDirRecursive fromp destdirp cm rm


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
  AFP.recreateSymlink symsource newsym cm


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
copyFile (MkPath from) (MkPath to) cm =
  AFP.copyFile from to cm


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
  AFP.easyCopy from to cm rm





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
  AFP.deleteFile fp


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
deleteDir (MkPath fp) = AFP.deleteDir fp


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
deleteDirRecursive (MkPath p) = AFP.deleteDirRecursive p


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
easyDelete (MkPath p) = AFP.easyDelete p



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
createRegularFile (MkPath destBS) = AFP.createRegularFile destBS


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: Path b -> IO ()
createDir (MkPath destBS) = AFP.createDir destBS

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: Path b -> IO ()
createDirIfMissing (MkPath destBS) =
  AFP.createDirIfMissing destBS


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
createDirRecursive (MkPath p) = AFP.createDirRecursive p


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
              -> Bool                 -- ^ whether this is a dir (irrelevant on posix)
              -> IO ()
createSymlink (MkPath destBS) (MkPath sympoint) dir =
  AFP.createSymlink destBS sympoint dir



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
renameFile (MkPath fromf) (MkPath tof) = AFP.renameFile fromf tof


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
moveFile (MkPath from) (MkPath to) cm = AFP.moveFile from to cm





    --------------------
    --[ File Reading ]--
    --------------------



-- | Read the target of a symbolic link.
readSymbolicLink :: Path b1 -> IO (Path b2)
readSymbolicLink (MkPath fp) = MkPath <$> AFP.readSymbolicLink fp



    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesExist :: Path b -> IO Bool
doesExist (MkPath bs) = AFP.doesExist bs


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: Path b -> IO Bool
doesFileExist (MkPath bs) = AFP.doesFileExist bs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: Path b -> IO Bool
doesDirectoryExist (MkPath bs) = AFP.doesDirectoryExist bs


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: Path b -> IO Bool
isReadable (MkPath bs) = AFP.isReadable bs

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: Path b -> IO Bool
isWritable (MkPath bs) = AFP.isWritable bs


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: Path b -> IO Bool
isExecutable (MkPath bs) = AFP.isExecutable bs



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: Path b -> IO Bool
canOpenDirectory (MkPath bs) = AFP.canOpenDirectory bs




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: Path b -> IO UTCTime
getModificationTime (MkPath bs) = AFP.getModificationTime bs

setModificationTime :: Path b -> UTCTime -> IO ()
setModificationTime (MkPath bs) t = AFP.setModificationTime bs t

setModificationTimeHiRes :: Path b -> POSIXTime -> IO ()
setModificationTimeHiRes (MkPath bs) t = AFP.setModificationTimeHiRes bs t



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
getDirsFiles (MkPath p) = fmap MkPath <$> AFP.getDirsFiles p


getDirsFilesRec :: Path b        -- ^ dir to read
                -> IO [Path b]
getDirsFilesRec (MkPath p) = fmap MkPath <$> AFP.getDirsFilesRec p


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: Path b        -- ^ dir to read
              -> IO [Path Rel]
getDirsFiles' (MkPath fp) = fmap MkPath <$> AFP.getDirsFiles' fp


getDirsFilesRec' :: Path b        -- ^ dir to read
                 -> IO [Path Rel]
getDirsFilesRec' (MkPath p) = fmap MkPath <$> AFP.getDirsFilesRec' p


getDirsFilesStreamRec :: (MonadCatch m, MonadAsync m, MonadMask m)
                      => Path b
                      -> IO (SerialT m (Path Rel))
getDirsFilesStreamRec (MkPath fp) = fmap MkPath <$> AFP.getDirsFilesStreamRec fp


-- | Like 'getDirsFiles'', except returning a Stream.
getDirsFilesStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                   => Path b
                   -> IO (SerialT m (Path Rel))
getDirsFilesStream (MkPath fp) = fmap MkPath <$> AFP.getDirsFilesStream fp


    -----------
    --[ CWD ]--
    -----------

getCurrentDirectory :: IO (Path b)
getCurrentDirectory = MkPath <$> AFP.getCurrentDirectory

setCurrentDirectory :: Path b -> IO ()
setCurrentDirectory (MkPath fp) = AFP.setCurrentDirectory fp




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
canonicalizePath (MkPath fp) = MkPath <$> AFP.canonicalizePath fp


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: Path b -> IO (Path Abs)
toAbs (MkPath bs) = MkPath <$> AFP.toAbs bs

