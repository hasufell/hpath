{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Directory.AFP
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
  -- * CWD
  , getCurrentDirectory
  , setCurrentDirectory
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

import           Prelude                 hiding ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import Data.Bits
import System.Directory.Types
#ifdef WINDOWS
import qualified System.Win32.WindowsFilePath.Directory as Dir
#else
import qualified System.Posix.PosixFilePath.Directory as Dir
import qualified System.Posix as Posix (FileMode)
import qualified System.Posix.Files.ByteString as Posix
import qualified Data.ByteString.Short as SBS
#endif
import AFP.AbstractFilePath.Types
import AFP.OsString.Internal.Types
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Streamly.Prelude               ( SerialT, MonadAsync )
import           Streamly.Data.Array.Foreign
import           Data.Word                      ( Word8 )

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as BS
import           Control.Exception.Safe         ( MonadCatch
                                                , MonadMask
                                                )


    ------------------------
    --[ File Permissions ]--
    ------------------------



data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)


emptyPermissions :: Permissions
emptyPermissions = Permissions {
                       readable   = False,
                       writable   = False,
                       executable = False,
                       searchable = False
                   }

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable b p = p { readable = b }

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable b p = p { writable = b }

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable b p = p { executable = b }

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable b p = p { searchable = b }


-- |Default permissions for a new file.
newFilePerms :: Permissions
newFilePerms = Permissions {
                       readable   = True,
                       writable   = True,
                       executable = False,
                       searchable = False
                   }


-- |Default permissions for a new directory.
newDirPerms :: Permissions
newDirPerms = Permissions {
                       readable   = True,
                       writable   = True,
                       executable = False,
                       searchable = True
                   }


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
getPermissions :: AbstractFilePath -> IO Permissions
#ifdef WINDOWS
getPermissions _ =
  undefined
#else
getPermissions (OsString (PS path')) = do
  let path = SBS.fromShort path'
  m <- Posix.getFileStatus path
  let isDir = Posix.isDirectory m
  r <- Posix.fileAccess path True  False False
  w <- Posix.fileAccess path False True  False
  x <- Posix.fileAccess path False False True
  pure Permissions
       { readable   = r
       , writable   = w
       , executable = x && not isDir
       , searchable = x && isDir
       }
#endif

setPermissions :: AbstractFilePath -> Permissions -> IO ()
#ifdef WINDOWS
setPermissions = undefined
#else
setPermissions (OsString (PS path')) (Permissions r w e s) = do
  let path = SBS.fromShort path'
  m <- Posix.getFileStatus path
  Posix.setFileMode path (modifyBit (e || s) Posix.ownerExecuteMode .
                          modifyBit w Posix.ownerWriteMode .
                          modifyBit r Posix.ownerReadMode .
                          Posix.fileMode $ m)
  where
    modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b
#endif



    --------------------
    --[ File Copying ]--
    --------------------


copyDirRecursive :: AbstractFilePath  -- ^ source dir
                 -> AbstractFilePath  -- ^ destination (parent dirs
                                 --   are not automatically created)
                 -> CopyMode
                 -> RecursiveErrorMode
                 -> IO ()
copyDirRecursive (OsString fromp) (OsString destdirp) cm rm =
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
recreateSymlink :: AbstractFilePath   -- ^ the old symlink file
                -> AbstractFilePath   -- ^ destination file
                -> CopyMode
                -> IO ()
recreateSymlink (OsString symsource) (OsString newsym) cm =
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
copyFile :: AbstractFilePath   -- ^ source file
         -> AbstractFilePath   -- ^ destination file
         -> CopyMode
         -> IO ()
copyFile (OsString from) (OsString to) cm =
  Dir.copyFile from to cm


-- |Copies a regular file, directory or symbolic link. In case of a
-- symbolic link it is just recreated, even if it points to a directory.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: AbstractFilePath
         -> AbstractFilePath
         -> CopyMode
         -> RecursiveErrorMode
         -> IO ()
easyCopy (OsString from) (OsString to) cm rm =
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
deleteFile :: AbstractFilePath -> IO ()
deleteFile (OsString fp) =
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
deleteDir :: AbstractFilePath -> IO ()
deleteDir (OsString fp) = Dir.deleteDir fp


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
deleteDirRecursive :: AbstractFilePath -> IO ()
deleteDirRecursive (OsString p) = Dir.deleteDirRecursive p


-- |Deletes a file, directory or symlink.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
-- Any other file type is ignored.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: AbstractFilePath -> IO ()
easyDelete (OsString p) = Dir.easyDelete p




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
createRegularFile :: AbstractFilePath -> IO ()
createRegularFile (OsString destBS) = Dir.createRegularFile Dir.newFilePerms destBS


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination already exists
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDir :: AbstractFilePath -> IO ()
createDir (OsString destBS) = Dir.createDir Dir.newDirPerms destBS

-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `NoSuchThing` if any of the parent components of the path
--      do not exist
createDirIfMissing :: AbstractFilePath -> IO ()
createDirIfMissing (OsString destBS) = Dir.createDirIfMissing Dir.newDirPerms destBS


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
createDirRecursive :: AbstractFilePath -> IO ()
createDirRecursive (OsString p) = Dir.createDirRecursive Dir.newDirPerms p


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
createSymlink :: AbstractFilePath     -- ^ destination file
              -> AbstractFilePath     -- ^ path the symlink points to
              -> IO ()
createSymlink (OsString destBS) (OsString sympoint) = Dir.createSymlink destBS sympoint



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
renameFile :: AbstractFilePath -> AbstractFilePath -> IO ()
renameFile (OsString fromf) (OsString tof) = Dir.renameFile fromf tof


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
moveFile :: AbstractFilePath   -- ^ file to move
         -> AbstractFilePath   -- ^ destination
         -> CopyMode
         -> IO ()
moveFile (OsString from) (OsString to) cm = Dir.moveFile from to cm





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
readFile :: AbstractFilePath -> IO L.ByteString
readFile (OsString path) = Dir.readFile path


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
readFileStrict :: AbstractFilePath -> IO BS.ByteString
readFileStrict (OsString path) = Dir.readFileStrict path


-- | Open the given file as a filestream. Once the filestream
-- exits, the filehandle is cleaned up.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
readFileStream :: AbstractFilePath -> IO (SerialT IO (Array Word8))
readFileStream (OsString fp) = Dir.readFileStream fp




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
writeFile :: AbstractFilePath
          -> Bool             -- ^ True if file must exist
          -> BS.ByteString
          -> IO ()
writeFile (OsString fp) nocreat bs = Dir.writeFile fp (if nocreat then Nothing else Just Dir.newFilePerms) bs


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
writeFileL :: AbstractFilePath
           -> Bool             -- ^ True if file must exist
           -> L.ByteString
           -> IO ()
writeFileL (OsString fp) nocreat lbs = Dir.writeFileL fp (if nocreat then Nothing else Just Dir.newFilePerms) lbs


-- |Append a given ByteString to a file.
-- The file must exist. Follows symlinks.
--
-- Throws:
--
--     - `InappropriateType` if file is not a regular file or a symlink
--     - `PermissionDenied` if we cannot read the file or the directory
--        containting it
--     - `NoSuchThing` if the file does not exist
appendFile :: AbstractFilePath -> BS.ByteString -> IO ()
appendFile (OsString fp) bs = Dir.appendFile fp bs



    -------------------
    --[ File checks ]--
    -------------------


-- |Checks if the given file exists.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesExist :: AbstractFilePath -> IO Bool
doesExist (OsString bs) = Dir.doesExist bs


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesFileExist :: AbstractFilePath -> IO Bool
doesFileExist (OsString bs) = Dir.doesFileExist bs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
--
-- Only eNOENT is catched (and returns False).
doesDirectoryExist :: AbstractFilePath -> IO Bool
doesDirectoryExist (OsString bs) = Dir.doesDirectoryExist bs


-- |Checks whether a file or folder is readable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isReadable :: AbstractFilePath -> IO Bool
isReadable (OsString bs) = Dir.isReadable bs

-- |Checks whether a file or folder is writable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isWritable :: AbstractFilePath -> IO Bool
isWritable (OsString bs) = Dir.isWritable bs


-- |Checks whether a file or folder is executable.
--
-- Only eACCES, eROFS, eTXTBSY, ePERM are catched (and return False).
--
-- Throws:
--
--     - `NoSuchThing` if the file does not exist
isExecutable :: AbstractFilePath -> IO Bool
isExecutable (OsString bs) = Dir.isExecutable bs



-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: AbstractFilePath -> IO Bool
canOpenDirectory (OsString bs) = Dir.canOpenDirectory bs




    ------------------
    --[ File times ]--
    ------------------


getModificationTime :: AbstractFilePath -> IO UTCTime
getModificationTime (OsString bs) = Dir.getModificationTime bs

setModificationTime :: AbstractFilePath -> UTCTime -> IO ()
setModificationTime (OsString bs) t = Dir.setModificationTime bs t

setModificationTimeHiRes :: AbstractFilePath -> POSIXTime -> IO ()
setModificationTimeHiRes (OsString bs) t = Dir.setModificationTimeHiRes bs t



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
getDirsFiles :: AbstractFilePath        -- ^ dir to read
             -> IO [AbstractFilePath]
getDirsFiles (OsString p) = fmap OsString <$> Dir.getDirsFiles p


-- | Like 'getDirsFiles', but returns the filename only, instead
-- of prepending the base path.
getDirsFiles' :: AbstractFilePath        -- ^ dir to read
              -> IO [AbstractFilePath]
getDirsFiles' (OsString fp) = fmap OsString <$> Dir.getDirsFiles' fp


-- | Like 'getDirsFiles'', except returning a Stream.
getDirsFilesStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                   => AbstractFilePath
                   -> IO (SerialT m AbstractFilePath)
getDirsFilesStream (OsString fp) = fmap OsString <$> Dir.getDirsFilesStream fp


    -----------
    --[ CWD ]--
    -----------

getCurrentDirectory :: IO AbstractFilePath
getCurrentDirectory = OsString <$> Dir.getCurrentDirectory

setCurrentDirectory :: AbstractFilePath -> IO ()
setCurrentDirectory (OsString fp) = Dir.setCurrentDirectory fp



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
getFileType :: AbstractFilePath -> IO FileType
getFileType (OsString fp) = Dir.getFileType fp



    --------------
    --[ Others ]--
    --------------



-- |Applies `realpath` on the given path.
--
-- Throws:
--
--    - `NoSuchThing` if the file at the given path does not exist
--    - `NoSuchThing` if the symlink is broken
canonicalizePath :: AbstractFilePath -> IO AbstractFilePath
canonicalizePath (OsString fp) = OsString <$> Dir.canonicalizePath fp


-- |Converts any path to an absolute path.
-- This is done in the following way:
--
--    - if the path is already an absolute one, just return it
--    - if it's a relative path, prepend the current directory to it
toAbs :: AbstractFilePath -> IO AbstractFilePath
toAbs (OsString bs) = OsString <$> Dir.toAbs bs

