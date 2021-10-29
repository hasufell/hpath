-- |
-- Module      :  System.Posix.PosixFilePath.Directory.Traversals
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Traversal and read operations on directories.


{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}


module System.Posix.PosixFilePath.Directory.Traversals (

  getDirectoryContents
, getDirectoryContents'

, allDirectoryContents
, allDirectoryContents'
, traverseDirectory

-- lower-level stuff
, readDirEnt
, packDirStream
, unpackDirStream
, fdOpendir

, realpath
) where


#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Monad
import AFP.AbstractFilePath.Posix ((</>), fromPlatformString)
import System.Posix.Foreign

import qualified System.Posix as Posix
import System.IO.Error
import Control.Exception
import System.Posix.PosixFilePath.FilePath
import System.Posix.Directory.PosixFilePath as PosixBS
import System.Posix.Files.PosixString

import System.IO.Unsafe
import "unix" System.Posix.IO.ByteString (closeFd)
import Unsafe.Coerce (unsafeCoerce)
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Ptr
import Foreign.Storable

import AFP.AbstractFilePath.Types
import qualified AFP.OsString.Internal.Types as T

import qualified Data.ByteString.Short as SBS





----------------------------------------------------------

-- | Get all files from a directory and its subdirectories.
--
-- Upon entering a directory, 'allDirectoryContents' will get all entries
-- strictly.  However the returned list is lazy in that directories will only
-- be accessed on demand.
--
-- Follows symbolic links for the input dir.
allDirectoryContents :: PosixFilePath -> IO [PosixFilePath]
allDirectoryContents topdir = do
    namesAndTypes <- getDirectoryContents topdir
    let properNames = filter ((`notElem` [".", ".."]) . snd) namesAndTypes
    paths <- forM properNames $ \(typ,name) -> unsafeInterleaveIO $ do
        let path = topdir </> name
        case () of
            () | typ == dtDir -> allDirectoryContents path
               | typ == dtUnknown -> do
                    isDir <- isDirectory <$> getFileStatus path
                    if isDir
                        then allDirectoryContents path
                        else return [path]
               | otherwise -> return [path]
    return (topdir : concat paths)

-- | Get all files from a directory and its subdirectories strictly.
--
-- Follows symbolic links for the input dir.
allDirectoryContents' :: PosixFilePath -> IO [PosixFilePath]
allDirectoryContents' = fmap reverse . traverseDirectory (\acc fp -> return (fp:acc)) []
-- this uses traverseDirectory because it's more efficient than forcing the
-- lazy version.

-- | Recursively apply the 'action' to the parent directory and all
-- files/subdirectories.
--
-- This function allows for memory-efficient traversals.
--
-- Follows symbolic links for the input dir.
traverseDirectory :: (s -> PosixFilePath -> IO s) -> s -> PosixFilePath -> IO s
traverseDirectory act s0 topdir = toploop
  where
    toploop = do
        isDir <- isDirectory <$> getFileStatus topdir
        s' <- act s0 topdir
        if isDir then actOnDirContents topdir s' loop
                 else return s'
    loop typ path acc = do
        isDir <- case () of
            () | typ == dtDir     -> return True
               | typ == dtUnknown -> isDirectory <$> getFileStatus path
               | otherwise        -> return False
        if isDir
          then act acc path >>= \acc' -> actOnDirContents path acc' loop
          else act acc path

actOnDirContents :: PosixFilePath
                 -> b
                 -> (DirType -> PosixFilePath -> b -> IO b)
                 -> IO b
actOnDirContents pathRelToTop b f = do
  locstr <- fromPlatformString pathRelToTop
  modifyIOError ((`ioeSetFileName` locstr) .
                 (`ioeSetLocation` "findBSTypRel")) $
    bracket
      (openDirStream pathRelToTop)
      Posix.closeDirStream
      (\dirp -> loop dirp b)
 where
  loop dirp b' = do
    (typ,e) <- readDirEnt dirp
    if (e == "")
      then return b'
      else
          if (e == "." || e == "..")
              then loop dirp b'
              else f typ (pathRelToTop </> e) b' >>= loop dirp


----------------------------------------------------------
-- dodgy stuff

type CDir = ()
type CDirent = ()

-- Posix doesn't export DirStream, so to re-use that type we need to use
-- unsafeCoerce.  It's just a newtype, so this is a legitimate usage.
-- ugly trick.
unpackDirStream :: DirStream -> Ptr CDir
unpackDirStream = unsafeCoerce

packDirStream :: Ptr CDir -> DirStream
packDirStream = unsafeCoerce

-- the __hscore_* functions are defined in the unix package.  We can import them and let
-- the linker figure it out.
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  c_name :: Ptr CDirent -> IO CString

foreign import ccall unsafe "__posixdir_d_type"
  c_type :: Ptr CDirent -> IO DirType

foreign import capi "stdlib.h realpath"
  c_realpath :: CString -> CString -> IO CString

-- Using normal 'ccall' here lead to memory bugs, crashes
-- and corrupted d_name entries. It appears there are two fdopendirs:
--   https://opensource.apple.com/source/Libc/Libc-1244.1.7/include/dirent.h.auto.html
-- The capi call picks the correct one.
foreign import capi unsafe "dirent.h fdopendir"
  c_fdopendir :: Posix.Fd -> IO (Ptr CDir)

----------------------------------------------------------
-- less dodgy but still lower-level


readDirEnt :: DirStream -> IO (DirType, PosixFilePath)
readDirEnt (unpackDirStream -> dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
       then do
         dEnt <- peek ptr_dEnt
         if (dEnt == nullPtr)
            then return (dtUnknown, mempty)
            else do
                 dName <- c_name dEnt >>= peekFilePath
                 dType <- c_type dEnt
                 c_freeDirEnt dEnt
                 return (dType, dName)
       else do
         errno <- getErrno
         if (errno == eINTR)
            then loop ptr_dEnt
            else do
                 let (Errno eo) = errno
                 if (eo == 0)
                    then return (dtUnknown, mempty)
                    else throwErrno "readDirEnt"


-- |Gets all directory contents (not recursively).
getDirectoryContents :: PosixFilePath -> IO [(DirType, PosixFilePath)]
getDirectoryContents path = do
  locstr <- fromPlatformString path
  modifyIOError ((`ioeSetFileName` locstr) .
                 (`ioeSetLocation` "System.Posix.RawFilePath.Directory.Traversals.getDirectoryContents")) $
    bracket
      (PosixBS.openDirStream path)
      PosixBS.closeDirStream
      _dirloop


-- |Binding to @fdopendir(3)@.
fdOpendir :: Posix.Fd -> IO DirStream
fdOpendir fd =
    packDirStream <$> throwErrnoIfNull "fdOpendir" (c_fdopendir fd)


-- |Like `getDirectoryContents` except for a file descriptor.
--
-- To avoid complicated error checks, the file descriptor is
-- __always__ closed, even if `fdOpendir` fails. Usually, this
-- only happens on successful `fdOpendir` and after the directory
-- stream is closed. Also see the manpage of @fdopendir(3)@ for
-- more details.
getDirectoryContents' :: Posix.Fd -> IO [(DirType, PosixFilePath)]
getDirectoryContents' fd = do
  dirstream <- fdOpendir fd `catchIOError` \e -> do
    closeFd fd
    ioError e
  -- closeDirStream closes the filedescriptor
  finally (_dirloop dirstream) (PosixBS.closeDirStream dirstream)


_dirloop :: DirStream -> IO [(DirType, PosixFilePath)]
{-# INLINE _dirloop #-}
_dirloop dirp = do
   t@(_typ, e) <- readDirEnt dirp
   if e == mempty then return [] else do
     es <- _dirloop dirp
     return (t:es)


-- | return the canonicalized absolute pathname
--
-- like canonicalizePath, but uses @realpath(3)@
realpath :: PosixFilePath -> IO PosixFilePath
realpath (T.PS inp) = fmap T.PS $
    allocaBytes pathMax $ \tmp -> do
        void $ SBS.useAsCString inp $ \cstr -> throwErrnoIfNull "realpath" $ c_realpath cstr tmp
        SBS.packCString tmp
