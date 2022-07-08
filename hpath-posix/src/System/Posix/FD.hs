-- |
-- Module      :  System.Posix.FD
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides an alternative for `System.Posix.IO.ByteString.openFd`
-- which gives us more control on what status flags to pass to the
-- low-level @open(2)@ call, in contrast to the unix package.


{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}


module System.Posix.FD (
    openFd
) where


import Foreign.C.String
import Foreign.C.Types
import System.Posix.Foreign
import qualified System.Posix as Posix
import System.Posix.PosixPath.FilePath
import System.OsPath.Types


foreign import ccall unsafe "open"
   c_open :: CString -> CInt -> Posix.CMode -> IO CInt


open_  :: CString
       -> Posix.OpenMode
       -> [Flags]
       -> Maybe Posix.FileMode
       -> IO Posix.Fd
open_ str how optional_flags maybe_mode = do
    fd <- c_open str all_flags mode_w
    return (Posix.Fd fd)
  where
    all_flags  = unionFlags $ optional_flags ++ [open_mode] ++ creat


    (creat, mode_w) = case maybe_mode of
                        Nothing -> ([],0)
                        Just x  -> ([oCreat], x)

    open_mode = case how of
                   Posix.ReadOnly  -> oRdonly
                   Posix.WriteOnly -> oWronly
                   Posix.ReadWrite -> oRdwr


-- |Open and optionally create this file. See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
--
-- Note that passing @Just x@ as the 4th argument triggers the
-- `oCreat` status flag, which must be set when you pass in `oExcl`
-- to the status flags. Also see the manpage for @open(2)@.
openFd :: PosixPath
       -> Posix.OpenMode
       -> [Flags]               -- ^ status flags of @open(2)@
       -> Maybe Posix.FileMode  -- ^ @Just x@ => creates the file with the given modes, Nothing => the file must exist.
       -> IO Posix.Fd
openFd name how optional_flags maybe_mode =
   withFilePath name $ \str ->
     throwErrnoPathIfMinus1Retry "openFd" name $
       open_ str how optional_flags maybe_mode

