module System.Posix.PosixPath.Directory where

import System.OsPath.Posix (PosixPath)

canonicalizePath :: PosixPath -> IO PosixPath

toAbs :: PosixPath -> IO PosixPath

doesFileExist :: PosixPath -> IO Bool

doesDirectoryExist :: PosixPath -> IO Bool

isWritable :: PosixPath -> IO Bool

canOpenDirectory :: PosixPath -> IO Bool
