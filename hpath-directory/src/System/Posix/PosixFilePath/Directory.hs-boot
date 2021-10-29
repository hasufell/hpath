module System.Posix.PosixFilePath.Directory where

import AFP.AbstractFilePath.Posix (PosixFilePath)

canonicalizePath :: PosixFilePath -> IO PosixFilePath

toAbs :: PosixFilePath -> IO PosixFilePath

doesFileExist :: PosixFilePath -> IO Bool

doesDirectoryExist :: PosixFilePath -> IO Bool

isWritable :: PosixFilePath -> IO Bool

canOpenDirectory :: PosixFilePath -> IO Bool
