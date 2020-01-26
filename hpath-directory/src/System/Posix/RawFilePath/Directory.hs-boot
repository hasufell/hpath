module System.Posix.RawFilePath.Directory where

import System.Posix.ByteString.FilePath (RawFilePath)

canonicalizePath :: RawFilePath -> IO RawFilePath

toAbs :: RawFilePath -> IO RawFilePath

doesFileExist :: RawFilePath -> IO Bool

doesDirectoryExist :: RawFilePath -> IO Bool

isWritable :: RawFilePath -> IO Bool

canOpenDirectory :: RawFilePath -> IO Bool
