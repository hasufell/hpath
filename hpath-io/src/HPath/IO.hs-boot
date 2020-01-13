module HPath.IO where


import HPath

canonicalizePath :: Path b -> IO (Path Abs)

toAbs :: Path b -> IO (Path Abs)

doesFileExist :: Path b -> IO Bool

doesDirectoryExist :: Path b -> IO Bool

isWritable :: Path b -> IO Bool

canOpenDirectory :: Path b -> IO Bool
