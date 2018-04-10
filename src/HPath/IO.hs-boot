module HPath.IO where


import HPath

canonicalizePath :: Path b -> IO (Path Abs)

toAbs :: Path b -> IO (Path Abs)
