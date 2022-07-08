{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Data.IORef
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils
#ifdef WINDOWS
import System.Win32.WindowsString.Info
#else
import System.Posix.Temp.PosixString (mkdtemp)
import System.Posix.Env.PosixString (getEnvDefault)
#endif
import System.Directory.OsPath
import System.OsPath
import System.OsString.Internal.Types


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = do
#ifdef WINDOWS
  tmpBase <- fmap ((</> "hpath-directory") . OsString) getTemporaryDirectory
  createDirRecursive tmpBase
#else
  (OsString tmpdir) <- fmap (</> "hpath-directory") (getEnvDefault "TMPDIR" "/tmp" >>= canonicalizePath . OsString)
  tmpBase <- OsString <$> mkdtemp tmpdir
#endif
  writeIORef baseTmpDir (Just (tmpBase <> "/"))
  putStrLn $ ("Temporary test directory at: " ++ show tmpBase)
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ afterAll_ deleteBaseTmpDir
    $ Spec.spec
