{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Data.IORef
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils
#ifdef WINDOWS
#else
import System.Posix.Temp.PosixString (mkdtemp)
import System.Posix.Env.PosixString (getEnvDefault)
#endif
import "hpath-directory" System.Directory.AFP
import AFP.AbstractFilePath
import AFP.OsString.Internal.Types


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = do
#ifdef WINDOWS
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
