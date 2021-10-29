{-# LANGUAGE OverloadedStrings #-}

import Data.IORef
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils
import System.Posix.Temp.PosixString (mkdtemp)
import System.Posix.Env.PosixString (getEnvDefault)
import "hpath-directory" System.Posix.PosixFilePath.Directory
import AFP.AbstractFilePath.Posix


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = do
  tmpdir <- getEnvDefault "TMPDIR" "/tmp" >>= canonicalizePath
  tmpBase <- mkdtemp (tmpdir </> "hpath-directory")
  writeIORef baseTmpDir (Just (tmpBase <> "/"))
  putStrLn $ ("Temporary test directory at: " ++ show tmpBase)
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ afterAll_ deleteBaseTmpDir
    $ Spec.spec
