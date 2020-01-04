{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.IORef
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils
import System.Posix.Temp.ByteString (mkdtemp)


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = do
  tmpBase <- mkdtemp "/tmp/"
  writeIORef baseTmpDir (Just (tmpBase `BS.append` "/"))
  putStrLn $ ("Temporary test directory at: " <> show tmpBase)
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ afterAll_ deleteBaseTmpDir
    $ Spec.spec
