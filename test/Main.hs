{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main =
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ before_ up
    $ after_ down
    $ Spec.spec
  where
    up = createTmpDir
    down = deleteTmpDir

