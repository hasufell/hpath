{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable
import           Data.List                      ( sortBy )
import           Streamly.External.Posix.DirStream
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.Posix.Directory as Posix
import           System.Posix.Foreign
import           Test.Hspec



checkDirContents :: FilePath -> IO ()
checkDirContents fp = do
  let f1 = fp </> "f1"
  let f2 = fp </> "f2"
  let f3 = fp </> "f3"
  let f4 = fp </> "f4"
  for_ [f1, f2, f3, f4] $ \f -> openFile f ReadWriteMode
  ds       <- Posix.openDirStream fp
  contents <- fmap (sortBy (\(_, y) (_, z) -> compare y z)) $ dirContents ds
  contents
    `shouldBe` [ (DirType 8, "f1")
               , (DirType 8, "f2")
               , (DirType 8, "f3")
               , (DirType 8, "f4")
               ]




main :: IO ()
main = hspec $ do
  describe "Streamly.External.FileSystem.DirStream.Posix" $ do
    it "dirContents" $ withSystemTempDirectory "y" checkDirContents
