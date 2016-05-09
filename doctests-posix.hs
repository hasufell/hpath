{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.Posix.Directory.Traversals

import Test.DocTest
import Test.HUnit

main = do
    doctest
      [ "-isrc"
      , "-XOverloadedStrings"
      , "System.Posix.FilePath"
      ]
    runTestTT unitTests


unitTests :: Test
unitTests = test
    [ TestCase $ do
        r <- (==) <$> allDirectoryContents "." <*> allDirectoryContents' "."
        assertBool "allDirectoryContents == allDirectoryContents'" r
    ]
