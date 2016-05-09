{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Test.DocTest
import Test.HUnit

main =
    doctest
      ["-isrc"
      , "-XOverloadedStrings"
      , "src/HPath.hs"
      ]

