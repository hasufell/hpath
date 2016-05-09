module Main where


import Test.DocTest
import Test.HUnit

main =
    doctest
      ["-isrc"
      , "-XOverloadedStrings"
      , "src/HPath.hs"
      ]

