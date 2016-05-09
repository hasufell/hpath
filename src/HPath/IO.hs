-- |
-- Module      :  HPath.IO
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD 3 clause
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- IO operations on HPath.

{-# OPTIONS_HADDOCK ignore-exports #-}

module HPath.IO
  where


import           HPath
import           HPath.Internal
import           System.Posix.Directory.Traversals (realpath)



-- | May fail on `realpath`.
canonicalizePath :: Path Abs -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- realpath l
  return $ MkPath nl
