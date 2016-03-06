{-# LANGUAGE DeriveDataTypeable #-}

-- TODO: - remove undefined in Ord instance
--       - use viewpatterns/patternsynonyms so we don't need to
--         export the constructors

-- | Internal types and functions.

module Path.Internal
  (Path(..))
  where

import Control.DeepSeq (NFData (..))
import Data.Data

-- | Path of some base and type.
--
-- Internally is a string. The string can be of two formats only:
--
-- 1. File format: @file.txt@, @foo\/bar.txt@, @\/foo\/bar.txt@
-- 2. Directory format: @foo\/@, @\/foo\/bar\/@
--
-- All directories end in a trailing separator. There are no duplicate
-- path separators @\/\/@, no @..@, no @.\/@, no @~\/@, etc.
data Path b = FPath FilePath
            | DPath FilePath
  deriving (Typeable)

-- | String equality.
--
-- The following property holds:
--
-- @show x == show y ≡ x == y@
instance Eq (Path b) where
  (==) (FPath x) (FPath y) = x == y
  (==) (DPath x) (DPath y) = x == y
  (==) _         _         = False

-- | String ordering.
--
-- The following property holds:
--
-- @show x \`compare\` show y ≡ x \`compare\` y@
instance Ord (Path b) where
  compare (FPath x) (FPath y) = compare x y
  compare (DPath x) (DPath y) = compare x y
  compare _         _         = undefined

-- | Same as 'Path.toFilePath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b) where
  show (FPath x) = show x
  show (DPath x) = show x

instance NFData (Path b) where
  rnf (FPath x) = rnf x
  rnf (DPath x) = rnf x

