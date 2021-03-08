{-# LANGUAGE DeriveDataTypeable #-}

-- | Internal types and functions.

module HPath.Internal
  (Path(..))
  where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.Data

-- | The main Path type.
--
-- The type variable 'b' is either:
--
--   * 'HPath.Abs' -- absolute path (starting with a @"/"@)
--   * 'HPath.Rel' -- relative path (not starting with a @"/"@)
--
-- Internally it is a ByteString. The path is guaranteed to
-- be normalised and contain no trailing Path separators,
-- except for the @"/"@ root path.
--
-- There are no duplicate path separators
-- @"\/\/"@, no @".."@, no @".\/"@, etc.
-- 
-- Two special paths exist:
--
--  * @"/"@ -- the 'HPath.rootPath' (absolute)
--  * @"."@ -- the 'HPath.pwdPath' (relative)
--
-- The constructor is not exposed. Instead, use the smart constructors
-- 'HPath.parseAbs', 'HPath.parseRel' and 'HPath.parseAny'.
data Path b = MkPath ByteString
  deriving (Typeable)

-- | ByteString equality.
--
-- The following property holds:
--
-- @show x == show y ≡ x == y@
instance Eq (Path b) where
  (==) (MkPath x) (MkPath y) = x == y

-- | ByteString ordering.
--
-- The following property holds:
--
-- @show x \`compare\` show y ≡ x \`compare\` y@
instance Ord (Path b) where
  compare (MkPath x) (MkPath y) = compare x y

-- | Same as 'HPath.toFilePath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b) where
  show (MkPath x) = show x

instance NFData (Path b) where
  rnf (MkPath x) = rnf x

