{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal types and functions.

module HPath.Internal
  (Path(..))
  where

import System.AbstractFilePath
import Control.DeepSeq (NFData (..))
import Data.Data
import GHC.Generics (Generic)
import           Language.Haskell.TH.Syntax (Lift(..), lift)
import qualified Language.Haskell.TH.Syntax as TH

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
data Path b = MkPath AbstractFilePath
  deriving (Typeable, Generic, NFData)

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

instance Typeable a => Lift (Path a) where
  lift (MkPath bs) = [| MkPath bs :: Path $(pure a) |]
    where
      a = TH.ConT $ TH.Name occ flav
        where
        tc   = typeRepTyCon (typeRep (Proxy :: Proxy a))
        occ  = TH.OccName (tyConName tc)
        flav = TH.NameG TH.TcClsName (TH.PkgName (tyConPackage tc)) (TH.ModName (tyConModule tc))

