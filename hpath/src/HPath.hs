-- |
-- Module      :  HPath
-- Copyright   :  © 2015–2016 FP Complete, 2016 Julian Ospald
-- License     :  BSD 3 clause
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for well-typed paths.


{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPath
  (
  -- * Types
   Abs
  ,Path
  ,Rel
  ,PathParseException
  ,PathException
#if __GLASGOW_HASKELL__ >= 708
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
#endif
   -- * Path Construction
  ,parseAbs
  ,parseRel
  ,parseAny
  ,rootPath
  -- * Path Conversion
  ,fromAbs
  ,fromRel
  ,toFilePath
  ,fromAny
  -- * Path Operations
  ,(</>)
  ,basename
  ,dirname
  ,getAllParents
  ,getAllComponents
  ,getAllComponentsAfterRoot
  ,stripDir
  -- * Path Examination
  ,isParentOf
  ,isRootPath
  -- * Path IO helpers
  ,withAbsPath
  ,withRelPath
  -- * Quasiquoters
  ,abs
  ,rel
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
#if MIN_VERSION_bytestring(0,10,8)
import           Data.ByteString(ByteString, stripPrefix)
#else
import           Data.ByteString(ByteString)
import qualified Data.List as L
#endif
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8
import           Data.Data
import           Data.Maybe
import           Data.Word8
import           HPath.Internal
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Exp(..), Lift(..), lift)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Prelude hiding (abs, any)
import           System.Posix.FilePath hiding ((</>))


--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root.
data Rel deriving (Typeable)

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbs ByteString
  | InvalidRel ByteString
  | Couldn'tStripPrefixTPS ByteString ByteString
  deriving (Show,Typeable)
instance Exception PathParseException

data PathException = RootDirHasNoBasename
  deriving (Show,Typeable)
instance Exception PathException


--------------------------------------------------------------------------------
-- PatternSynonyms

#if __GLASGOW_HASKELL__ >= 710
pattern Path :: ByteString -> Path a
#endif
#if __GLASGOW_HASKELL__ >= 708
pattern Path x <- (MkPath x)
#endif

--------------------------------------------------------------------------------
-- Path Parsers



-- | Get a location for an absolute path. Produces a normalised path.
--
-- Throws: 'PathParseException'
--
-- >>> parseAbs "/abc"          :: Maybe (Path Abs)
-- Just "/abc"
-- >>> parseAbs "/"             :: Maybe (Path Abs)
-- Just "/"
-- >>> parseAbs "/abc/def"      :: Maybe (Path Abs)
-- Just "/abc/def"
-- >>> parseAbs "/abc/def/.///" :: Maybe (Path Abs)
-- Just "/abc/def"
-- >>> parseAbs "abc"           :: Maybe (Path Abs)
-- Nothing
-- >>> parseAbs ""              :: Maybe (Path Abs)
-- Nothing
-- >>> parseAbs "/abc/../foo"   :: Maybe (Path Abs)
-- Nothing
parseAbs :: MonadThrow m
         => ByteString -> m (Path Abs)
parseAbs filepath =
  if isAbsolute filepath &&
     isValid filepath &&
     not (hasParentDir filepath)
     then return (MkPath . dropTrailingPathSeparator . normalise $ filepath)
     else throwM (InvalidAbs filepath)


-- | Get a location for a relative path. Produces a normalised
-- path.
--
-- Note that @filepath@ may contain any number of @./@ but may not consist
-- solely of @./@.  It also may not contain a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
-- >>> parseRel "abc"        :: Maybe (Path Rel)
-- Just "abc"
-- >>> parseRel "def/"       :: Maybe (Path Rel)
-- Just "def"
-- >>> parseRel "abc/def"    :: Maybe (Path Rel)
-- Just "abc/def"
-- >>> parseRel "abc/def/."  :: Maybe (Path Rel)
-- Just "abc/def"
-- >>> parseRel "/abc"       :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel ""           :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel "abc/../foo" :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel "."          :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel ".."         :: Maybe (Path Rel)
-- Nothing
parseRel :: MonadThrow m
         => ByteString -> m (Path Rel)
parseRel filepath =
  if not (isAbsolute filepath) &&
     filepath /= BS.singleton _period &&
     filepath /= BS.pack [_period, _period] &&
     not (hasParentDir filepath) &&
     isValid filepath
     then return (MkPath . dropTrailingPathSeparator . normalise $ filepath)
     else throwM (InvalidRel filepath)



-- | Parses a path, whether it's relative or absolute.
--
-- Excludes '.' and '..'.
--
-- Throws: 'PathParseException'
--
-- >>> parseAny "/abc"       :: Maybe (Either (Path Abs) (Path Rel))
-- Just (Left "/abc")
-- >>> parseAny "..."        :: Maybe (Either (Path Abs) (Path Rel))
-- Just (Right "...")
-- >>> parseAny "abc/def"    :: Maybe (Either (Path Abs) (Path Rel))
-- Just (Right "abc/def")
-- >>> parseAny "abc/def/."  :: Maybe (Either (Path Abs) (Path Rel))
-- Just (Right "abc/def")
-- >>> parseAny "/abc"       :: Maybe (Either (Path Abs) (Path Rel))
-- Just (Left "/abc")
-- >>> parseAny ""           :: Maybe (Either (Path Abs) (Path Rel))
-- Nothing
-- >>> parseAny "abc/../foo" :: Maybe (Either (Path Abs) (Path Rel))
-- Nothing
-- >>> parseAny "."          :: Maybe (Either (Path Abs) (Path Rel))
-- Nothing
-- >>> parseAny ".."         :: Maybe (Either (Path Abs) (Path Rel))
-- Nothing
parseAny :: MonadThrow m => ByteString -> m (Either (Path Abs) (Path Rel))
parseAny filepath = case parseAbs filepath of
  Just p -> pure $ Left p
  Nothing         -> case parseRel filepath of
    Just p -> pure $ Right p
    Nothing       -> throwM (InvalidRel filepath)


rootPath :: Path Abs
rootPath = (MkPath (BS.singleton _slash))


--------------------------------------------------------------------------------
-- Path Conversion

-- | Convert any Path to a ByteString type.
toFilePath :: Path b -> ByteString
toFilePath (MkPath l) = l

-- | Convert an absolute Path to a ByteString type.
fromAbs :: Path Abs -> ByteString
fromAbs = toFilePath

-- | Convert a relative Path to a ByteString type.
fromRel :: Path Rel -> ByteString
fromRel = toFilePath

fromAny :: Either (Path Abs) (Path Rel) -> ByteString
fromAny = either toFilePath toFilePath


--------------------------------------------------------------------------------
-- Path Operations

-- | Append two paths.
--
-- The second argument must always be a relative path, which ensures
-- that undefinable things like `"/abc" </> "/def"` cannot happen.
--
-- Technically, the first argument can be a path that points to a non-directory,
-- because this library is IO-agnostic and makes no assumptions about
-- file types.
--
-- >>> (MkPath "/")        </> (MkPath "file"     :: Path Rel)
-- "/file"
-- >>> (MkPath "/path/to") </> (MkPath "file"     :: Path Rel)
-- "/path/to/file"
-- >>> (MkPath "/")        </> (MkPath "file/lal" :: Path Rel)
-- "/file/lal"
-- >>> (MkPath "/")        </> (MkPath "file"    :: Path Rel)
-- "/file"
(</>) :: Path b -> Path Rel -> Path b
(</>) (MkPath a) (MkPath b) = MkPath (a' `BS.append` b)
  where
    a' = if hasTrailingPathSeparator a
            then a
            else addTrailingPathSeparator a


-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The bases must match.
--
-- >>> (MkPath "/lal/lad")     `stripDir` (MkPath "/lal/lad/fad") :: Maybe (Path Rel)
-- Just "fad"
-- >>> (MkPath "lal/lad")      `stripDir` (MkPath "lal/lad/fad")  :: Maybe (Path Rel)
-- Just "fad"
-- >>> (MkPath "/")            `stripDir` (MkPath "/")            :: Maybe (Path Rel)
-- Nothing
-- >>> (MkPath "/lal/lad/fad") `stripDir` (MkPath "/lal/lad")     :: Maybe (Path Rel)
-- Nothing
-- >>> (MkPath "fad")          `stripDir` (MkPath "fad")          :: Maybe (Path Rel)
-- Nothing
stripDir :: MonadThrow m
         => Path b -> Path b -> m (Path Rel)
stripDir (MkPath p) (MkPath l) =
  case stripPrefix p' l of
    Nothing -> throwM (Couldn'tStripPrefixTPS p' l)
    Just ok -> if BS.null ok
                 then throwM (Couldn'tStripPrefixTPS p' l)
                 else return (MkPath ok)
  where
    p' = addTrailingPathSeparator p


-- |Get all parents of a path.
--
-- >>> getAllParents (MkPath "/abs/def/dod")
-- ["/abs/def","/abs","/"]
-- >>> getAllParents (MkPath "/foo")
-- ["/"]
-- >>> getAllParents (MkPath "/")
-- []
getAllParents :: Path Abs -> [Path Abs]
getAllParents (MkPath p)
  | np == BS.singleton pathSeparator = []
  | otherwise = dirname (MkPath np) : getAllParents (dirname $ MkPath np)
  where
    np = normalise p


-- | Gets all path components.
--
-- >>> getAllComponents (MkPath "abs/def/dod")
-- ["abs","def","dod"]
-- >>> getAllComponents (MkPath "abs")
-- ["abs"]
getAllComponents :: Path Rel -> [Path Rel]
getAllComponents (MkPath p) = fmap MkPath . splitDirectories $ p


-- | Gets all path components after the "/" root directory.
--
-- >>> getAllComponentsAfterRoot (MkPath "/abs/def/dod")
-- ["abs","def","dod"]
-- >>> getAllComponentsAfterRoot (MkPath "/abs")
-- ["abs"]
getAllComponentsAfterRoot :: Path Abs -> [Path Rel]
getAllComponentsAfterRoot p = getAllComponents (fromJust $ stripDir rootPath p)


-- | Extract the directory name of a path.
--
-- >>> dirname (MkPath "/abc/def/dod")
-- "/abc/def"
-- >>> dirname (MkPath "/")
-- "/"
dirname :: Path Abs -> Path Abs
dirname (MkPath fp) = MkPath (takeDirectory fp)

-- | Extract the file part of a path.
--
--
-- The following properties hold:
--
-- @basename (p \<\/> a) == basename a@
--
-- Throws: `PathException` if given the root path "/"
--
-- >>> basename (MkPath "/abc/def/dod") :: Maybe (Path Rel)
-- Just "dod"
-- >>> basename (MkPath "abc/def/dod")  :: Maybe (Path Rel)
-- Just "dod"
-- >>> basename (MkPath "dod")          :: Maybe (Path Rel)
-- Just "dod"
-- >>> basename (MkPath "/")            :: Maybe (Path Rel)
-- Nothing
basename :: MonadThrow m => Path b -> m (Path Rel)
basename (MkPath l)
  | not (isAbsolute rl) = return $ MkPath rl
  | otherwise           = throwM RootDirHasNoBasename
  where
    rl = last . splitPath $ l



--------------------------------------------------------------------------------
-- Path Examination

-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
--
-- >>> (MkPath "/lal/lad")     `isParentOf` (MkPath "/lal/lad/fad")
-- True
-- >>> (MkPath "lal/lad")      `isParentOf` (MkPath "lal/lad/fad")
-- True
-- >>> (MkPath "/")            `isParentOf` (MkPath "/")
-- False
-- >>> (MkPath "/lal/lad/fad") `isParentOf` (MkPath "/lal/lad")
-- False
-- >>> (MkPath "fad")          `isParentOf` (MkPath "fad")
-- False
isParentOf :: Path b -> Path b -> Bool
isParentOf p l = isJust (stripDir p l :: Maybe (Path Rel))


-- | Check whether the given Path is the root "/" path.
--
-- >>> isRootPath (MkPath "/lal/lad")
-- False
-- >>> isRootPath (MkPath "/")
-- True
isRootPath :: Path Abs -> Bool
isRootPath = (== rootPath)


--------------------------------------------------------------------------------
-- Path IO helpers


withAbsPath :: Path Abs -> (ByteString -> IO a) -> IO a
withAbsPath (MkPath p) action = action p


withRelPath :: Path Rel -> (ByteString -> IO a) -> IO a
withRelPath (MkPath p) action = action p



------------------------
-- ByteString helpers

#if MIN_VERSION_bytestring(0,10,8)
#else
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix a b = BS.pack `fmap` L.stripPrefix (BS.unpack a) (BS.unpack b)
#endif


------------------------
-- QuasiQuoters

instance Lift (Path a) where
  lift (MkPath bs) = AppE <$> [| MkPath . BS.pack |] <*> lift (BS.unpack bs)


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
  { quoteExp  = (\s -> quoteExp' . fromString $ s)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }

mkAbs :: ByteString -> Q Exp
mkAbs = either (error . show) lift . parseAbs

mkRel :: ByteString -> Q Exp
mkRel = either (error . show) lift . parseRel

-- | Quasiquote an absolute Path. This accepts Unicode Chars and will encode as UTF-8.
--
-- >>> [abs|/etc/profile|] :: Path Abs
-- "/etc/profile"
-- >>> [abs|/|] :: Path Abs
-- "/"
-- >>> [abs|/|] :: Path Abs
-- "/\239\131\144"
abs :: QuasiQuoter
abs = qq mkAbs

-- | Quasiquote a relative Path. This accepts Unicode Chars and will encode as UTF-8.
--
-- >>> [rel|etc|] :: Path Rel
-- "etc"
-- >>> [rel|bar/baz|] :: Path Rel
-- "bar/baz"
-- >>> [rel||] :: Path Rel
-- "\239\131\144"
rel :: QuasiQuoter
rel = qq mkRel

