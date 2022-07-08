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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module HPath
  (
  -- * Types
   Path
  ,Abs
  ,Rel
  ,PathParseException
  ,PathException
#if __GLASGOW_HASKELL__ >= 708
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
#endif
   -- * Path Construction
  ,parseAbs
  ,parseAbs'
  ,parseRel
  ,parseRel'
  ,parseAny
  ,parseAny'
  ,pwdPath
  -- * Path Conversion
  ,fromAbs
  ,fromRel
  ,toFilePath
  ,fromAny
  -- * Path Operations
  ,(</>)
  ,basename
  ,basename'
  ,dirname
  ,getAllParents
  ,getAllComponents
  ,getAllComponentsAfterRoot
  ,stripDir
  -- * Path Examination
  ,isParentOf
  ,isPwdPath
  -- * Path IO helpers
  ,withAbsPath
  ,withRelPath
  -- * Quasiquoters
  ,abs
  ,rel
  )
  where

import           System.OsPath      hiding ((</>))
import qualified System.OsPath as AFP
import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import qualified Data.List as L
import           Data.Data
import           Data.Maybe
import           HPath.Internal
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift(..), lift)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Prelude hiding (abs, any)
import System.OsString.Internal.Types
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.OsPath.Windows.Internal as Raw
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS
#else
import qualified System.OsPath.Posix.Internal as Raw
import qualified System.OsPath.Data.ByteString.Short as BS
#endif

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (abs, any)
-- >>> import HPath
-- >>> import qualified System.OsPath as AFP
-- >>> import Data.String
-- >>> instance IsString OsString where fromString = either (error . show) id . AFP.encodeUtf


--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a drive.
data Rel deriving (Typeable)

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbs OsPath
  | InvalidRel OsPath
  | Couldn'tStripPrefixTPS OsPath OsPath
  deriving (Show,Typeable)
instance Exception PathParseException

data PathException = RootDirHasNoBasename
  deriving (Show,Typeable)
instance Exception PathException


--------------------------------------------------------------------------------
-- PatternSynonyms

#if __GLASGOW_HASKELL__ >= 710
pattern Path :: OsPath -> Path a
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
-- >>> parseAbs "/abc"
-- "/abc"
-- >>> parseAbs "/"
-- "/"
-- >>> parseAbs "/abc/def"
-- "/abc/def"
-- >>> parseAbs "/abc/def/.///"
-- "/abc/def"
-- >>> parseAbs "abc"
-- *** Exception: InvalidAbs "abc"
-- >>> parseAbs ""
-- *** Exception: InvalidAbs ""
-- >>> parseAbs "/abc/../foo"
-- *** Exception: InvalidAbs "/abc/../foo"
parseAbs :: MonadThrow m
         => OsPath -> m (Path Abs)
parseAbs filepath = do
  if | isAbsolute filepath
     , hasDrive filepath
     , isValid filepath
     , not (hasParentDir filepath) -> pure . MkPath . dropTrailingPathSeparator . normalise $ filepath
     | otherwise -> throwM (InvalidAbs filepath)


parseAbs' :: MonadThrow m
          => String -> m (Path Abs)
parseAbs' str = do
  fp <- AFP.encodeUtf str
  parseAbs fp


-- | Get a location for a relative path. Produces a normalised
-- path.
--
-- Note that @filepath@ may contain any number of @./@,
-- but not a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
-- >>> parseRel "abc"
-- "abc"
-- >>> parseRel "def/"
-- "def"
-- >>> parseRel "abc/def"
-- "abc/def"
-- >>> parseRel "abc/def/."
-- "abc/def"
-- >>> parseRel "/abc"
-- *** Exception: InvalidRel "/abc"
-- >>> parseRel ""
-- *** Exception: InvalidRel ""
-- >>> parseRel "abc/../foo"
-- *** Exception: InvalidRel "abc/../foo"
-- >>> parseRel "."
-- "."
-- >>> parseRel "././././."
-- "."
-- >>> parseRel "./..."
-- "..."
-- >>> parseRel ".."
-- *** Exception: InvalidRel ".."
parseRel :: MonadThrow m
         => OsPath -> m (Path Rel)
parseRel filepath = do
  if | not (isAbsolute filepath)
     , not (hasDrive filepath)
     , filepath /= [osp|..|]
     , not (hasParentDir filepath)
     , isValid filepath
     -> return . MkPath . dropTrailingPathSeparator . normalise $ filepath
     | otherwise
     -> throwM (InvalidRel filepath)

parseRel' :: MonadThrow m
          => String -> m (Path Rel)
parseRel' str = do
  fp <- AFP.encodeUtf str
  parseRel fp


-- | Parses a path, whether it's relative or absolute.
--
-- Throws: 'PathParseException'
--
-- >>> parseAny "/abc"
-- Left "/abc"
-- >>> parseAny "..."
-- Right "..."
-- >>> parseAny "abc/def"
-- Right "abc/def"
-- >>> parseAny "abc/def/."
-- Right "abc/def"
-- >>> parseAny "/abc"
-- Left "/abc"
-- >>> parseAny ""
-- *** Exception: InvalidRel ""
-- >>> parseAny "abc/../foo"
-- *** Exception: InvalidRel "abc/../foo"
-- >>> parseAny "."
-- Right "."
-- >>> parseAny ".."
-- *** Exception: InvalidRel ".."
parseAny :: MonadThrow m => OsPath -> m (Either (Path Abs) (Path Rel))
parseAny filepath = case parseAbs filepath of
  Just p -> pure $ Left p
  Nothing         -> case parseRel filepath of
    Just p -> pure $ Right p
    Nothing       -> throwM (InvalidRel filepath)

parseAny' :: MonadThrow m
          => String -> m (Either (Path Abs) (Path Rel))
parseAny' str = do
  fp <- AFP.encodeUtf str
  parseAny fp


-- | The @"."@ pwd path.
pwdPath :: Path Rel
pwdPath = MkPath [osp|.|]


--------------------------------------------------------------------------------
-- Path Conversion

-- | Convert any Path to an OsPath type.
toFilePath :: Path b -> OsPath
toFilePath (MkPath l) = l

-- | Convert an absolute Path to a OsPath type.
fromAbs :: Path Abs -> OsPath
fromAbs = toFilePath

-- | Convert a relative Path to a OsPath type.
fromRel :: Path Rel -> OsPath
fromRel = toFilePath

fromAny :: Either (Path Abs) (Path Rel) -> OsPath
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
-- >>> [abs|/|] </> [rel|file|]
-- "/file"
-- >>> [abs|/path/to|] </> [rel|file|]
-- "/path/to/file"
-- >>> [abs|/|] </> [rel|file/lal|]
-- "/file/lal"
-- >>> [abs|/|] </> [rel|.|]
-- "/"
-- >>> [rel|.|] </> [rel|.|]
-- "."
(</>) :: Path b -> Path Rel -> Path b
(</>) (MkPath a) (MkPath b) =
  MkPath (dropTrailingPathSeparator $ normalise (a AFP.</> b))


-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The bases must match.
--
-- >>> [abs|/lal/lad|]     `stripDir` [abs|/lal/lad/fad|]
-- "fad"
-- >>> [rel|lal/lad|]      `stripDir` [rel|lal/lad/fad|]
-- "fad"
-- >>> [abs|/|]            `stripDir` [abs|/|]
-- "."
-- >>> [abs|/lal/lad/fad|] `stripDir` [abs|/lal/lad|]
-- *** Exception: Couldn'tStripPrefixTPS "/lal/lad/fad" "/lal/lad"
-- >>> [abs|/abs|]         `stripDir` [abs|/lal/lad|]
-- *** Exception: Couldn'tStripPrefixTPS "/abs" "/lal/lad"
-- >>> [rel|fad|]          `stripDir` [rel|fad|]
-- "."
-- >>> [rel|.|]            `stripDir` [rel|.|]
-- "."
-- >>> [rel|.|]            `stripDir` [rel|.foo|]
-- *** Exception: Couldn'tStripPrefixTPS "." ".foo"
stripDir :: MonadThrow m => Path b -> Path b -> m (Path Rel)
stripDir (MkPath p) (MkPath l)
  | p == l = return pwdPath
  | otherwise = case L.stripPrefix (AFP.unpack $ addTrailingPathSeparator p) (AFP.unpack l) of
      Nothing -> throwM (Couldn'tStripPrefixTPS p l)
      Just ok -> return (MkPath $ AFP.pack ok)


-- |Get all parents of a path.
--
-- >>> getAllParents [abs|/abs/def/dod|]
-- ["/abs/def","/abs","/"]
-- >>> getAllParents [abs|/foo|]
-- ["/"]
-- >>> getAllParents [abs|/|]
-- []
getAllParents :: Path Abs -> [Path Abs]
getAllParents (MkPath p)
  | np == [osp|/|] = []
  | otherwise = dirname (MkPath np) : getAllParents (dirname $ MkPath np)
  where
    np = normalise p


-- | Gets all path components.
--
-- >>> getAllComponents [rel|abs/def/dod|]
-- ["abs","def","dod"]
-- >>> getAllComponents [rel|abs|]
-- ["abs"]
-- >>> getAllComponents [rel|.|]
-- ["."]
getAllComponents :: Path Rel -> [Path Rel]
getAllComponents (MkPath p) = fmap MkPath . splitDirectories $ p


-- | Gets all path components after the drive.
--
-- >>> getAllComponentsAfterRoot [abs|/abs/def/dod|]
-- ["abs","def","dod"]
-- >>> getAllComponentsAfterRoot [abs|/abs|]
-- ["abs"]
getAllComponentsAfterRoot :: Path Abs -> [Path Rel]
getAllComponentsAfterRoot (MkPath p) = getAllComponents (MkPath $ dropDrive p)


-- | Extract the directory name of a path.
--
-- >>> dirname [abs|/abc/def/dod|]
-- "/abc/def"
-- >>> dirname [abs|/|]
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
-- Throws: `PathException` if given a drive (e.g. "/")
--
-- >>> basename [abs|/abc/def/dod|]
-- "dod"
-- >>> basename [rel|abc/def/dod|]
-- "dod"
-- >>> basename [rel|dod|]
-- "dod"
-- >>> basename [rel|.|]
-- "."
-- >>> basename [abs|/|]
-- *** Exception: RootDirHasNoBasename
basename :: MonadThrow m => Path b -> m (Path Rel)
basename (MkPath l)
  | not (isAbsolute rl) = return $ MkPath rl
  | otherwise           = throwM RootDirHasNoBasename
  where
    rl = last . splitPath $ l

-- | Extract the file part of a relative path.
--
-- The following properties hold:
--
-- @basename' (p \<\/> a) == basename' a@
--
-- >>> basename' [rel|abc/def/dod|]
-- "dod"
-- >>> basename' [rel|dod|]
-- "dod"
-- >>> basename' [rel|.|]
-- "."
basename' :: Path Rel -> Path Rel
basename' (MkPath l) = MkPath . last . splitPath $ l


--------------------------------------------------------------------------------
-- Path Examination

-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
--
-- >>> [abs|/lal/lad|]     `isParentOf` [abs|/lal/lad/fad|]
-- True
-- >>> [rel|lal/lad|]      `isParentOf` [rel|lal/lad/fad|]
-- True
-- >>> [abs|/|]            `isParentOf` [abs|/|]
-- False
-- >>> [abs|/lal/lad/fad|] `isParentOf` [abs|/lal/lad|]
-- False
-- >>> [rel|fad|]          `isParentOf` [rel|fad|]
-- False
-- >>> [rel|.|]            `isParentOf` [rel|.foo|]
-- False
isParentOf :: Path b -> Path b -> Bool
isParentOf p l = case stripDir p l :: Maybe (Path Rel) of
  Nothing -> False
  Just ok
    | isPwdPath ok -> False
    | otherwise -> True


-- | Check whether the given Path is the pwd "." path.
--
-- >>> isPwdPath [rel|lal/lad|]
-- False
-- >>> isPwdPath [rel|.|]
-- True
isPwdPath :: Path Rel -> Bool
isPwdPath = (== pwdPath)


--------------------------------------------------------------------------------
-- Path IO helpers


withAbsPath :: Path Abs -> (OsPath -> IO a) -> IO a
withAbsPath (MkPath p) action = action p


withRelPath :: Path Rel -> (OsPath -> IO a) -> IO a
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

qq :: (String -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
  { quoteExp  = quoteExp'
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }

mkAbs :: String -> Q Exp
mkAbs = either (fail . show) lift . parseAbs'

mkRel :: String -> Q Exp
mkRel = either (fail . show) lift . parseRel'

-- | Quasiquote an absolute Path. This accepts Unicode Chars and will encode as UTF-8.
--
-- >>> [abs|/etc/profile|] :: Path Abs
-- "/etc/profile"
-- >>> [abs|/|] :: Path Abs
-- "/"
-- >>> (\(MkPath p) -> decodeUtf p) ([abs|/ƛ|] :: Path Abs)
-- "/\411"
abs :: QuasiQuoter
abs = qq mkAbs

-- | Quasiquote a relative Path. This accepts Unicode Chars and will encode as UTF-8.
--
-- >>> [rel|etc|] :: Path Rel
-- "etc"
-- >>> [rel|bar/baz|] :: Path Rel
-- "bar/baz"
-- >>> (\(MkPath p) -> decodeUtf p) ([rel|ƛ|] :: Path Rel)
-- "\411"
rel :: QuasiQuoter
rel = qq mkRel


hasParentDir :: OsPath -> Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
hasParentDir (OsString (WS fp)) =
#else
hasParentDir (OsString (PS fp)) =
#endif
  predicate (`BS.cons` pathDoubleDot)
   BS.isSuffixOf
 ||
  predicate (\sep -> BS.singleton sep
      `BS.append` pathDoubleDot
      `BS.append` BS.singleton sep)
   BS.isInfixOf
 ||
  predicate (BS.snoc pathDoubleDot)
    BS.isPrefixOf
  where
    pathDoubleDot = BS.pack [0x2e, 0x2e]
    predicate f p =
      foldr (\a b -> f a
              `p` fp || b)
            False
            Raw.pathSeparators
