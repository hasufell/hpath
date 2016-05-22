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
{-# LANGUAGE PatternSynonyms #-}

module HPath
  (
  -- * Types
   Abs
  ,Path
  ,Rel
  ,Fn
  ,PathParseException
  ,PathException
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
   -- * Path Parsing
  ,parseAbs
  ,parseFn
  ,parseRel
  -- * Path Conversion
  ,fromAbs
  ,fromRel
  ,toFilePath
  -- * Path Operations
  ,(</>)
  ,basename
  ,dirname
  ,isParentOf
  ,getAllParents
  ,stripDir
  -- * Path IO helpers
  ,withAbsPath
  ,withRelPath
  ,withFnPath
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
#if MIN_VERSION_bytestring(0,10,8)
import           Data.ByteString(ByteString, stripPrefix)
#else
import           Data.ByteString(ByteString)
#endif
import qualified Data.ByteString as BS
import           Data.Data
import qualified Data.List as L
import           Data.Maybe
import           Data.Word8
import           HPath.Internal
import           System.Posix.FilePath hiding ((</>))


--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root.
data Rel deriving (Typeable)

-- | A filename, without any '/'.
data Fn deriving (Typeable)

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbs ByteString
  | InvalidRel ByteString
  | InvalidFn ByteString
  | Couldn'tStripPrefixTPS ByteString ByteString
  deriving (Show,Typeable)
instance Exception PathParseException

data PathException = RootDirHasNoBasename
  deriving (Show,Typeable)
instance Exception PathException

instance RelC Rel
instance RelC Fn

--------------------------------------------------------------------------------
-- PatternSynonyms

pattern Path x <- (MkPath x)

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
-- Just "/abc/def/"
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
     then return (MkPath $ normalise filepath)
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
-- Just "def/"
-- >>> parseRel "abc/def"    :: Maybe (Path Rel)
-- Just "abc/def"
-- >>> parseRel "abc/def/."  :: Maybe (Path Rel)
-- Just "abc/def/"
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
     then return (MkPath $ normalise filepath)
     else throwM (InvalidRel filepath)


-- | Parses a filename. Filenames must not contain slashes.
-- Excludes '.' and '..'.
--
-- Throws: 'PathParseException'
--
-- >>> parseFn "abc"        :: Maybe (Path Fn)
-- Just "abc"
-- >>> parseFn "..."        :: Maybe (Path Fn)
-- Just "..."
-- >>> parseFn "def/"       :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/def"    :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/def/."  :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "/abc"       :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn ""           :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/../foo" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "."          :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn ".."         :: Maybe (Path Fn)
-- Nothing
parseFn :: MonadThrow m
        => ByteString -> m (Path Fn)
parseFn filepath =
  if isFileName filepath &&
     filepath /= BS.singleton _period &&
     filepath /= BS.pack [_period, _period] &&
     isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidFn filepath)



--------------------------------------------------------------------------------
-- Path Conversion

-- | Convert any Path to a ByteString type.
toFilePath :: Path b -> ByteString
toFilePath (MkPath l) = l

-- | Convert an absolute Path to a ByteString type.
fromAbs :: Path Abs -> ByteString
fromAbs = toFilePath

-- | Convert a relative Path to a ByteString type.
fromRel :: RelC r => Path r -> ByteString
fromRel = toFilePath



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
-- >>> (MkPath "/")        </> (MkPath "file/"    :: Path Rel)
-- "/file/"
(</>) :: RelC r => Path b -> Path r -> Path b
(</>) (MkPath a) (MkPath b) = MkPath (a' `BS.append` b)
  where
    a' = if BS.last a == pathSeparator
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


-- |Get all parents of a path.
--
-- >>> getAllParents (MkPath "/abs/def/dod")
-- ["/abs/def","/abs","/"]
-- >>> getAllParents (MkPath "/")
-- []
getAllParents :: Path Abs -> [Path Abs]
getAllParents (MkPath p)
  | np == BS.singleton pathSeparator = []
  | otherwise = dirname (MkPath np) : getAllParents (dirname $ MkPath np)
  where
    np = dropTrailingPathSeparator . normalise $ p


-- | Extract the directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname p@
--
-- >>> dirname (MkPath "/abc/def/dod")
-- "/abc/def"
-- >>> dirname (MkPath "/")
-- "/"
dirname :: Path Abs -> Path Abs
dirname (MkPath fp) = MkPath (takeDirectory $ dropTrailingPathSeparator fp)

-- | Extract the file part of a path.
--
--
-- The following properties hold:
--
-- @basename (p \<\/> a) == basename a@
--
-- Throws: `PathException` if given the root path "/"
--
-- >>> basename (MkPath "/abc/def/dod") :: Maybe (Path Fn)
-- Just "dod"
-- >>> basename (MkPath "/")            :: Maybe (Path Fn)
-- Nothing
basename :: MonadThrow m => Path b -> m (Path Fn)
basename (MkPath l)
  | not (isAbsolute rl) = return $ MkPath rl
  | otherwise           = throwM RootDirHasNoBasename
  where
    rl = last . splitPath . dropTrailingPathSeparator $ l


--------------------------------------------------------------------------------
-- Path IO helpers


withAbsPath :: Path Abs -> (ByteString -> IO a) -> IO a
withAbsPath (MkPath p) action = action p


withRelPath :: Path Rel -> (ByteString -> IO a) -> IO a
withRelPath (MkPath p) action = action p


withFnPath :: Path Fn -> (ByteString -> IO a) -> IO a
withFnPath (MkPath p) action = action p


------------------------
-- ByteString helpers

#if MIN_VERSION_bytestring(0,10,8)
#else
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix a b = BS.pack `fmap` L.stripPrefix (BS.unpack a) (BS.unpack b)
#endif
