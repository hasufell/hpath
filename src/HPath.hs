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
{-# OPTIONS_HADDOCK ignore-exports #-}

module HPath
  (
  -- * Types
   Abs
  ,Path
  ,Rel
  ,Fn
  ,PathParseException
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
   -- * Path Parsing
  ,parseAbs
  ,parseFn
  ,parseRel
  -- * Path Conversion
  ,canonicalizePath
  ,fromAbs
  ,fromRel
  ,normalize
  ,toFilePath
  -- * Path Operations
  ,(</>)
  ,basename
  ,dirname
  ,isParentOf
  ,getAllParents
  ,stripDir
  -- * ByteString/Word8 constants
  ,nullByte
  ,pathDot
  ,pathDot'
  ,pathSeparator
  ,pathSeparator'
  -- * ByteString operations
  ,addTrailingPathSeparator
  ,combine
  ,dropFileName
  ,dropTrailingPathSeparator
  ,fpToString
  ,joinPath
  ,normalise
  ,splitDirectories
  ,splitFileName
  ,splitPath
  ,takeDirectory
  ,userStringToFP
  -- * ByteString Query functions
  ,hiddenFile
  -- * Queries
  ,hasParentDir
  ,isAbsolute
  ,isFileName
  ,isRelative
  ,isValid
  -- * String based functions
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Data
import qualified Data.List as L
import           Data.Maybe
import           Data.Word8
import           HPath.Internal
import           System.Posix.FilePath hiding ((</>))
import           System.Posix.Directory.Traversals(realpath)


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
-- >>> parseAbs "/abc" :: Maybe (Path Abs)
-- Just "/abc"
-- >>> parseAbs "/" :: Maybe (Path Abs)
-- Just "/"
-- >>> parseAbs "/abc/def" :: Maybe (Path Abs)
-- Just "/abc/def"
-- >>> parseAbs "/abc/def/.///" :: Maybe (Path Abs)
-- Just "/abc/def/"
-- >>> parseAbs "abc" :: Maybe (Path Abs)
-- Nothing
-- >>> parseAbs "" :: Maybe (Path Abs)
-- Nothing
-- >>> parseAbs "/abc/../foo" :: Maybe (Path Abs)
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
-- >>> parseRel "abc" :: Maybe (Path Rel)
-- Just "abc"
-- >>> parseRel "def/" :: Maybe (Path Rel)
-- Just "def/"
-- >>> parseRel "abc/def" :: Maybe (Path Rel)
-- Just "abc/def"
-- >>> parseRel "abc/def/." :: Maybe (Path Rel)
-- Just "abc/def/"
-- >>> parseRel "/abc" :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel "" :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel "abc/../foo" :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel "." :: Maybe (Path Rel)
-- Nothing
-- >>> parseRel ".." :: Maybe (Path Rel)
-- Nothing
parseRel :: MonadThrow m
         => ByteString -> m (Path Rel)
parseRel filepath =
  if not (isAbsolute filepath) &&
     filepath /= pathDot' && filepath /= pathDoubleDot &&
     not (hasParentDir filepath) &&
     isValid filepath
     then return (MkPath $ normalise filepath)
     else throwM (InvalidRel filepath)


-- | Parses a filename. Filenames must not contain slashes.
-- Excludes '.' and '..'.
--
-- Throws: 'PathParseException'
--
-- >>> parseFn "abc" :: Maybe (Path Fn)
-- Just "abc"
-- >>> parseFn "..." :: Maybe (Path Fn)
-- Just "..."
-- >>> parseFn "def/" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/def" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/def/." :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "/abc" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "abc/../foo" :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn "." :: Maybe (Path Fn)
-- Nothing
-- >>> parseFn ".." :: Maybe (Path Fn)
-- Nothing
parseFn :: MonadThrow m
        => ByteString -> m (Path Fn)
parseFn filepath =
  if isFileName filepath &&
     filepath /= pathDot' && filepath /= pathDoubleDot &&
     isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidFn filepath)



--------------------------------------------------------------------------------
-- Path Conversion

-- | Convert to a ByteString type.
toFilePath :: Path b -> ByteString
toFilePath (MkPath l) = l

fromAbs :: Path Abs -> ByteString
fromAbs = toFilePath

fromRel :: RelC r => Path r -> ByteString
fromRel = toFilePath

normalize :: Path t -> Path t
normalize (MkPath l) = MkPath $ normalise l

-- | May fail on `realpath`.
canonicalizePath :: Path Abs -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- realpath l
  return $ MkPath nl

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
-- >>> (MkPath "/") </> (MkPath "file" :: Path Rel)
-- "/file"
-- >>> (MkPath "/path/to") </> (MkPath "file" :: Path Rel)
-- "/path/to/file"
-- >>> (MkPath "/") </> (MkPath "file/lal" :: Path Rel)
-- "/file/lal"
-- >>> (MkPath "/") </> (MkPath "file/" :: Path Rel)
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
-- >>> (MkPath "/lal/lad") `stripDir` (MkPath "/lal/lad/fad") :: Maybe (Path Rel)
-- Just "fad"
-- >>> (MkPath "lal/lad") `stripDir` (MkPath "lal/lad/fad") :: Maybe (Path Rel)
-- Just "fad"
-- >>> (MkPath "/") `stripDir` (MkPath "/") :: Maybe (Path Rel)
-- Nothing
-- >>> (MkPath "/lal/lad/fad") `stripDir` (MkPath "/lal/lad") :: Maybe (Path Rel)
-- Nothing
-- >>> (MkPath "fad") `stripDir` (MkPath "fad") :: Maybe (Path Rel)
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
-- >>> (MkPath "/lal/lad") `isParentOf` (MkPath "/lal/lad/fad")
-- True
-- >>> (MkPath "lal/lad") `isParentOf` (MkPath "lal/lad/fad")
-- True
-- >>> (MkPath "/") `isParentOf` (MkPath "/")
-- False
-- >>> (MkPath "/lal/lad/fad") `isParentOf` (MkPath "/lal/lad")
-- False
-- >>> (MkPath "fad") `isParentOf` (MkPath "fad")
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
  | np == pathSeparator' = []
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
-- >>> basename (MkPath "/") :: Maybe (Path Fn)
-- Nothing
basename :: MonadThrow m => Path b -> m (Path Fn)
basename (MkPath l)
  | not (isAbsolute rl) = return $ MkPath rl
  | otherwise           = throwM RootDirHasNoBasename
  where
    rl = last . splitPath . dropTrailingPathSeparator $ l


--------------------------------------------------------------------------------
-- ByteString Query functions


-- | Whether the file is a hidden file.
--
-- >>> hiddenFile (MkPath ".foo")
-- True
-- >>> hiddenFile (MkPath "..foo.bar")
-- True
-- >>> hiddenFile (MkPath "...")
-- True
-- >>> hiddenFile (MkPath "dod")
-- False
-- >>> hiddenFile (MkPath "dod.bar")
-- False
hiddenFile :: Path Fn -> Bool
hiddenFile (MkPath fp)
  | fp == pathDoubleDot = False
  | fp == pathDot'      = False
  | otherwise           = pathDot' `BS.isPrefixOf` fp
 

--------------------------------------------------------------------------------
-- ByteString/Word8 constants

pathSeparator' :: ByteString
pathSeparator' = BS.singleton pathSeparator


pathDot :: Word8
pathDot = _period


pathDot' :: ByteString
pathDot' = BS.singleton pathDot


pathDoubleDot :: ByteString
pathDoubleDot = pathDot `BS.cons` pathDot'


nullByte :: Word8
nullByte = _nul


--------------------------------------------------------------------------------
-- ByteString Operations



-- |Normalise a file.
--
-- >>> normalise "/file/\\test////"
-- "/file/\\test/"
-- >>> normalise "/file/./test"
-- "/file/test"
-- >>> normalise "/test/file/../bob/fred/"
-- "/test/file/../bob/fred/"
-- >>> normalise "../bob/fred/"
-- "../bob/fred/"
-- >>> normalise "./bob/fred/"
-- "bob/fred/"
-- >>> normalise "./bob////.fred/./...///./..///#."
-- "bob/.fred/.../../#."
-- >>> normalise "."
-- "."
-- >>> normalise "./"
-- "./"
-- >>> normalise "./."
-- "./"
-- >>> normalise "/./"
-- "/"
-- >>> normalise "/"
-- "/"
-- >>> normalise "bob/fred/."
-- "bob/fred/"
-- >>> normalise "//home"
-- "/home"
normalise :: ByteString -> ByteString
normalise filepath =
  result `BS.append`
  (if addPathSeparator
       then BS.singleton pathSeparator
       else BS.empty)
  where
    result = let n = f filepath
             in if BS.null n
                then BS.singleton _period
                else n
    addPathSeparator = isDirPath filepath &&
      not (hasTrailingPathSeparator' result)
    isDirPath xs = hasTrailingPathSeparator' xs
        || not (BS.null xs) && BS.last xs == _period
           && hasTrailingPathSeparator' (BS.init xs)
    f = joinPath . dropDots . propSep . splitDirectories
    propSep :: [ByteString] -> [ByteString]
    propSep (x:xs)
      | BS.all (== pathSeparator) x = BS.singleton pathSeparator : xs
      | otherwise                   = x : xs
    propSep [] = []
    dropDots :: [ByteString] -> [ByteString]
    dropDots = filter (BS.singleton _period /=)
    hasTrailingPathSeparator' :: RawFilePath -> Bool
    hasTrailingPathSeparator' x
        | BS.null x = False
        | otherwise = isPathSeparator $ BS.last x



-- |Uses UTF-8 decoding to convert the bytestring into a String.
fpToString :: ByteString -> String
fpToString = toString


-- |Uses UTF-8 encoding to convert a user provided String into
-- a ByteString, which represents a filepath.
userStringToFP :: String -> ByteString
userStringToFP = fromString


#if MIN_VERSION_bytestring(0,10,8)
#else
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix a b = BS.pack `fmap` L.stripPrefix (BS.unpack a) (BS.unpack b)
#endif

--------------------------------------------------------------------------------
-- ByteString Query functions

-- | Helper function: check if the filepath has any parent directories in it.
--
-- >>> hasParentDir "/.."
-- True
-- >>> hasParentDir "foo/bar/.."
-- True
-- >>> hasParentDir "foo/../bar/."
-- True
-- >>> hasParentDir "foo/bar"
-- False
-- >>> hasParentDir "foo"
-- False
-- >>> hasParentDir ""
-- False
-- >>> hasParentDir ".."
-- False
hasParentDir :: ByteString -> Bool
hasParentDir filepath =
  ((pathSeparator `BS.cons` pathDoubleDot)  `BS.isSuffixOf` filepath) ||
  ((pathSeparator' `BS.append` pathDoubleDot `BS.append` pathSeparator')
      `BS.isInfixOf`  filepath) ||
  ((pathDoubleDot `BS.append` pathSeparator') `BS.isPrefixOf` filepath)


-- | Is the given filename a valid filename?
--
-- >>> isFileName "lal"
-- True
-- >>> isFileName "."
-- True
-- >>> isFileName ".."
-- True
-- >>> isFileName ""
-- False
-- >>> isFileName "\0"
-- False
-- >>> isFileName "/random_ path:*"
-- False
isFileName :: ByteString -> Bool
isFileName filepath =
  not (pathSeparator' `BS.isInfixOf` filepath) &&
  not (BS.null filepath) &&
  not (nullByte `BS.elem` filepath)


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
-- >>> isValid "/random_ path:*"
-- True
isValid :: RawFilePath -> Bool
isValid filepath
  | BS.null filepath        = False
  | _nul `BS.elem` filepath = False
  | otherwise               = True

