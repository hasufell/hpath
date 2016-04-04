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



{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ,dropWhileEnd
  ,joinPath
  ,normalise
  ,splitDirectories
  ,splitFileName
  ,splitPath
  ,stripPrefix
  ,takeDirectory
  -- * Queries
  ,hasDot
  ,hasDoublePS
  ,hasLeadingPathSeparator
  ,hasParentDir
  ,hasTrailingPathSeparator
  ,isAbsolute
  ,isFileName
  ,isRelative
  ,isValid
  -- * String based functions
  ,realPath
  )
  where

import           Control.Exception (Exception)
import           Control.Monad(void)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.ByteString(ByteString)
import qualified Data.ByteString as B
import           Data.Char(ord)
import           Data.Data
import qualified Data.List as L
import           Data.Maybe
import           Data.Word(Word8)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.Marshal.Alloc(allocaBytes)
import           HPath.Foreign
import           HPath.Internal

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

instance RelC Rel
instance RelC Fn

--------------------------------------------------------------------------------
-- PatternSynonyms

pattern Path x <- (MkPath x)

--------------------------------------------------------------------------------
-- Path Parsers

-- | Get a location for an absolute path.
--
-- Throws: 'PathParseException'
--
parseAbs :: MonadThrow m
         => ByteString -> m (Path Abs)
parseAbs filepath =
  if isAbsolute filepath &&
     not (B.null filepath) &&
     isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidAbs filepath)

-- | Get a location for a relative path. Produces a normalized
-- path which always ends in a path separator.
--
-- Note that @filepath@ may contain any number of @./@ but may not consist
-- solely of @./@.  It also may not contain a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRel :: MonadThrow m
         => ByteString -> m (Path Rel)
parseRel filepath =
  if not (isAbsolute filepath) &&
     not (B.null filepath) &&
     isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidRel filepath)

parseFn :: MonadThrow m
        => ByteString -> m (Path Fn)
parseFn filepath =
  if not (isAbsolute filepath) &&
     not (B.null filepath) &&
     isFileName filepath &&
     isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidFn filepath)


--------------------------------------------------------------------------------
-- Path Conversion

-- | Convert to a ByteString type.
--
-- All TPS data types have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b -> ByteString
toFilePath (MkPath l) = l

fromAbs :: Path Abs -> ByteString
fromAbs = toFilePath

fromRel :: RelC r => Path r -> ByteString
fromRel = toFilePath

normalize :: Path t -> Path t
normalize (MkPath l) = MkPath $ normalise l

-- | May fail on `realPath`.
canonicalizePath :: Path Abs -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- realPath l
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
(</>) :: RelC r => Path b -> Path r -> Path b
(</>) (MkPath a) (MkPath b) = MkPath (a' `B.append` b)
  where
    a' = addTrailingPathSeparator a

-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The bases must match.
--
stripDir :: MonadThrow m
         => Path b -> Path b -> m (Path Rel)
stripDir (MkPath p) (MkPath l) =
  case stripPrefix p' l of
    Nothing -> throwM (Couldn'tStripPrefixTPS p' l)
    Just "" -> throwM (Couldn'tStripPrefixTPS p' l)
    Just ok -> return (MkPath ok)
  where
    p' = addTrailingPathSeparator p

-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
isParentOf :: Path b -> Path b -> Bool
isParentOf p l = isJust (stripDir p l :: Maybe (Path Rel))


getAllParents :: Path Abs -> [Path Abs]
getAllParents (MkPath p) =
  case np of
    (MkPath "/") -> []
    _            -> dirname np : getAllParents (dirname np)
  where
    np = MkPath . dropTrailingPathSeparator . normalise $ p


-- | Extract the directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname p@
--
dirname :: Path Abs -> Path Abs
dirname (MkPath fp) = MkPath (takeDirectory $ dropTrailingPathSeparator fp)

-- | Extract the file part of a path.
--
--
-- The following properties hold:
--
-- @basename (p \<\/> a) == basename a@
--
-- Except when "/" is passed in which case the filename "." is returned.
basename :: Path b -> Path Fn
basename (MkPath l)
  | not (isAbsolute rl) = MkPath rl
  | otherwise                    = MkPath "."
  where
    rl = last . splitPath . dropTrailingPathSeparator $ l


--------------------------------------------------------------------------------
-- ByteString/Word8 constants

pathSeparator :: Word8
pathSeparator = fromIntegral (ord '/')

pathSeparator' :: ByteString
pathSeparator' = "/"


pathDot :: Word8
pathDot = fromIntegral (ord '.')


pathDot' :: ByteString
pathDot' = "."


nullByte :: Word8
nullByte = fromIntegral (ord '\0')


--------------------------------------------------------------------------------
-- ByteString Operations


dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd p = B.reverse . B.dropWhile p . B.reverse


dropTrailingPathSeparator :: ByteString -> ByteString
dropTrailingPathSeparator filepath =
  if hasTrailingPathSeparator filepath
  then let filepath' = dropWhileEnd (== pathSeparator) filepath
    in if B.null filepath' then B.singleton . B.last $ filepath else filepath'
  else filepath


addTrailingPathSeparator :: ByteString -> ByteString
addTrailingPathSeparator filepath
  | B.null filepath            = filepath
  | filepath == pathSeparator' = filepath
  | not (hasTrailingPathSeparator filepath)
                               = filepath `B.append` pathSeparator'
  | otherwise                  = filepath


normalise :: ByteString -> ByteString
normalise filepath = result `B.append`
                     (if addPathSeparator then pathSeparator' else B.empty)
  where
    (drv, pth) = splitDrive filepath
    result = joinDrive' (normaliseDrive drv) (f pth)
    joinDrive' d p
      | d == "" && p == "" = B.singleton pathDot
      | otherwise          = joinDrive d p
    addPathSeparator = isDirPath pth && not (hasTrailingPathSeparator result)
    isDirPath xs = hasTrailingPathSeparator xs
        || not (B.null xs) && B.last xs == pathDot
           && hasTrailingPathSeparator (B.init xs)
    normaliseDrive p
      | p == ""   = ""
      | otherwise = B.singleton pathSeparator
    f = joinPath . dropDots . propSep . splitDirectories
    propSep :: [ByteString] -> [ByteString]
    propSep (x:xs)
      | B.all (== pathSeparator) x = pathSeparator' : xs
      | otherwise = x : xs
    propSep [] = []
    dropDots = filter (pathDot' /=)


splitPath :: ByteString -> [ByteString]
splitPath filepath = [drv | drv /= ""] ++ f pth
  where
    (drv, pth) = splitDrive filepath
    f p
      | p == ""   = []
      | otherwise = (a `B.append` c) : f d
        where
          (a, b) = B.break (== pathSeparator) p
          (c, d) = splitDrive b


joinPath :: [ByteString] -> ByteString
joinPath = foldr combine ""


splitDrive :: ByteString -> (ByteString, ByteString)
splitDrive = B.span (== pathSeparator)


joinDrive :: ByteString -> ByteString -> ByteString
joinDrive = combineAlways


splitDirectories :: ByteString -> [ByteString]
splitDirectories = map dropTrailingPathSeparator . splitPath


combine :: ByteString -> ByteString -> ByteString
combine d p
  | hasLeadingPathSeparator p = p
  | otherwise                 = combineAlways d p


combineAlways :: ByteString -> ByteString -> ByteString
combineAlways d p
  | B.null d = p
  | B.null p = d
  | hasTrailingPathSeparator d = d `B.append` p
  | otherwise = d `B.append` B.singleton pathSeparator `B.append` p


takeDirectory :: ByteString -> ByteString
takeDirectory = dropTrailingPathSeparator . dropFileName


dropFileName :: ByteString -> ByteString
dropFileName = fst . splitFileName


splitFileName :: ByteString -> (ByteString, ByteString)
splitFileName filepath = (if B.null dir then "./" else dir, name)
  where
    (dir, name) = splitFileName_ filepath
    splitFileName_ p = (drv `B.append` dir', file)
      where
        (drv, pth)  = splitDrive p
        (dir', file) = B.breakEnd (== pathSeparator) pth


stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix a b = B.pack `fmap` L.stripPrefix (B.unpack a) (B.unpack b)


--------------------------------------------------------------------------------
-- ByteString Query functions

-- | Helper function: check if the filepath has any parent directories in it.
hasParentDir :: ByteString -> Bool
hasParentDir filepath =
     ("/.."  `B.isSuffixOf` filepath) ||
     ("/../" `B.isInfixOf`  filepath) ||
     ("../"  `B.isPrefixOf` filepath)

hasDot :: ByteString -> Bool
hasDot filepath =
     ("/."  `B.isSuffixOf` filepath) ||
     ("/./" `B.isInfixOf`  filepath) ||
     ("./"  `B.isPrefixOf` filepath)

hasDoublePS :: ByteString -> Bool
hasDoublePS filepath =
     ("//" `B.isInfixOf` filepath)


hasTrailingPathSeparator :: ByteString -> Bool
hasTrailingPathSeparator filepath
  | B.null filepath                  = False
  | B.last filepath == pathSeparator = True
  | otherwise                        = False


hasLeadingPathSeparator :: ByteString -> Bool
hasLeadingPathSeparator filepath
  | B.null filepath                  = False
  | B.head filepath == pathSeparator = True
  | otherwise                        = False


isFileName :: ByteString -> Bool
isFileName filepath =
     not ("/" `B.isInfixOf` filepath)


isAbsolute :: ByteString -> Bool
isAbsolute filepath
  | B.null filepath                  = False
  | B.head filepath == pathSeparator = True
  | otherwise                        = False


isRelative :: ByteString -> Bool
isRelative = not . isAbsolute


isValid :: ByteString -> Bool
isValid filepath
  | B.null filepath            = False
  | filepath ==  ""            = False
  | nullByte `B.elem` filepath = False
  | otherwise                  = True



--------------------------------------------------------------------------------
-- String based path functions

foreign import ccall "realpath"
  c_realpath :: CString -> CString -> IO CString

-- | return the canonicalized absolute pathname
--
-- like canonicalizePath, but uses realpath(3)
realPath :: ByteString -> IO ByteString
realPath inp =
  allocaBytes pathMax $ \tmp -> do
      void $ B.useAsCString inp
           $ \cstr -> throwErrnoIfNull "realpath"
           $ c_realpath cstr tmp
      B.packCString tmp

