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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module HPath
  (
  -- * Types
   Abs
  ,NoTPS
  ,Path
  ,Rel
  ,TPS
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
   -- * Parsing
  ,PathParseException
  ,parseAbsMaybeTPS
  ,parseAbsNoTPS
  ,parseAbsTPS
  ,parseRelMaybeTPS
  ,parseRelNoTPS
  ,parseRelTPS
  -- * Constructors
  ,mkAbsMaybeTPS
  ,mkAbsNoTPS
  ,mkAbsTPS
  ,mkRelMaybeTPS
  ,mkRelNoTPS
  ,mkRelTPS
  -- * Operations
  ,(</>)
  ,basename
  ,dirname
  ,isParentOf
  ,stripDir
  -- * Conversion
  ,fromAbsMaybeTPS
  ,fromAbsNoTPS
  ,fromAbsTPS
  ,fromRelMaybeTPS
  ,fromRelNoTPS
  ,fromRelTPS
  ,toFilePath
  ,toNoTPS
  ,toTPS
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           HPath.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root.
data Rel deriving (Typeable)

-- | A path without trailing separator.
data NoTPS deriving (Typeable)

-- | A path with trailing separator.
data TPS deriving (Typeable)

-- | A path without any guarantee about whether it ends in a
-- trailing path separators. Use `toTPS` and `toNoTPS`
-- if that guarantee is required.
data MaybeTPS deriving (Typeable)

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbsTPS FilePath
  | InvalidRelTPS FilePath
  | InvalidAbsNoTPS FilePath
  | InvalidRelNoTPS FilePath
  | InvalidAbsMaybeTPS FilePath
  | InvalidRelMaybeTPS FilePath
  | Couldn'tStripPrefixTPS FilePath FilePath
  deriving (Show,Typeable)
instance Exception PathParseException

--------------------------------------------------------------------------------
-- PatternSynonyms

pattern Path x <- (MkPath x)

--------------------------------------------------------------------------------
-- Parsers

-- | Get a location for an absolute path. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseAbsTPS :: MonadThrow m
            => FilePath -> m (Path Abs TPS)
parseAbsTPS filepath =
  if FilePath.isAbsolute filepath &&
     not (null (normalizeTPS filepath)) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (MkPath (normalizeTPS filepath))
     else throwM (InvalidAbsTPS filepath)

-- | Get a location for a relative path. Produces a normalized
-- path which always ends in a path separator.
--
-- Note that @filepath@ may contain any number of @./@ but may not consist
-- solely of @./@.  It also may not contain a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRelTPS :: MonadThrow m
            => FilePath -> m (Path Rel TPS)
parseRelTPS filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeTPS filepath)) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (MkPath (normalizeTPS filepath))
     else throwM (InvalidRelTPS filepath)

-- | Get a location for an absolute path, which must not end with a trailing
-- path separator.
--
-- Throws: 'PathParseException'
--
parseAbsNoTPS :: MonadThrow m
              => FilePath -> m (Path Abs NoTPS)
parseAbsNoTPS filepath =
  if FilePath.isAbsolute filepath &&
     not (FilePath.hasTrailingPathSeparator filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeNoTPS filepath)) &&
     FilePath.isValid filepath
     then return (MkPath (normalizeNoTPS filepath))
     else throwM (InvalidAbsNoTPS filepath)

-- | Get a location for a relative path, which must not end with a trailing
-- path separator.
--
-- Note that @filepath@ may contain any number of @./@ but may not contain a
-- single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRelNoTPS :: MonadThrow m
              => FilePath -> m (Path Rel NoTPS)
parseRelNoTPS filepath =
  if not (FilePath.isAbsolute filepath ||
          FilePath.hasTrailingPathSeparator filepath) &&
     not (null filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeNoTPS filepath)) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (MkPath (normalizeNoTPS filepath))
     else throwM (InvalidRelNoTPS filepath)

-- | Get a location for an absolute path that may or may not end in a trailing
-- path separator. Use `toTPS` and `toNoTPS` if that guarantee is required.
--
-- Throws: 'PathParseException'
--
parseAbsMaybeTPS :: MonadThrow m
                 => FilePath -> m (Path Abs MaybeTPS)
parseAbsMaybeTPS filepath =
  if FilePath.isAbsolute filepath &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeNoTPS filepath)) &&
     FilePath.isValid filepath
     then return (MkPath (normalizeNoTPS filepath))
     else throwM (InvalidAbsMaybeTPS filepath)

-- | Get a location for a relative path that may or may not end in a trailing
-- path separator. Use `toTPS` and `toNoTPS` if that guarantee is required.
--
-- Note that @filepath@ may contain any number of @./@ but may not contain a
-- single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRelMaybeTPS :: MonadThrow m
                 => FilePath -> m (Path Rel MaybeTPS)
parseRelMaybeTPS filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeNoTPS filepath)) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (MkPath (normalizeNoTPS filepath))
     else throwM (InvalidRelMaybeTPS filepath)

-- | Helper function: check if the filepath has any parent directories in it.
-- This handles the logic of checking for different path separators on Windows.
hasParentDir :: FilePath -> Bool
hasParentDir filepath' =
     ("/.." `isSuffixOf` filepath) ||
     ("/../" `isInfixOf` filepath) ||
     ("../" `isPrefixOf` filepath)
  where
    filepath =
        case FilePath.pathSeparator of
            '/' -> filepath'
            x   -> map (\y -> if x == y then '/' else y) filepath'

--------------------------------------------------------------------------------
-- Constructors

-- | Make a 'Path Abs TPS'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsTPS :: FilePath -> Q Exp
mkAbsTPS s =
  case parseAbsTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Abs TPS|]

-- | Make a 'Path Rel TPS'.
mkRelTPS :: FilePath -> Q Exp
mkRelTPS s =
  case parseRelTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Rel TPS|]

-- | Make a 'Path Abs NoTPS'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsNoTPS :: FilePath -> Q Exp
mkAbsNoTPS s =
  case parseAbsNoTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Abs NoTPS|]

-- | Make a 'Path Rel NoTPS'.
mkRelNoTPS :: FilePath -> Q Exp
mkRelNoTPS s =
  case parseRelNoTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Rel NoTPS|]

-- | Make a 'Path Rel MaybeTPS'.
mkAbsMaybeTPS :: FilePath -> Q Exp
mkAbsMaybeTPS s =
  case parseAbsMaybeTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Abs MaybeTPS|]

-- | Make a 'Path Rel MaybeTPS'.
mkRelMaybeTPS :: FilePath -> Q Exp
mkRelMaybeTPS s =
  case parseRelMaybeTPS s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Rel MaybeTPS|]

--------------------------------------------------------------------------------
-- Conversion

-- | Convert to a 'FilePath' type.
--
-- All TPS data types have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b t -> FilePath
toFilePath (MkPath l) = l

fromAbsTPS :: Path Abs TPS -> FilePath
fromAbsTPS = toFilePath

fromRelTPS :: Path Rel TPS -> FilePath
fromRelTPS = toFilePath

fromAbsNoTPS :: Path Abs NoTPS -> FilePath
fromAbsNoTPS = toFilePath

fromRelNoTPS :: Path Rel NoTPS -> FilePath
fromRelNoTPS = toFilePath

fromAbsMaybeTPS :: Path Abs MaybeTPS -> FilePath
fromAbsMaybeTPS = toFilePath

fromRelMaybeTPS :: Path Rel MaybeTPS -> FilePath
fromRelMaybeTPS = toFilePath

toTPS :: Path b MaybeTPS -> Path b TPS
toTPS (MkPath l) = MkPath (FilePath.addTrailingPathSeparator l)

toNoTPS :: Path b MaybeTPS -> Path b NoTPS
toNoTPS (MkPath l) = MkPath (FilePath.dropTrailingPathSeparator l)

--------------------------------------------------------------------------------
-- Operations

-- | Append two paths.
--
-- The second argument must always be a relative path, which ensures
-- that undefinable things like `"/abc" </> "/def"` cannot happen.
--
-- Technically, the first argument can be a path that points to a non-directory,
-- because this library is IO-agnostic and makes no assumptions about
-- file types.
(</>) :: Path b t1 -> Path Rel t2 -> Path b t2
(</>) (MkPath a) (MkPath b) = MkPath (a' ++ b)
  where
    a' = FilePath.addTrailingPathSeparator a

-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The bases must match.
--
stripDir :: MonadThrow m
         => Path b t1 -> Path b t2 -> m (Path Rel t2)
stripDir (MkPath p) (MkPath l) =
  case stripPrefix p' l of
    Nothing -> throwM (Couldn'tStripPrefixTPS p' l)
    Just "" -> throwM (Couldn'tStripPrefixTPS p' l)
    Just ok -> return (MkPath ok)
  where
    p' = FilePath.addTrailingPathSeparator p

-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
isParentOf :: Path b t1 -> Path b t2 -> Bool
isParentOf p l =
  isJust (stripDir p l)

-- | Extract the directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname p@
--
dirname :: Path Abs t -> Path Abs TPS
dirname (MkPath fp) = MkPath (normalizeTPS (FilePath.takeDirectory $ FilePath.dropTrailingPathSeparator fp))

-- | Extract the file part of a path.
--
-- Throws InvalidRelTPS if it's passed e.g. '/', because there is no
-- basename for that and it would break the `Path Rel t` type.
--
-- The following properties hold:
--
-- @basename (p \<\/> a) == basename a@
--
basename :: MonadThrow m => Path b t -> m (Path Rel t)
basename (MkPath l)
  | not (FilePath.isAbsolute rl) = return $ MkPath rl
  | otherwise                    = throwM (InvalidRelTPS rl)
  where
    rl = case FilePath.hasTrailingPathSeparator l of
           True  -> last (FilePath.splitPath l)
           False -> normalizeNoTPS (FilePath.takeFileName l)

--------------------------------------------------------------------------------
-- Internal functions

-- | Internal use for normalizing a path while always adding
-- a trailing path separator.
normalizeTPS :: FilePath -> FilePath
normalizeTPS =
  clean . FilePath.addTrailingPathSeparator . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x

-- | Internal use for normalizing a path without adding or removing
-- a trailing path separator.
normalizeNoTPS :: FilePath -> FilePath
normalizeNoTPS =
  clean . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x

