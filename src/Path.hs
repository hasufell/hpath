-- |
-- Module      :  Path
-- Copyright   :  © 2015–2016 FP Complete
-- License     :  BSD 3 clause
--
-- Maintainer  :  Chris Done <chrisdone@fpcomplete.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for well-typed paths.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module Path
  (-- * Types
   Path
  ,Abs
  ,Rel
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
  ,PathParseException
  -- * Constructors
  ,mkAbsDir
  ,mkRelDir
  ,mkAbsFile
  ,mkRelFile
  -- * Operations
  ,(</>)
  ,stripDir
  ,isParentOf
  ,parent
  ,filename
  -- * Conversion
  ,toFilePath
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Path.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root.
data Rel deriving (Typeable)


-- | Exception when parsing a location.
data PathParseException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  | Couldn'tStripPrefixDir FilePath FilePath
  | InvalidTypeCombination
  deriving (Show,Typeable)
instance Exception PathParseException

--------------------------------------------------------------------------------
-- Parsers

-- | Get a location for an absolute directory. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseAbsDir :: MonadThrow m
            => FilePath -> m (Path Abs)
parseAbsDir filepath =
  if FilePath.isAbsolute filepath &&
     not (null (normalizeDir filepath)) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (DPath (normalizeDir filepath))
     else throwM (InvalidAbsDir filepath)

-- | Get a location for a relative directory. Produces a normalized
-- path which always ends in a path separator.
--
-- Note that @filepath@ may contain any number of @./@ but may not consist solely of @./@.  It also may not contain a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path Rel)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeDir filepath)) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (DPath (normalizeDir filepath))
     else throwM (InvalidRelDir filepath)

-- | Get a location for an absolute file.
--
-- Throws: 'PathParseException'
--
parseAbsFile :: MonadThrow m
             => FilePath -> m (Path Abs)
parseAbsFile filepath =
  if FilePath.isAbsolute filepath &&
     not (FilePath.hasTrailingPathSeparator filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeFile filepath)) &&
     FilePath.isValid filepath
     then return (FPath (normalizeFile filepath))
     else throwM (InvalidAbsFile filepath)

-- | Get a location for a relative file.
--
-- Note that @filepath@ may contain any number of @./@ but may not contain a single @..@ anywhere.
--
-- Throws: 'PathParseException'
--
parseRelFile :: MonadThrow m
             => FilePath -> m (Path Rel)
parseRelFile filepath =
  if not (FilePath.isAbsolute filepath ||
          FilePath.hasTrailingPathSeparator filepath) &&
     not (null filepath) &&
     not ("~/" `isPrefixOf` filepath) &&
     not (hasParentDir filepath) &&
     not (null (normalizeFile filepath)) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (FPath (normalizeFile filepath))
     else throwM (InvalidRelFile filepath)

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

-- | Make a 'Path Abs Dir'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsDir :: FilePath -> Q Exp
mkAbsDir s =
  case parseAbsDir s of
    Left err -> error (show err)
    Right (DPath str) ->
      [|DPath $(return (LitE (StringL str))) :: Path Abs|]
    _ -> error "Invalid Type"

-- | Make a 'Path Rel Dir'.
mkRelDir :: FilePath -> Q Exp
mkRelDir s =
  case parseRelDir s of
    Left err -> error (show err)
    Right (DPath str) ->
      [|DPath $(return (LitE (StringL str))) :: Path Rel|]
    _ -> error "Invalid Type"

-- | Make a 'Path Abs File'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsFile :: FilePath -> Q Exp
mkAbsFile s =
  case parseAbsFile s of
    Left err -> error (show err)
    Right (FPath str) ->
      [|FPath $(return (LitE (StringL str))) :: Path Abs|]
    _ -> error "Invalid Type"

-- | Make a 'Path Rel File'.
mkRelFile :: FilePath -> Q Exp
mkRelFile s =
  case parseRelFile s of
    Left err -> error (show err)
    Right (FPath str) ->
      [|FPath $(return (LitE (StringL str))) :: Path Rel|]
    _ -> error "Invalid Type"

--------------------------------------------------------------------------------
-- Conversion

-- | Convert to a 'FilePath' type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b -> FilePath
toFilePath (FPath l) = l
toFilePath (DPath l) = l


--------------------------------------------------------------------------------
-- Operations

-- | Append two paths.
--
-- The following cases are valid and the equalities hold:
--
-- @$(mkAbsDir x) \<\/> $(mkRelDir y) = $(mkAbsDir (x ++ \"/\" ++ y))@
--
-- @$(mkAbsDir x) \<\/> $(mkRelFile y) = $(mkAbsFile (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelDir y) = $(mkRelDir (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelFile y) = $(mkRelFile (x ++ \"/\" ++ y))@
--
-- The following are proven not possible to express:
--
-- @$(mkAbsFile …) \<\/> x@
--
-- @$(mkRelFile …) \<\/> x@
--
-- @x \<\/> $(mkAbsFile …)@
--
-- @x \<\/> $(mkAbsDir …)@
--
(</>) :: MonadThrow m => Path b -> Path Rel -> m (Path b)
(</>) (DPath a) (FPath b) = return $ FPath (a ++ b)
(</>) (DPath a) (DPath b) = return $ DPath (a ++ b)
(</>) _         _         = throwM InvalidTypeCombination

-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The following properties hold:
--
-- @stripDir x (x \<\/> y) = y@
--
-- Cases which are proven not possible:
--
-- @stripDir (a :: Path Abs …) (b :: Path Rel …)@
--
-- @stripDir (a :: Path Rel …) (b :: Path Abs …)@
--
-- In other words the bases must match.
--
stripDir :: MonadThrow m
         => Path b -> Path b -> m (Path Rel)
stripDir (DPath p) (FPath l) =
  case stripPrefix p l of
    Nothing -> throwM (Couldn'tStripPrefixDir p l)
    Just "" -> throwM (Couldn'tStripPrefixDir p l)
    Just ok -> return (FPath ok)
stripDir (DPath p) (DPath l) =
  case stripPrefix p l of
    Nothing -> throwM (Couldn'tStripPrefixDir p l)
    Just "" -> throwM (Couldn'tStripPrefixDir p l)
    Just ok -> return (DPath ok)
stripDir _ _ = throwM InvalidTypeCombination


-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
-- Returns False if the first argument is not a directory path.
isParentOf :: Path b -> Path b -> Bool
isParentOf p@(DPath _) l@(DPath _) = isJust (stripDir p l)
isParentOf p@(DPath _) l@(FPath _) = isJust (stripDir p l)
isParentOf _         _         = False

-- | Take the absolute parent directory from the absolute path.
--
-- The following properties hold:
--
-- @parent (x \<\/> y) == x@
--
-- On the root, getting the parent is idempotent:
--
-- @parent (parent \"\/\") = \"\/\"@
--
parent :: Path Abs -> Path Abs
parent (DPath fp) =
  DPath (normalizeDir (FilePath.takeDirectory (FilePath.dropTrailingPathSeparator fp)))
parent (FPath fp) =
  DPath (normalizeDir (FilePath.takeDirectory (FilePath.dropTrailingPathSeparator fp)))

-- | Extract the file/directory part of a path.
--
-- The following properties hold:
--
-- @filename (p \<\/> a) == filename a@
--
filename :: Path b -> Path Rel
filename (FPath l) =
  FPath (normalizeFile (FilePath.takeFileName l))
filename (DPath l) =
  DPath (last (FilePath.splitPath l))


--------------------------------------------------------------------------------
-- Internal functions

-- | Internal use for normalizing a directory.
normalizeDir :: FilePath -> FilePath
normalizeDir =
  clean . FilePath.addTrailingPathSeparator . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x

-- | Internal use for normalizing a fileectory.
normalizeFile :: FilePath -> FilePath
normalizeFile =
  clean . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x

