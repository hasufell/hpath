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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module HPath
  (
  -- * Types
   Abs
  ,Path
  ,Rel
  ,Fn
  -- * PatternSynonyms/ViewPatterns
  ,pattern Path
   -- * Parsing
  ,PathParseException
  ,parseAbs
  ,parseFn
  ,parseRel
  -- * Constructors
  ,mkAbs
  ,mkFn
  ,mkRel
  -- * Operations
  ,(</>)
  ,basename
  ,dirname
  ,isParentOf
  ,getAllParents
  ,stripDir
  -- * Conversion
  ,canonicalizePath
  ,fromAbs
  ,fromRel
  ,normalize
  ,toFilePath
  -- * Queries
  ,hasDot
  ,hasDoublePS
  ,hasParentDir
  ,isFileName
  -- * String based functions
  ,realPath
  )
  where

import           Control.Exception (Exception)
import           Control.Monad(void)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data
import           Data.List
import           Data.Maybe
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.Marshal.Alloc(allocaBytes)
import           Language.Haskell.TH
import           HPath.Foreign
import           HPath.Internal
import qualified System.FilePath as FilePath

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
  = InvalidAbs FilePath
  | InvalidRel FilePath
  | InvalidFn FilePath
  | Couldn'tStripPrefixTPS FilePath FilePath
  deriving (Show,Typeable)
instance Exception PathParseException

instance RelC Rel
instance RelC Fn

--------------------------------------------------------------------------------
-- PatternSynonyms

pattern Path x <- (MkPath x)

--------------------------------------------------------------------------------
-- Parsers

-- | Get a location for an absolute path.
--
-- Throws: 'PathParseException'
--
parseAbs :: MonadThrow m
         => FilePath -> m (Path Abs)
parseAbs filepath =
  if FilePath.isAbsolute filepath &&
     not (null filepath) &&
     FilePath.isValid filepath
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
         => FilePath -> m (Path Rel)
parseRel filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     FilePath.isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidRel filepath)

parseFn :: MonadThrow m
        => FilePath -> m (Path Fn)
parseFn filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     isFileName filepath &&
     FilePath.isValid filepath
     then return (MkPath filepath)
     else throwM (InvalidFn filepath)

--------------------------------------------------------------------------------
-- Constructors

-- | Make a 'Path Abs TPS'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbs :: FilePath -> Q Exp
mkAbs s =
  case parseAbs s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Abs|]

-- | Make a 'Path Rel TPS'.
mkRel :: FilePath -> Q Exp
mkRel s =
  case parseRel s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Rel|]

-- | Make a 'Path Rel TPS'.
mkFn :: FilePath -> Q Exp
mkFn s =
  case parseFn s of
    Left err -> error (show err)
    Right (MkPath str) ->
      [|MkPath $(return (LitE (StringL str))) :: Path Fn|]

--------------------------------------------------------------------------------
-- Conversion

-- | Convert to a 'FilePath' type.
--
-- All TPS data types have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b -> FilePath
toFilePath (MkPath l) = l

fromAbs :: Path Abs -> FilePath
fromAbs = toFilePath

fromRel :: RelC r => Path r -> FilePath
fromRel = toFilePath

normalize :: Path t -> Path t
normalize (MkPath l) = MkPath $ FilePath.normalise l

-- | May fail on `realPath`.
canonicalizePath :: Path Abs -> IO (Path Abs)
canonicalizePath (MkPath l) = do
  nl <- realPath l
  return $ MkPath nl

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
(</>) :: RelC r => Path b -> Path r -> Path b
(</>) (MkPath a) (MkPath b) = MkPath (a' ++ b)
  where
    a' = FilePath.addTrailingPathSeparator a

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
    p' = FilePath.addTrailingPathSeparator p

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
    np = MkPath . FilePath.dropTrailingPathSeparator . FilePath.normalise $ p


-- | Extract the directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname p@
--
dirname :: Path Abs -> Path Abs
dirname (MkPath fp) = MkPath (FilePath.takeDirectory $ FilePath.dropTrailingPathSeparator fp)

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
  | not (FilePath.isAbsolute rl) = MkPath rl
  | otherwise                    = MkPath "."
  where
    rl = last . FilePath.splitPath . FilePath.dropTrailingPathSeparator $ l


--------------------------------------------------------------------------------
-- Query functions

-- | Helper function: check if the filepath has any parent directories in it.
hasParentDir :: FilePath -> Bool
hasParentDir filepath =
     ("/.." `isSuffixOf` filepath) ||
     ("/../" `isInfixOf` filepath) ||
     ("../" `isPrefixOf` filepath)

hasDot :: FilePath -> Bool
hasDot filepath =
     ("/." `isSuffixOf` filepath) ||
     ("/./" `isInfixOf` filepath) ||
     ("./" `isPrefixOf` filepath)

hasDoublePS :: FilePath -> Bool
hasDoublePS filepath =
     ("//" `isInfixOf` filepath)

isFileName :: FilePath -> Bool
isFileName filepath =
     not ("/" `isInfixOf` filepath)


--------------------------------------------------------------------------------
-- String based path functions

foreign import ccall "realpath"
  c_realpath :: CString -> CString -> IO CString

-- | return the canonicalized absolute pathname
--
-- like canonicalizePath, but uses realpath(3)
realPath :: String -> IO String
realPath inp = do
    allocaBytes pathMax $ \tmp -> do
        void $ withCString inp
             $ \cstr -> throwErrnoIfNull "realpath"
             $ c_realpath cstr tmp
        peekCAString tmp

