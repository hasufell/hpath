-- |
-- Module      :  System.Posix.FilePath
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- The equivalent of "System.FilePath" on raw (byte string) file paths.
--
-- Not all functions of "System.FilePath" are implemented yet. Feel free to contribute!


{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}


module System.Posix.FilePath (

  -- * Separator predicates
  pathSeparator
, isPathSeparator
, searchPathSeparator
, isSearchPathSeparator
, extSeparator
, isExtSeparator

  -- * $PATH methods
, splitSearchPath
, getSearchPath

  -- * Extension functions
, splitExtension
, takeExtension
, replaceExtension
, dropExtension
, addExtension
, hasExtension
, (<.>)
, splitExtensions
, dropExtensions
, takeExtensions
, stripExtension

  -- * Filename\/directory functions
, splitFileName
, takeFileName
, replaceFileName
, dropFileName
, takeBaseName
, replaceBaseName
, takeDirectory
, replaceDirectory
, combine
, (</>)
, splitPath
, joinPath
, splitDirectories

  -- * Trailing slash functions
, hasTrailingPathSeparator
, addTrailingPathSeparator
, dropTrailingPathSeparator

  -- * File name manipulations
, normalise
, makeRelative
, equalFilePath
, isRelative
, isAbsolute
, isValid
, makeValid
, isFileName
, hasParentDir
, hiddenFile

, module System.Posix.ByteString.FilePath
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String (fromString)
import           System.Posix.ByteString.FilePath
import qualified System.Posix.Env.ByteString as PE

import           Data.Maybe (isJust)
import           Data.Word8
#if !MIN_VERSION_bytestring(0,10,8)
import qualified Data.List as L
#endif
import           Control.Arrow (second)

-- $setup
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import qualified Data.ByteString as BS
-- >>> instance Arbitrary ByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ByteString where coarbitrary = coarbitrary . BS.unpack
--
-- >>> let _chr :: Word8 -> Char; _chr = chr . fromIntegral



------------------------
-- Separator predicates


-- | Path separator character
pathSeparator :: Word8
pathSeparator = _slash


-- | Check if a character is the path separator
--
-- prop> \n ->  (_chr n == '/') == isPathSeparator n
isPathSeparator :: Word8 -> Bool
isPathSeparator = (== pathSeparator)


-- | Search path separator
searchPathSeparator :: Word8
searchPathSeparator = _colon


-- | Check if a character is the search path separator
--
-- prop> \n -> (_chr n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: Word8 -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension separator
extSeparator :: Word8
extSeparator = _period


-- | Check if a character is the file extension separator
--
-- prop> \n -> (_chr n == '.') == isExtSeparator n
isExtSeparator :: Word8 -> Bool
isExtSeparator = (== extSeparator)



------------------------
-- $PATH methods


-- | Take a ByteString, split it on the 'searchPathSeparator'.
-- Blank items are converted to @.@.
--
-- Follows the recommendations in
-- <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- >>> splitSearchPath "File1:File2:File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1::File2:File3"
-- ["File1",".","File2","File3"]
-- >>> splitSearchPath ""
-- ["."]
splitSearchPath :: ByteString -> [RawFilePath]
splitSearchPath = f
  where
    f bs = let (pre, post) = BS.break isSearchPathSeparator bs
           in if BS.null post
                 then g pre
                 else g pre ++ f (BS.tail post)
    g x
      | BS.null x = [BS.singleton _period]
      | otherwise = [x]


-- | Get a list of 'RawFilePath's in the $PATH variable.
getSearchPath :: IO [RawFilePath]
getSearchPath = fmap (maybe [] splitSearchPath) (PE.getEnv $ fromString "PATH")



------------------------
-- Extension functions

-- | Split a 'RawFilePath' into a path+filename and extension
--
-- >>> splitExtension "file.exe"
-- ("file",".exe")
-- >>> splitExtension "file"
-- ("file","")
-- >>> splitExtension "/path/file.tar.gz"
-- ("/path/file.tar",".gz")
--
-- prop> \path -> uncurry (BS.append) (splitExtension path) == path
splitExtension :: RawFilePath -> (RawFilePath, ByteString)
splitExtension x = if BS.null basename
    then (x,BS.empty)
    else (BS.append path (BS.init basename),BS.cons extSeparator fileExt)
  where
    (path,file) = splitFileNameRaw x
    (basename,fileExt) = BS.breakEnd isExtSeparator file


-- | Get the final extension from a 'RawFilePath'
--
-- >>> takeExtension "file.exe"
-- ".exe"
-- >>> takeExtension "file"
-- ""
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: RawFilePath -> ByteString
takeExtension = snd . splitExtension


-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: RawFilePath -> ByteString -> RawFilePath
replaceExtension path ext = dropExtension path <.> ext


-- | Drop the final extension from a 'RawFilePath'
--
-- >>> dropExtension "file.exe"
-- "file"
-- >>> dropExtension "file"
-- "file"
-- >>> dropExtension "/path/file.tar.gz"
-- "/path/file.tar"
dropExtension :: RawFilePath -> RawFilePath
dropExtension = fst . splitExtension


-- | Add an extension to a 'RawFilePath'
--
-- >>> addExtension "file" ".exe"
-- "file.exe"
-- >>> addExtension "file.tar" ".gz"
-- "file.tar.gz"
-- >>> addExtension "/path/" ".ext"
-- "/path/.ext"
addExtension :: RawFilePath -> ByteString -> RawFilePath
addExtension file ext
    | BS.null ext = file
    | isExtSeparator (BS.head ext) = BS.append file ext
    | otherwise = BS.intercalate (BS.singleton extSeparator) [file, ext]


-- | Check if a 'RawFilePath' has an extension
--
-- >>> hasExtension "file"
-- False
-- >>> hasExtension "file.tar"
-- True
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: RawFilePath -> Bool
hasExtension = isJust . BS.elemIndex extSeparator . takeFileName


-- | Operator version of 'addExtension'
(<.>) :: RawFilePath -> ByteString -> RawFilePath
(<.>) = addExtension


-- | Split a 'RawFilePath' on the first extension.
--
-- >>> splitExtensions "/path/file.tar.gz"
-- ("/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: RawFilePath -> (RawFilePath, ByteString)
splitExtensions x = if BS.null basename
    then (path,fileExt)
    else (BS.append path basename,fileExt)
  where
    (path,file) = splitFileNameRaw x
    (basename,fileExt) = BS.break isExtSeparator file


-- | Remove all extensions from a 'RawFilePath'
--
-- >>> dropExtensions "/path/file.tar.gz"
-- "/path/file"
dropExtensions :: RawFilePath -> RawFilePath
dropExtensions = fst . splitExtensions


-- | Take all extensions from a 'RawFilePath'
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: RawFilePath -> ByteString
takeExtensions = snd . splitExtensions


-- | Drop the given extension from a FilePath, and the @\".\"@ preceding it.
-- Returns 'Nothing' if the FilePath does not have the given extension, or
-- 'Just' and the part before the extension if it does.
--
-- This function can be more predictable than 'dropExtensions',
-- especially if the filename might itself contain @.@ characters.
--
-- >>> stripExtension "hs.o" "foo.x.hs.o"
-- Just "foo.x"
-- >>> stripExtension "hi.o" "foo.x.hs.o"
-- Nothing
-- >>> stripExtension ".c.d" "a.b.c.d"
-- Just "a.b"
-- >>> stripExtension ".c.d" "a.b..c.d"
-- Just "a.b."
-- >>> stripExtension "baz"  "foo.bar"
-- Nothing
-- >>> stripExtension "bar"  "foobar"
-- Nothing
--
-- prop> \path -> stripExtension "" path == Just path
-- prop> \path -> dropExtension path  == fromJust (stripExtension (takeExtension path) path)
-- prop> \path -> dropExtensions path == fromJust (stripExtension (takeExtensions path) path)
stripExtension :: ByteString -> RawFilePath -> Maybe RawFilePath
stripExtension bs path
  | BS.null bs = Just path
  | otherwise  = stripSuffix' dotExt path
  where
    dotExt = if isExtSeparator $ BS.head bs
                then bs
                else extSeparator `BS.cons` bs
#if MIN_VERSION_bytestring(0,10,8)
    stripSuffix' = BS.stripSuffix
#else
    stripSuffix' xs ys = fmap (BS.pack . reverse) $ L.stripPrefix (reverse $ BS.unpack xs) (reverse $ BS.unpack ys)
#endif


------------------------
-- Filename/directory functions


-- | Split a 'RawFilePath' into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- ("path/","file.txt")
-- >>> splitFileName "path/"
-- ("path/","")
-- >>> splitFileName "file.txt"
-- ("./","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
splitFileName :: RawFilePath -> (RawFilePath, RawFilePath)
splitFileName x = if BS.null path
    then (dotSlash, file)
    else (path,file)
  where
    (path,file) = splitFileNameRaw x
    dotSlash = _period `BS.cons` (BS.singleton pathSeparator)


-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- "file.txt"
-- >>> takeFileName "path/"
-- ""
takeFileName :: RawFilePath -> RawFilePath
takeFileName = snd . splitFileName


-- | Change the file name
--
-- prop> \path -> replaceFileName path (takeFileName path) == path
replaceFileName :: RawFilePath -> ByteString -> RawFilePath
replaceFileName x y = fst (splitFileNameRaw x) </> y


-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- "path/"
-- >>> dropFileName "file.txt"
-- "./"
dropFileName :: RawFilePath -> RawFilePath
dropFileName = fst . splitFileName


-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- "file.tar"
-- >>> takeBaseName ""
-- ""
takeBaseName :: RawFilePath -> ByteString
takeBaseName = dropExtension . takeFileName


-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- "path/bob.gz"
--
-- prop> \path -> replaceBaseName path (takeBaseName path) == path
replaceBaseName :: RawFilePath -> ByteString -> RawFilePath
replaceBaseName path name = combineRaw dir (name <.> ext)
  where
    (dir,file) = splitFileNameRaw path
    ext = takeExtension file


-- | Get the directory, moving up one level if it's already a directory
--
-- >>> takeDirectory "path/file.txt"
-- "path"
-- >>> takeDirectory "file"
-- "."
-- >>> takeDirectory "/path/to/"
-- "/path/to"
-- >>> takeDirectory "/path/to"
-- "/path"
takeDirectory :: RawFilePath -> RawFilePath
takeDirectory x = case () of
    () | x == BS.singleton pathSeparator -> x
       | BS.null res && not (BS.null file) -> file
       | otherwise -> res
  where
    res = fst $ BS.spanEnd isPathSeparator file
    file = dropFileName x


-- | Change the directory component of a 'RawFilePath'
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: RawFilePath -> ByteString -> RawFilePath
replaceDirectory file dir = combineRaw dir (takeFileName file)


-- | Join two paths together
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
combine :: RawFilePath -> RawFilePath -> RawFilePath
combine a b | not (BS.null b) && isPathSeparator (BS.head b) = b
            | otherwise = combineRaw a b


-- | Operator version of combine
(</>) :: RawFilePath -> RawFilePath -> RawFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- ["/","path/","to/","file.txt"]
--
-- prop> \path -> BS.concat (splitPath path) == path
splitPath :: RawFilePath -> [RawFilePath]
splitPath = splitter
  where
    splitter x
      | BS.null x = []
      | otherwise = case BS.elemIndex pathSeparator x of
            Nothing -> [x]
            Just ix -> case BS.findIndex (not . isPathSeparator) $ BS.drop (ix+1) x of
                          Nothing -> [x]
                          Just runlen -> uncurry (:) . second splitter $ BS.splitAt (ix+1+runlen) x


-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path/to/file.txt"
joinPath :: [RawFilePath] -> RawFilePath
joinPath = foldr (</>) BS.empty


-- | Like 'splitPath', but without trailing slashes
--
-- >>> splitDirectories "/path/to/file.txt"
-- ["/","path","to","file.txt"]
-- >>> splitDirectories ""
-- []
splitDirectories :: RawFilePath -> [RawFilePath]
splitDirectories x
    | BS.null x = []
    | isPathSeparator (BS.head x) = let (root,rest) = BS.splitAt 1 x
                                    in root : splitter rest
    | otherwise = splitter x
  where
    splitter = filter (not . BS.null) . BS.split pathSeparator



------------------------
-- Trailing slash functions

-- | Check if the last character of a 'RawFilePath' is '/'.
--
-- >>> hasTrailingPathSeparator "/path/"
-- True
-- >>> hasTrailingPathSeparator "/"
-- True
-- >>> hasTrailingPathSeparator "/path"
-- False
hasTrailingPathSeparator :: RawFilePath -> Bool
hasTrailingPathSeparator x
  | BS.null x = False
  | otherwise = isPathSeparator $ BS.last x


-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
addTrailingPathSeparator :: RawFilePath -> RawFilePath
addTrailingPathSeparator x = if hasTrailingPathSeparator x
    then x
    else x `BS.snoc` pathSeparator


-- | Remove a trailing path separator
--
-- >>> dropTrailingPathSeparator "/path/"
-- "/path"
-- >>> dropTrailingPathSeparator "/path////"
-- "/path"
-- >>> dropTrailingPathSeparator "/"
-- "/"
-- >>> dropTrailingPathSeparator "//"
-- "/"
dropTrailingPathSeparator :: RawFilePath -> RawFilePath
dropTrailingPathSeparator x
  | x == BS.singleton pathSeparator = x
  | otherwise = if hasTrailingPathSeparator x
                  then dropTrailingPathSeparator $ BS.init x
                  else x



------------------------
-- File name manipulations


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
normalise :: RawFilePath -> RawFilePath
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
      not (hasTrailingPathSeparator result)
    isDirPath xs = hasTrailingPathSeparator xs
        || not (BS.null xs) && BS.last xs == _period
           && hasTrailingPathSeparator (BS.init xs)
    f = joinPath . dropDots . propSep . splitDirectories
    propSep :: [ByteString] -> [ByteString]
    propSep (x:xs)
      | BS.all (== pathSeparator) x = BS.singleton pathSeparator : xs
      | otherwise                   = x : xs
    propSep [] = []
    dropDots :: [ByteString] -> [ByteString]
    dropDots = filter (BS.singleton _period /=)



-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- >>> makeRelative "/directory" "/directory/file.ext"
-- "file.ext"
-- >>> makeRelative "/Home" "/home/bob"
-- "/home/bob"
-- >>> makeRelative "/home/" "/home/bob/foo/bar"
-- "bob/foo/bar"
-- >>> makeRelative "/fred" "bob"
-- "bob"
-- >>> makeRelative "/file/test" "/file/test/fred"
-- "fred"
-- >>> makeRelative "/file/test" "/file/test/fred/"
-- "fred/"
-- >>> makeRelative "some/path" "some/path/a/b/c"
-- "a/b/c"
--
-- prop> \p -> makeRelative p p == "."
-- prop> \p -> makeRelative (takeDirectory p) p `equalFilePath` takeFileName p
-- prop \x y -> equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
makeRelative :: RawFilePath -> RawFilePath -> RawFilePath
makeRelative root path
  | equalFilePath root path = BS.singleton _period
  | takeAbs root /= takeAbs path = path
  | otherwise = f (dropAbs root) (dropAbs path)
  where
    f x y
      | BS.null x = BS.dropWhile isPathSeparator y
      | otherwise = let (x1,x2) = g x
                        (y1,y2) = g y
                    in if equalFilePath x1 y1 then f x2 y2 else path
    g x = (BS.dropWhile isPathSeparator a, BS.dropWhile isPathSeparator b)
      where (a, b) = BS.break isPathSeparator $ BS.dropWhile isPathSeparator x
    dropAbs x = snd $ BS.span (== _slash) x
    takeAbs x = fst $ BS.span (== _slash) x


-- |Equality of two filepaths. The filepaths are normalised
-- and trailing path separators are dropped.
--
-- >>> equalFilePath "foo" "foo"
-- True
-- >>> equalFilePath "foo" "foo/"
-- True
-- >>> equalFilePath "foo" "./foo"
-- True
-- >>> equalFilePath "" ""
-- True
-- >>> equalFilePath "foo" "/foo"
-- False
-- >>> equalFilePath "foo" "FOO"
-- False
-- >>> equalFilePath "foo" "../foo"
-- False
--
-- prop> \p -> equalFilePath p p
equalFilePath :: RawFilePath -> RawFilePath -> Bool
equalFilePath p1 p2 = f p1 == f p2
  where
    f x = dropTrailingPathSeparator $ normalise x


-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: RawFilePath -> Bool
isRelative = not . isAbsolute


-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
isAbsolute :: RawFilePath -> Bool
isAbsolute x
    | BS.length x > 0 = isPathSeparator (BS.head x)
    | otherwise = False


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


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- >>> makeValid ""
-- "_"
-- >>> makeValid "file\0name"
-- "file_name"
--
-- prop> \p -> if isValid p then makeValid p == p else makeValid p /= p
-- prop> \p -> isValid (makeValid p)
makeValid :: RawFilePath -> RawFilePath
makeValid path
  | BS.null path = BS.singleton _underscore
  | otherwise    = BS.map (\x -> if x == _nul then _underscore else x) path


-- | Is the given path a valid filename? This includes
-- "." and "..".
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
isFileName :: RawFilePath -> Bool
isFileName filepath =
  not (BS.singleton pathSeparator `BS.isInfixOf` filepath) &&
  not (BS.null filepath) &&
  not (_nul `BS.elem` filepath)


-- | Check if the filepath has any parent directories in it.
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
hasParentDir :: RawFilePath -> Bool
hasParentDir filepath =
    (pathSeparator `BS.cons` pathDoubleDot)
     `BS.isSuffixOf` filepath
   ||
    (BS.singleton pathSeparator
        `BS.append` pathDoubleDot
        `BS.append` BS.singleton pathSeparator)
     `BS.isInfixOf`  filepath
   ||
    (pathDoubleDot `BS.append` BS.singleton pathSeparator)
      `BS.isPrefixOf` filepath
  where
    pathDoubleDot = BS.pack [_period, _period]


-- | Whether the file is a hidden file.
--
-- >>> hiddenFile ".foo"
-- True
-- >>> hiddenFile "..foo.bar"
-- True
-- >>> hiddenFile "some/path/.bar"
-- True
-- >>> hiddenFile "..."
-- True
-- >>> hiddenFile "dod.bar"
-- False
-- >>> hiddenFile "."
-- False
-- >>> hiddenFile ".."
-- False
-- >>> hiddenFile ""
-- False
hiddenFile :: RawFilePath -> Bool
hiddenFile fp
  | fn == BS.pack [_period, _period] = False
  | fn == BS.pack [_period]          = False
  | otherwise                        = BS.pack [extSeparator]
                                         `BS.isPrefixOf` fn
  where
    fn = takeFileName fp



------------------------
-- internal stuff

-- Just split the input FileName without adding/normalizing or changing
-- anything.
splitFileNameRaw :: RawFilePath -> (RawFilePath, RawFilePath)
splitFileNameRaw = BS.breakEnd isPathSeparator

-- | Combine two paths, assuming rhs is NOT absolute.
combineRaw :: RawFilePath -> RawFilePath -> RawFilePath
combineRaw a b | BS.null a = b
                  | BS.null b = a
                  | isPathSeparator (BS.last a) = BS.append a b
                  | otherwise = BS.intercalate (BS.singleton pathSeparator) [a, b]

