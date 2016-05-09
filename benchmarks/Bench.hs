{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}
import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath ((</>))
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString as PosixBS
import System.Posix.Directory.Traversals
import qualified System.Posix.FilePath as PosixBS
import System.Posix.Files.ByteString

import Control.Exception
import qualified Data.ByteString.Char8 as BS

import System.Environment (getArgs, withArgs)
import System.IO.Error
import System.IO.Unsafe
import System.Process (system)
import Criterion.Main


-- | Based on code from 'Real World Haskell', at
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html#id620419
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive topdir = do
    names <- System.Directory.getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then listFilesRecursive path
            else return [path]
    return (topdir : concat paths)

----------------------------------------------------------

getDirectoryContentsBS :: RawFilePath -> IO [RawFilePath]
getDirectoryContentsBS path = 
  modifyIOError ((`ioeSetFileName` (BS.unpack path)) .
                 (`ioeSetLocation` "getDirectoryContentsBS")) $ do
    bracket
      (PosixBS.openDirStream path)
      PosixBS.closeDirStream
      loop
 where
  loop dirp = do
     e <- PosixBS.readDirStream dirp
     if BS.null e then return [] else do
       es <- loop dirp
       return (e:es)


-- | similar to 'listFilesRecursive, but uses RawFilePaths
listFilesRecursiveBS :: RawFilePath -> IO [RawFilePath]
listFilesRecursiveBS topdir = do
    names <- getDirectoryContentsBS topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> unsafeInterleaveIO $ do
        let path = PosixBS.combine topdir name
        isDir <- isDirectory <$> getFileStatus path
        if isDir
            then listFilesRecursiveBS path
            else return [path]
    return (topdir : concat paths)
----------------------------------------------------------


benchTraverse :: RawFilePath -> IO ()
benchTraverse = traverseDirectory (\() p -> BS.putStrLn p) ()

main :: IO ()
main = do
  args <- getArgs
  let (d,otherArgs) = case args of
          []   -> ("/usr/local",[])
          x:xs -> (x,xs)
  withArgs otherArgs $ defaultMain
    [ bench "traverse (FilePath)"      $ nfIO $ listFilesRecursive d >>= mapM_ putStrLn
    , bench "traverse (RawFilePath)"   $ nfIO $ listFilesRecursiveBS (BS.pack d) >>= mapM_ BS.putStrLn
    , bench "allDirectoryContents"     $ nfIO $ allDirectoryContents (BS.pack d) >>= mapM_ BS.putStrLn
    , bench "allDirectoryContents'"    $ nfIO $ allDirectoryContents' (BS.pack d) >>= mapM_ BS.putStrLn
    , bench "traverseDirectory"        $ nfIO $ benchTraverse (BS.pack d)
    , bench "unix find"                $ nfIO $ void $ system ("find " ++ d)
    ]
