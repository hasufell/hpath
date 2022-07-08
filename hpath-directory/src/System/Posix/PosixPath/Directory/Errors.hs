-- |
-- Module      :  System.Posix.PosixPath.Directory.Errors
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides error handling.

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf          #-}

module System.Posix.PosixPath.Directory.Errors
  (
  -- * Types
    HPathIOException(..)
  , RecursiveFailureHint(..)

  -- * Path based functions
  , throwFileDoesExist
  , throwDirDoesExist
  , throwSameFile
  , sameFile
  , throwDestinationInSource

  -- * Error handling functions
  , catchErrno
  , rethrowErrnoAs
  )
  where


import Control.Exception.Safe
import Control.Monad
  (
    forM
  , when
  )
import Control.Monad.IfElse
  (
    whenM
  )
import Data.List
  (
    mapAccumL
  )
import Foreign.C.Error
  (
    getErrno
  , Errno
  )
import {-# SOURCE #-} System.Posix.PosixPath.Directory
  (
    canonicalizePath
  , toAbs
  , doesFileExist
  , doesDirectoryExist
  )
import System.IO.Error
  (
    alreadyExistsErrorType
  , mkIOError
  )
import System.Posix.Files.PosixString
  (
    getFileStatus
  )
import qualified System.Posix.Files.PosixString as PF
import System.OsPath.Posix
import qualified System.OsPath.Posix.Internal as Raw
import qualified System.OsString.Internal.Types as Raw
import qualified System.OsPath.Data.ByteString.Short as BS
import System.Directory.Types
import System.OsString.Internal.Types











    ----------------------------
    --[ Path based functions ]--
    ----------------------------


-- |Throws `AlreadyExists` `IOError` if file exists.
throwFileDoesExist :: PosixPath -> IO ()
throwFileDoesExist bs = do
  locstr <- decodeFS bs
  whenM (doesFileExist bs)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "File already exists"
                     Nothing
                   $ (Just locstr)
        )


-- |Throws `AlreadyExists` `IOError` if directory exists.
throwDirDoesExist :: PosixPath -> IO ()
throwDirDoesExist bs = do
  locstr <- decodeFS bs
  whenM (doesDirectoryExist bs)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "Directory already exists"
                     Nothing
                   $ (Just locstr)
        )


-- |Uses `isSameFile` and throws `SameFile` if it returns True.
throwSameFile :: PosixPath
              -> PosixPath
              -> IO ()
throwSameFile bs1 bs2 =
  whenM (sameFile bs1 bs2)
        (throwIO $ SameFile (OsString bs1) (OsString bs2))


-- |Check if the files are the same by examining device and file id.
-- This follows symbolic links.
sameFile :: PosixPath -> PosixPath -> IO Bool
sameFile fp1 fp2 =
  handleIOError (\_ -> return False) $ do
    fs1 <- getFileStatus fp1
    fs2 <- getFileStatus fp2

    if ((PF.deviceID fs1, PF.fileID fs1) ==
        (PF.deviceID fs2, PF.fileID fs2))
      then return True
      else return False


-- TODO: make this more robust when destination does not exist
-- |Checks whether the destination directory is contained
-- within the source directory by comparing the device+file ID of the
-- source directory with all device+file IDs of the parent directories
-- of the destination.
throwDestinationInSource :: PosixPath -- ^ source dir
                         -> PosixPath -- ^ full destination, @dirname dest@
                                        --   must exist
                         -> IO ()
throwDestinationInSource sbs dbs = do
  destAbs <- toAbs dbs
  dest'   <- (\x -> maybe x (\y -> x </> y) $ basename dbs)
             <$> (canonicalizePath $ takeDirectory destAbs)
  dids <- forM (takeAllParents dest') $ \p -> do
          fs <- PF.getSymbolicLinkStatus p
          return (PF.deviceID fs, PF.fileID fs)
  sid <- fmap (\x -> (PF.deviceID x, PF.fileID x))
              $ PF.getFileStatus sbs
  when (elem sid dids)
       (throwIO $ DestinationInSource (OsString dbs) (OsString sbs))
  where
    basename x = let b = takeBaseName x
                 in if b == mempty then Nothing else Just b

    takeAllParents :: PosixPath -> [PosixPath]
    takeAllParents p =
      let s = splitDirectories p
      in fmap Raw.PS
           . filterEmptyHead
           . snd
           . mapAccumL (\a b -> (if | BS.null a                       -> ( b
                                                                         , a
                                                                         )
                                    | BS.length a == 1
                                    , Raw.isPathSeparator (BS.head a) -> ( BS.singleton (Raw.unPW pathSeparator) <> b
                                                                         , BS.singleton (Raw.unPW pathSeparator)
                                                                         )
                                    | otherwise                       -> (a <> BS.singleton (Raw.unPW pathSeparator) <> b
                                                                         , a
                                                                         )
                                )
                       ) mempty
           . fmap Raw.unPS
           $ s
     where
      filterEmptyHead :: [BS.ShortByteString] -> [BS.ShortByteString]
      filterEmptyHead [] = []
      filterEmptyHead (a:as)
        | BS.null a = as
        | otherwise = (a:as)




    --------------------------------
    --[ Error handling functions ]--
    --------------------------------


-- |Carries out an action, then checks if there is an IOException and
-- a specific errno. If so, then it carries out another action, otherwise
-- it rethrows the error.
catchErrno :: [Errno] -- ^ errno to catch
           -> IO a    -- ^ action to try, which can raise an IOException
           -> IO a    -- ^ action to carry out in case of an IOException and
                      --   if errno matches
           -> IO a
catchErrno en a1 a2 =
  catchIOError a1 $ \e -> do
    errno <- getErrno
    if errno `elem` en
      then a2
      else ioError e


-- |Execute the given action and retrow IO exceptions as a new Exception
-- that have the given errno. If errno does not match the exception is rethrown
-- as is.
rethrowErrnoAs :: Exception e
               => [Errno]       -- ^ errno to catch
               -> e             -- ^ rethrow as if errno matches
               -> IO a          -- ^ action to try
               -> IO a
rethrowErrnoAs en fmex action = catchErrno en action (throwIO fmex)

