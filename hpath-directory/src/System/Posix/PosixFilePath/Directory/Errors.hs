-- |
-- Module      :  System.Posix.PosixFilePath.Directory.Errors
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides error handling.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Posix.PosixFilePath.Directory.Errors
  (
  -- * Types
    HPathIOException(..)
  , RecursiveFailureHint(..)

  -- * Exception identifiers
  , isSameFile
  , isDestinationInSource
  , isRecursiveFailure
  , isReadContentsFailed
  , isCreateDirFailed
  , isCopyFileFailed
  , isRecreateSymlinkFailed

  -- * Path based functions
  , throwFileDoesExist
  , throwDirDoesExist
  , throwSameFile
  , sameFile
  , throwDestinationInSource

  -- * Error handling functions
  , catchErrno
  , rethrowErrnoAs
  , handleIOError
  , hideError
  , bracketeer
  , reactOnError
  )
  where


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception.Safe hiding (handleIOError)
import Control.Monad
  (
    forM
  , when
  )
import Control.Monad.IfElse
  (
    whenM
  )
import Foreign.C.Error
  (
    getErrno
  , Errno
  )
import GHC.IO.Exception
  (
    IOErrorType
  )
import {-# SOURCE #-} System.Posix.PosixFilePath.Directory
  (
    canonicalizePath
  , toAbs
  , doesFileExist
  , doesDirectoryExist
  )
import System.IO.Error
  (
    alreadyExistsErrorType
  , ioeGetErrorType
  , mkIOError
  )
import System.Posix.Files.PosixString
  (
    getFileStatus
  )
import qualified System.Posix.Files.PosixString as PF
import AFP.AbstractFilePath.Posix
import System.Directory.Types
import AFP.OsString.Internal.Types






toConstr :: HPathIOException -> String
toConstr SameFile {}            = "SameFile"
toConstr DestinationInSource {} = "DestinationInSource"
toConstr RecursiveFailure {}    = "RecursiveFailure"





    -----------------------------
    --[ Exception identifiers ]--
    -----------------------------


isSameFile, isDestinationInSource, isRecursiveFailure :: HPathIOException -> Bool
isSameFile ex = toConstr (ex :: HPathIOException) == toConstr (SameFile mempty mempty)
isDestinationInSource ex = toConstr (ex :: HPathIOException) == (toConstr $ DestinationInSource mempty mempty)
isRecursiveFailure ex = toConstr (ex :: HPathIOException) == (toConstr $ RecursiveFailure mempty)


isReadContentsFailed, isCreateDirFailed, isCopyFileFailed, isRecreateSymlinkFailed ::RecursiveFailureHint -> Bool
isReadContentsFailed ReadContentsFailed{} = True
isReadContentsFailed _ = False
isCreateDirFailed CreateDirFailed{} = True
isCreateDirFailed _ = False
isCopyFileFailed CopyFileFailed{} = True
isCopyFileFailed _ = False
isRecreateSymlinkFailed RecreateSymlinkFailed{} = True
isRecreateSymlinkFailed _ = False





    ----------------------------
    --[ Path based functions ]--
    ----------------------------


-- |Throws `AlreadyExists` `IOError` if file exists.
throwFileDoesExist :: PosixFilePath -> IO ()
throwFileDoesExist bs = do
  locstr <- fromPlatformStringIO bs
  whenM (doesFileExist bs)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "File already exists"
                     Nothing
                   $ (Just locstr)
        )


-- |Throws `AlreadyExists` `IOError` if directory exists.
throwDirDoesExist :: PosixFilePath -> IO ()
throwDirDoesExist bs = do
  locstr <- fromPlatformStringIO bs
  whenM (doesDirectoryExist bs)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "Directory already exists"
                     Nothing
                   $ (Just locstr)
        )


-- |Uses `isSameFile` and throws `SameFile` if it returns True.
throwSameFile :: PosixFilePath
              -> PosixFilePath
              -> IO ()
throwSameFile bs1 bs2 =
  whenM (sameFile bs1 bs2)
        (throwIO $ SameFile (OsString bs1) (OsString bs2))


-- |Check if the files are the same by examining device and file id.
-- This follows symbolic links.
sameFile :: PosixFilePath -> PosixFilePath -> IO Bool
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
throwDestinationInSource :: PosixFilePath -- ^ source dir
                         -> PosixFilePath -- ^ full destination, @dirname dest@
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



-- |Like `catchIOError`, with arguments swapped.
handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError


hideError :: IOErrorType -> IO () -> IO ()
hideError err = handleIO (\e -> if err == ioeGetErrorType e then pure () else ioError e)


-- |Like `bracket`, but allows to have different clean-up
-- actions depending on whether the in-between computation
-- has raised an exception or not.
bracketeer :: IO a        -- ^ computation to run first
           -> (a -> IO b) -- ^ computation to run last, when
                          --   no exception was raised
           -> (a -> IO b) -- ^ computation to run last,
                          --   when an exception was raised
           -> (a -> IO c) -- ^ computation to run in-between
           -> IO c
bracketeer before after afterEx thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` afterEx a
    _ <- after a
    return r


reactOnError :: IO a
             -> [(IOErrorType, IO a)]      -- ^ reaction on IO errors
             -> [(HPathIOException, IO a)] -- ^ reaction on HPathIOException
             -> IO a
reactOnError a ios fmios =
  a `catches` [iohandler, fmiohandler]
  where
    iohandler = Handler $
      \(ex :: IOException) ->
         foldr (\(t, a') y -> if ioeGetErrorType ex == t
                                then a'
                                else y)
               (throwIO ex)
               ios
    fmiohandler = Handler $
      \(ex :: HPathIOException) ->
         foldr (\(t, a') y -> if toConstr ex == toConstr t
                                then a'
                                else y)
               (throwIO ex)
               fmios


