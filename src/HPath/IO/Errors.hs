-- |
-- Module      :  HPath.IO.Errors
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

module HPath.IO.Errors
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
  , doesFileExist
  , doesDirectoryExist
  , isWritable
  , canOpenDirectory

  -- * Error handling functions
  , catchErrno
  , rethrowErrnoAs
  , handleIOError
  , bracketeer
  , reactOnError
  )
  where


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception
import Control.Monad
  (
    forM
  , when
  )
import Control.Monad.IfElse
  (
    whenM
  )
import Data.ByteString
  (
    ByteString
  )
import Data.ByteString.UTF8
  (
    toString
  )
import Data.Typeable
  (
    Typeable
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
import HPath
import HPath.Internal
  (
    Path(..)
  )
import {-# SOURCE #-} HPath.IO
  (
    canonicalizePath
  , toAbs
  )
import System.IO.Error
  (
    alreadyExistsErrorType
  , catchIOError
  , ioeGetErrorType
  , mkIOError
  )

import qualified System.Posix.Directory.ByteString as PFD
import System.Posix.Files.ByteString
  (
    fileAccess
  , getFileStatus
  )
import qualified System.Posix.Files.ByteString as PF


-- |Additional generic IO exceptions that the posix functions
-- do not provide.
data HPathIOException = SameFile ByteString ByteString
                      | DestinationInSource ByteString ByteString
                      | RecursiveFailure [(RecursiveFailureHint, IOException)]
  deriving (Eq, Show, Typeable)


-- |A type for giving failure hints on recursive failure, which allows
-- to programmatically make choices without examining
-- the weakly typed I/O error attributes (like `ioeGetFileName`).
--
-- The first argument to the data constructor is always the
-- source and the second the destination.
data RecursiveFailureHint = ReadContentsFailed    ByteString ByteString
                          | CreateDirFailed       ByteString ByteString
                          | CopyFileFailed        ByteString ByteString
                          | RecreateSymlinkFailed ByteString ByteString
  deriving (Eq, Show)


instance Exception HPathIOException


toConstr :: HPathIOException -> String
toConstr SameFile {}            = "SameFile"
toConstr DestinationInSource {} = "DestinationInSource"
toConstr RecursiveFailure {}    = "RecursiveFailure"





    -----------------------------
    --[ Exception identifiers ]--
    -----------------------------


isSameFile, isDestinationInSource, isRecursiveFailure :: HPathIOException -> Bool
isSameFile ex = toConstr (ex :: HPathIOException) == toConstr SameFile{}
isDestinationInSource ex = toConstr (ex :: HPathIOException) == toConstr DestinationInSource{}
isRecursiveFailure ex = toConstr (ex :: HPathIOException) == toConstr RecursiveFailure{}


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
throwFileDoesExist :: Path b -> IO ()
throwFileDoesExist fp@(MkPath bs) =
  whenM (doesFileExist fp)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "File already exists"
                     Nothing
                   $ (Just (toString $ bs))
        )


-- |Throws `AlreadyExists` `IOError` if directory exists.
throwDirDoesExist :: Path b -> IO ()
throwDirDoesExist fp@(MkPath bs) =
  whenM (doesDirectoryExist fp)
        (ioError . mkIOError
                     alreadyExistsErrorType
                     "Directory already exists"
                     Nothing
                   $ (Just (toString $ bs))
        )


-- |Uses `isSameFile` and throws `SameFile` if it returns True.
throwSameFile :: Path b1
              -> Path b2
              -> IO ()
throwSameFile fp1@(MkPath bs1) fp2@(MkPath bs2) =
  whenM (sameFile fp1 fp2)
        (throwIO $ SameFile bs1 bs2)


-- |Check if the files are the same by examining device and file id.
-- This follows symbolic links.
sameFile :: Path b1 -> Path b2 -> IO Bool
sameFile (MkPath fp1) (MkPath fp2) =
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
throwDestinationInSource :: Path b1 -- ^ source dir
                         -> Path b2 -- ^ full destination, @dirname dest@
                                    --   must exist
                         -> IO ()
throwDestinationInSource (MkPath sbs) dest@(MkPath dbs) = do
  destAbs <- toAbs dest
  dest'   <- (\x -> maybe x (\y -> x </> y) $ basename dest)
             <$> (canonicalizePath $ dirname destAbs)
  dids <- forM (getAllParents dest') $ \p -> do
          fs <- PF.getSymbolicLinkStatus (fromAbs p)
          return (PF.deviceID fs, PF.fileID fs)
  sid <- fmap (\x -> (PF.deviceID x, PF.fileID x))
              $ PF.getFileStatus sbs
  when (elem sid dids)
       (throwIO $ DestinationInSource dbs sbs)


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
doesFileExist :: Path b -> IO Bool
doesFileExist (MkPath bs) =
  handleIOError (\_ -> return False) $ do
    fs  <- PF.getSymbolicLinkStatus bs
    return $ not . PF.isDirectory $ fs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
doesDirectoryExist :: Path b -> IO Bool
doesDirectoryExist (MkPath bs) =
  handleIOError (\_ -> return False) $ do
    fs  <- PF.getSymbolicLinkStatus bs
    return $ PF.isDirectory fs


-- |Checks whether a file or folder is writable.
isWritable :: Path b -> IO Bool
isWritable (MkPath bs) =
  handleIOError (\_ -> return False) $
    fileAccess bs False True False


-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
canOpenDirectory :: Path b -> IO Bool
canOpenDirectory (MkPath bs) =
  handleIOError (\_ -> return False) $ do
    bracket (PFD.openDirStream bs)
            PFD.closeDirStream
            (\_ -> return ())
    return True




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

