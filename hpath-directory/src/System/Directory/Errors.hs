-- |
-- Module      :  System.Directory.Errors
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

module System.Directory.Errors
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

  -- * Error handling functions
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
import System.IO.Error
  (
    alreadyExistsErrorType
  , ioeGetErrorType
  , mkIOError
  )
import System.Directory.Types




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





    --------------------------------
    --[ Error handling functions ]--
    --------------------------------




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


