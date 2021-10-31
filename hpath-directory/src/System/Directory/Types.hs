module System.Directory.Types where

import Control.Exception (Exception, IOException)
import Data.Typeable (Typeable)
import AFP.AbstractFilePath.Types





    -------------
    --[ Types ]--
    -------------

-- |Additional generic IO exceptions that the posix functions
-- do not provide.
data HPathIOException = SameFile AbstractFilePath AbstractFilePath
                      | DestinationInSource AbstractFilePath AbstractFilePath
                      | RecursiveFailure [(RecursiveFailureHint, IOException)]
  deriving (Eq, Show, Typeable)


-- |A type for giving failure hints on recursive failure, which allows
-- to programmatically make choices without examining
-- the weakly typed I/O error attributes (like `ioeGetFileName`).
--
-- The first argument to the data constructor is always the
-- source and the second the destination.
data RecursiveFailureHint = ReadContentsFailed    AbstractFilePath AbstractFilePath
                          | CreateDirFailed       AbstractFilePath AbstractFilePath
                          | CopyFileFailed        AbstractFilePath AbstractFilePath
                          | RecreateSymlinkFailed AbstractFilePath AbstractFilePath
  deriving (Eq, Show)



instance Exception HPathIOException



-- |The error mode for recursive operations.
--
-- On `FailEarly` the whole operation fails immediately if any of the
-- recursive sub-operations fail, which is sort of the default
-- for IO operations.
--
-- On `CollectFailures` skips errors in the recursion and keeps on recursing.
-- However all errors are collected in the `RecursiveFailure` error type,
-- which is raised finally if there was any error. Also note that
-- `RecursiveFailure` does not give any guarantees on the ordering
-- of the collected exceptions.
data RecursiveErrorMode = FailEarly
                        | CollectFailures
  deriving (Eq, Show)


-- |The mode for copy and file moves.
-- Overwrite mode is usually not very well defined, but is a convenience
-- shortcut.
data CopyMode = Strict    -- ^ fail if any target exists
              | Overwrite -- ^ overwrite targets
  deriving (Eq, Show)

