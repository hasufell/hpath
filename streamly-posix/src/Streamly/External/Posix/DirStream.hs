{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

-- |
-- Module      :  Streamly.External.Posix.DirStream
-- Copyright   :  © 2020 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides high-level file streaming API,
-- working with directory streams (POSIX).
module Streamly.External.Posix.DirStream
  (
  -- * Directory listing
    unfoldDirContents
  , dirContentsStream
  , dirContents
  )
where

import           Control.Exception.Safe
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Prelude                 hiding ( readFile )
import           System.Posix.ByteString
import           System.Posix.Directory.ByteString
                                               as PosixBS
import           System.Posix.Foreign           ( DirType )
import           System.Posix.PosixFilePath.Directory.Traversals
                                         hiding ( getDirectoryContents )
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
#if MIN_VERSION_streamly(0,7,1)
import qualified Streamly.Internal.Data.Unfold as SIU
#endif
#if MIN_VERSION_streamly(0,8,0)
import           Streamly.Prelude
import           Streamly.Internal.Data.Unfold.Type
#else
import           Streamly
import           Streamly.Internal.Data.Unfold.Types
import qualified Streamly.Internal.Prelude     as S
#endif

import System.AbstractFilePath.Posix


-- | Create an 'Unfold' of directory contents.
unfoldDirContents :: MonadIO m => Unfold m DirStream (DirType, PosixFilePath)
unfoldDirContents = Unfold step return
 where
  {-# INLINE [0] step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return $ if
      | e == mempty                                            -> D.Stop
      | [unsafeFromChar '.'] == unpackPlatformString e               -> D.Skip dirstream
      | [unsafeFromChar '.', unsafeFromChar '.'] == unpackPlatformString e -> D.Skip dirstream
      | otherwise                                              -> D.Yield (typ, e) dirstream


-- | Read the directory contents as a stream.
--
-- The DirStream is closed automatically, when the streamly stream exits
-- normally, aborts or gets garbage collected.
-- The stream must not be used after the dirstream is closed.
dirContentsStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                  => DirStream
                  -> SerialT m (DirType, PosixFilePath)
dirContentsStream ds =
#if MIN_VERSION_streamly(0,8,0)
  unfold (SIU.finally (liftIO . PosixBS.closeDirStream) unfoldDirContents) $ ds
#else
#if MIN_VERSION_streamly(0,7,1)
  S.unfold (SIU.finallyIO (liftIO . PosixBS.closeDirStream) unfoldDirContents) $ ds
#else
  S.finally (liftIO . PosixBS.closeDirStream $ ds) . S.unfold unfoldDirContents $ ds
#endif
#endif


-- | Read the directory contents strictly as a list.
--
-- The DirStream is closed automatically.
dirContents :: (MonadCatch m, MonadAsync m, MonadMask m)
            => DirStream
            -> m [(DirType, PosixFilePath)]
#if MIN_VERSION_streamly(0,8,0)
dirContents = toList . dirContentsStream
#else
dirContents = S.toList . dirContentsStream
#endif

