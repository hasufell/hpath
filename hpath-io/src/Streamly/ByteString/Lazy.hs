{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.ByteString.Lazy where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, foldlChunks, fromChunks)
import Data.ByteString.Unsafe
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Prelude hiding (length)
import Streamly
import Streamly.ByteString (arrayToByteString, byteStringToArray)
import Streamly.Memory.Array
import qualified Streamly.Prelude as S

toByteString ::
     forall m. (MonadIO m, MonadAsync m)
  => SerialT m (Array Word8)
  -> m ByteString
toByteString stream = do
  ys :: [BS.ByteString] <- S.toList $ S.mapM arrayToByteString stream
  pure $ fromChunks ys

stepFunction ::
     forall m. (MonadIO m)
  => SerialT m (Array Word8)
  -> BS.ByteString
  -> SerialT m (Array Word8)
stepFunction stream1 bs = do
  arr <- liftIO $ byteStringToArray bs
  let stream2 = pure arr
  stream1 <> stream2

fromByteString ::
     forall m. (MonadIO m)
  => ByteString
  -> SerialT m (Array Word8)
fromByteString bs = foldlChunks stepFunction mempty bs

