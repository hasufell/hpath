{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.ByteString where

import Control.Monad.IO.Class
import Data.ByteString hiding (length)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Prelude hiding (length)
import Streamly
import Streamly.Internal.Memory.Array.Types
import Streamly.Memory.Array
import qualified Streamly.Prelude as S

toByteString ::
     forall m. (MonadIO m, MonadAsync m)
  => SerialT m (Array Word8)
  -> m ByteString
toByteString stream =
  let xs = S.mapM arrayToByteString stream
      ys = S.foldlM' (\a b -> pure $ a <> b) mempty xs
   in ys

arrayToByteString :: (MonadIO m) => Array Word8 -> m ByteString
arrayToByteString arr
  | length arr == 0 = return mempty
arrayToByteString Array {..} =
  liftIO $
  withForeignPtr aStart $ \ptr ->
    unsafePackCStringFinalizer ptr aLen (return ())
  where
    aLen =
      let p = unsafeForeignPtrToPtr aStart
       in aEnd `minusPtr` p

byteStringToArray :: (MonadIO m) => ByteString -> m (Array Word8)
byteStringToArray bs =
  liftIO $
  unsafeUseAsCStringLen
    bs
    (\(ptr, _) -> do
       let endPtr pr = (castPtr pr `plusPtr` (BS.length bs))
       fptr <- newForeignPtr_ (castPtr ptr)
       return $ Array {aStart = fptr, aEnd = endPtr ptr, aBound = endPtr ptr})

fromByteString ::
     forall m. (MonadIO m)
  => ByteString
  -> m (Array Word8)
fromByteString bs = byteStringToArray bs
