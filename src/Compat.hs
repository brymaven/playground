{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Compat where

import Prelude hiding (length)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr, unsafeCreate, memcpy)
import qualified Foundation as F
import Foundation.Array
import Foundation.Array.Internal (withPtr, fromForeignPtr)



-- | Convert a ByteString to a UArray Word8,
-- without re-allocating or copying anything
fromByteString :: ByteString -> UArray F.Word8
fromByteString = fromForeignPtr . toForeignPtr

-- toByteString v = unsafeCreate len $ \dst -> withPtr v $ \src -> memcpy dst src len
--   where !len = F.length v

-- | Convert a UArray Word8 to ByteString
--
-- all the bytes are copied to a brand new memory chunk
toByteString :: UArray F.Word8 -> ByteString
toByteString v = unsafeCreate len $ \dst -> withPtr v $ \src -> memcpy dst src len
  where !(F.CountOf len) = F.length v
