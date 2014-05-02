-- This file was copied from network-bytestring package.
-- Original source: https://github.com/tibbe/network-bytestring/blob/master/Network/Socket/ByteString/IOVec.hsc
module Network.Socket.Msg.IOVec
  ( IOVec(..)
  ) where

import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

#include <sys/uio.h>

data IOVec = IOVec
    { iovBase :: !(Ptr CChar)
    , iovLen :: !CSize
    } deriving (Show)

instance Storable IOVec where
  sizeOf _ = (#const sizeof(struct iovec))
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- (#peek struct iovec, iov_base) p
    len <- (#peek struct iovec, iov_len) p
    return $! IOVec base len

  poke p iov = do
    (#poke struct iovec, iov_base) p (iovBase iov)
    (#poke struct iovec, iov_len) p (iovLen iov)
