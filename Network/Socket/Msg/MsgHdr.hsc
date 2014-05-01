{-# LANGUAGE CPP #-}

-- | Support module for the POSIX 'sendmsg,recvmsg' system calls.
-- This file was copied from network-bytestring package.
-- Original source: https://github.com/tibbe/network-bytestring/blob/master/Network/Socket/ByteString/MsgHdr.hsc

module Network.Socket.Msg.MsgHdr
  ( MsgHdr(..)
  ) where

#include <sys/types.h>
#include <sys/socket.h>

import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Network.Socket (SockAddr)

import Network.Socket.Msg.IOVec (IOVec)

data MsgHdr = MsgHdr
    { msgName       :: !(Ptr SockAddr)
    , msgNameLen    :: !CSize
    , msgIov        :: !(Ptr IOVec)
    , msgIovLen     :: !CSize
    , msgControl    :: !(Ptr ())
    , msgControlLen :: !CSize
    , msgFlags      :: !CInt
    }

instance Storable MsgHdr where
  sizeOf _ = (#const sizeof(struct msghdr))
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    name <- (#peek struct msghdr, msg_name) p
    nameLen <- (#peek struct msghdr, msg_namelen) p
    iov <- (#peek struct msghdr, msg_iov) p
    iovLen <- (#peek struct msghdr, msg_iovlen) p
    control <- (#peek struct msghdr, msg_control) p
    controlLen <- (#peek struct msghdr, msg_controllen) p
    flags <- (#peek struct msghdr, msg_flags) p
    return $! MsgHdr name nameLen iov iovLen control controlLen flags

  poke p mh = do
    (#poke struct msghdr, msg_name) p (msgName mh)
    (#poke struct msghdr, msg_namelen) p (msgNameLen mh)
    (#poke struct msghdr, msg_iov) p (msgIov mh)
    (#poke struct msghdr, msg_iovlen) p (msgIovLen mh)
    (#poke struct msghdr, msg_control) p (msgControl mh)
    (#poke struct msghdr, msg_controllen) p (msgControlLen mh)
    (#poke struct msghdr, msg_flags) p (msgFlags mh)
