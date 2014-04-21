{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.Internal
    ( c_sendmsg
    , c_recvmsg
    , c_cmsg_firsthdr
    , c_cmsg_nexthdr
    , c_cmsg_data
    ) where

import Network.Socket.Msg.CMsg (CMsgHdr)
import Network.Socket.Msg.MsgHdr (MsgHdr)

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize(..))

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall unsafe "recvmsg"
  c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall unsafe "cmsg_firsthdr"
  c_cmsg_firsthdr :: Ptr MsgHdr -> Ptr CMsgHdr

foreign import ccall unsafe "cmsg_nexthdr"
  c_cmsg_nexthdr :: Ptr MsgHdr -> Ptr CMsgHdr -> Ptr CMsgHdr

foreign import ccall unsafe "cmsg_data"
  c_cmsg_data :: Ptr CMsgHdr -> Ptr ()
