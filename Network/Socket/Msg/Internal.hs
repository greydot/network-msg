{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.Internal
    ( c_sendmsg
    , c_recvmsg
    ) where

import Network.Socket.Msg.MsgHdr (MsgHdr)

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize(..))

foreign import ccall "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall "recvmsg"
  c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
