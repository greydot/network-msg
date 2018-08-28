{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.Internal
    ( c_sendmsg
    , c_recvmsg
    , c_sendmmsg
    , c_recvmmsg
    ) where

import Network.Socket.Msg.MsgHdr (MsgHdr,MMsgHdr)

import Foreign.C.Types (CInt(..),CUInt(..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize(..))

foreign import ccall "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall "recvmsg"
  c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall "sendmmsg"
  c_sendmmsg :: CInt -> Ptr MMsgHdr -> CUInt -> CInt -> IO CInt

foreign import ccall "recvmmsg"
  c_recvmmsg :: CInt -> Ptr MMsgHdr -> CUInt -> CInt -> Ptr () -> IO CInt
