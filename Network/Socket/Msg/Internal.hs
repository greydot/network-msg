{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.Internal
    ( c_sendmsg
    , c_recvmsg
    , zeroBytes
    ) where

import Network.Socket.Msg.MsgHdr (MsgHdr)

import Control.Monad (void)
import Foreign.C.Types (CInt(..),CSize(..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize(..))

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall unsafe "recvmsg"
  c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize

foreign import ccall unsafe "memset"
  c_memset :: Ptr a -> CInt -> CSize -> IO CSize

zeroBytes :: Ptr a -> Int -> IO ()
zeroBytes p s = void $ c_memset p 0 $ fromIntegral s
