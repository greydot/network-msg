module Network.Socket.Msg (CMsg(..),recvMsg) where

import Network.Socket.Msg.CMsg
import Network.Socket.Msg.Internal
import Network.Socket.Msg.IOVec
import Network.Socket.Msg.MsgHdr

import Control.Applicative
import qualified Data.ByteString as B
import Data.Maybe (isNothing, fromJust)
import Network.Socket
import Network.Socket.Internal (peekSockAddr,throwSocketErrorWaitRead)
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Storable (Storable(..),poke)
import Foreign.Ptr (Ptr,plusPtr,nullPtr)

isNullPtr :: Ptr a -> Bool
isNullPtr = (==) nullPtr

---- The buffer will be filled as follows:
-- -------------
-- | Data (sz) |
-- -------------
-- |  SockAddr |
-- -------------
-- |   IOVec   |
-- -------------
-- |   MsgHdr  |
-- -------------
-- |  Aux Data |
-- -------------
recvMsg :: Socket -> Int -> IO (B.ByteString, SockAddr, [CMsg])
recvMsg sock@(MkSocket sockfd _ _ _ _) sz = allocaBytes bufSz $ \bufPtr -> do
        let addrPtr = plusPtr bufPtr addrSz
        let iovPtr = plusPtr addrPtr sz
        let mhdrPtr = plusPtr iovPtr (sizeOf (undefined :: IOVec))
        let auxPtr = plusPtr mhdrPtr (sizeOf (undefined :: MsgHdr))

        let iov = IOVec bufPtr (fromIntegral sz)
        let msghdr = MsgHdr { msgName = addrPtr 
                            , msgNameLen = fromIntegral addrSz
                            , msgIov = iovPtr
                            , msgIovLen = 1
                            , msgControl = auxPtr
                            , msgControlLen = fromIntegral auxSz
                            , msgFlags = 0 }
        poke iovPtr iov
        poke mhdrPtr msghdr

        rsz <- fmap fromIntegral (
# if !defined(__HUGS__) 
            throwSocketErrorWaitRead sock "recvMsg" $
# endif
            c_recvmsg sockfd mhdrPtr 0)
        (,,) <$> B.packCStringLen (bufPtr,rsz)
             <*> peekSockAddr addrPtr
             <*> extractCMsgs msghdr
    where
        -- Assume that 1024 bytes is enough for auxillary data
        auxSz = 1024
        -- Reserve 16 bytes for peer address
        addrSz = 16
        bufSz = sum [sz, auxSz, addrSz, sizeOf (undefined :: MsgHdr), sizeOf (undefined :: IOVec)]

extractCMsgs :: MsgHdr -> IO [CMsg]
extractCMsgs msg = alloca $ \pMsg ->
                   poke pMsg msg >> extractCMsgs' pMsg (c_cmsg_firsthdr pMsg) []
    where
        extractCMsgs' pMsg pCMsg resList
            | isNullPtr pCMsg = return resList
            | otherwise = do
                cmsg <- peekCMsg pCMsg
                extractCMsgs' pMsg
                              (c_cmsg_nexthdr pMsg pCMsg)
                              (if isNothing cmsg
                                  then resList
                                  else (fromJust cmsg):resList)
