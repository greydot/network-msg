module Network.Socket.Msg (CMsg(..),sendMsg,recvMsg) where

import Network.Socket.Msg.CMsg
import Network.Socket.Msg.Internal
import Network.Socket.Msg.IOVec
import Network.Socket.Msg.MsgHdr

import Control.Applicative
import qualified Data.ByteString as B
import Data.Maybe (isNothing, fromJust)
import Network.Socket
import Network.Socket.Internal (peekSockAddr,pokeSockAddr,sizeOfSockAddr,throwSocketErrorWaitRead,throwSocketErrorWaitWrite)
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Storable (Storable(..),poke)
import Foreign.Ptr (Ptr,plusPtr,nullPtr)

isNullPtr :: Ptr a -> Bool
isNullPtr = (==) nullPtr

-- The buffer in both functions is filled as follows:
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

sendMsg :: Socket -> B.ByteString -> SockAddr -> [CMsg] -> IO ()
sendMsg sock@(MkSocket sockfd _ _ _ _) bytes sa cmsgs = allocaBytes bufSz $ \bufPtr -> do
        let saPtr = plusPtr bufPtr $ B.length bytes
        let iovPtr = plusPtr saPtr $ sizeOfSockAddr sa
        let msgPtr = plusPtr iovPtr $ sizeOf (undefined :: IOVec)
        let auxPtr = plusPtr msgPtr $ sizeOf (undefined :: MsgHdr)

        let iovec = IOVec bufPtr (fromIntegral $ B.length bytes)
        let msghdr = MsgHdr { msgName = saPtr
                            , msgNameLen = fromIntegral $ sizeOfSockAddr sa
                            , msgIov = iovPtr
                            , msgIovLen = 1
                            , msgControl = auxPtr
                            , msgControlLen = fromIntegral auxSz
                            , msgFlags = 0 }
        pokeCMsgs auxPtr cmsgs
        pokeSockAddr saPtr sa
        poke msgPtr msghdr
        poke iovPtr iovec
        
        _ <-
# if !defined(__HUGS__) 
            throwSocketErrorWaitWrite sock "sendMsg" $
# endif
                c_sendmsg sockfd msgPtr 0
        return ()
    where
        auxSz = sum $ map cmsgSpace cmsgs
        bufSz = sum [auxSz, sizeOfSockAddr sa, B.length bytes, sizeOf (undefined :: MsgHdr), sizeOf (undefined :: IOVec)]
        pokeCMsgs :: Ptr CMsgHdr -> [CMsg] -> IO ()
        pokeCMsgs _ [] = return ()
        pokeCMsgs ptr (c:cs) = pokeCMsg ptr c >> pokeCMsgs (plusPtr ptr $ cmsgSpace c) cs

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
